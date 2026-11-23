import cats.effect.*
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import fs2.Stream
import org.typelevel.log4cats.slf4j.Slf4jLogger
import pureconfig.*
import softphone4s.config.PureConfigReaders.given
import softphone4s.*
import softphone4s.Publication.updatePresence
import softphone4s.config.SoftphoneConfig
import softphone4s.dialog.DialogInfo
import softphone4s.messaging.MessageContent
import softphone4s.model.{Callee, DtmfDigit, FailureStatus}
import softphone4s.presence.{Activity, Availability, Presence, PresenceTuple}
import softphone4s.registration.RegistrationState
import softphone4s.subscription.{EventPackage, MessageSummary}
import sound4s.{AudioFile, ResourceName, SoundStreaming}

import java.awt.{List as _, *}
import java.awt.event.*
import java.time.LocalTime
import javax.sound.sampled.AudioFormat as JavaAudioFormat
import sound4s.AudioFormat
import scala.concurrent.duration.*

/** A softphone with an AWT window, using what the library offers: registration,
  * outgoing and incoming calls, hold, DTMF, instant messages, presence
  * (published, and watched in others), message waiting and dialog state.
  *
  * It reads the account from `demo.softphone` (see `application.conf`) and
  * takes the number to call from the window. Run it with
  * `sbt "softphone4sDemo/runMain SoftphoneDemo"`; it needs a display, a
  * microphone and a speaker.
  *
  * What it does not do: more than one call at a time (a second is answered
  * `486 Busy Here`), mute, or any of the account's secrets in the window.
  */
object SoftphoneDemo extends IOApp.Simple {

  ScribeConfig.configure()

  /** The format as the JDK describes it, converted to sound4s's. */
  private val audioFormat: IO[AudioFormat] =
    IO.fromEither(
      AudioFormat
        .fromJavaFormat(JavaAudioFormat(8000f, 16, 1, true, false))
        .left
        .map(error => new IllegalArgumentException(error.toString))
    )

  /** What we say about ourselves. */
  private enum Mood(
      val label: String,
      val availability: Availability,
      val activity: Option[Activity]
  ) {
    case Available extends Mood("Available", Availability.Open, None)
    case Away      extends Mood("Away", Availability.Open, Some(Activity.Away))
    case Busy      extends Mood("Busy", Availability.Open, Some(Activity.Busy))
    case OnThePhone
        extends Mood(
          "On the phone",
          Availability.Open,
          Some(Activity.OnThePhone)
        )
    case Offline extends Mood("Offline", Availability.Closed, None)
  }

  private def presenceOf(account: softphone4s.model.AccountId, mood: Mood) =
    Presence
      .forAccount(
        account,
        List(
          PresenceTuple("softphone4s", Some(mood.availability), None, Nil)
        ),
        List(mood.label)
      )
      .copy(activities = mood.activity.toList)

  /** What the line is doing, and so what the buttons can do. */
  private enum Line {
    case Idle
    case Claimed
    case Ringing(decide: Decision => IO[Unit])
    case InCall(call: Call[IO], hangup: IO[Unit])
  }

  private enum Decision { case Answer, Decline }

  private enum Hold { case Off, On }

  /** Which buttons are on. */
  private enum Mode { case Idle, Ringing, Active }

  private enum Kind(val label: String) {
    case PresenceOf     extends Kind("Presence")
    case MessageWaiting extends Kind("Message waiting")
    case Dialogs        extends Kind("Dialogs")
  }

  private def message(e: Throwable): String =
    Option(e.getMessage).getOrElse(e.toString)

  private def clock(elapsed: FiniteDuration): String = {
    val s = elapsed.toSeconds
    f"${s / 60}%02d:${s % 60}%02d"
  }

  // ---------------------------------------------------------------- window

  private final class Ui(account: String) {

    val frame = new Frame(s"softphone4s: $account")

    val registrationLabel = new Label("Unregistered", Label.CENTER)
    val moodChoice        = new Choice

    val numberField                       = new TextField(22)
    val callButton                        = new Button("Call")
    val answerButton                      = new Button("Answer")
    val hangupButton                      = new Button("Hang up")
    val holdButton                        = new Button("Hold")
    val callLabel                         = new Label("No call", Label.CENTER)
    val timerLabel                        = new Label("", Label.CENTER)
    val dtmfLabel                         = new Label("", Label.CENTER)
    val keypad: List[(DtmfDigit, Button)] =
      DtmfDigit.values.toList.map(d => d -> new Button(d.symbol.toString))

    val toField    = new TextField(24)
    val textField  = new TextField(40)
    val sendButton = new Button("Send")
    val transcript = new TextArea("", 12, 50, TextArea.SCROLLBARS_VERTICAL_ONLY)
    val watchField = new TextField(24)
    val watchChoice   = new Choice
    val watchButton   = new Button("Watch")
    val unwatchButton = new Button("Stop watching")
    val watchOutput   =
      new TextArea("", 12, 50, TextArea.SCROLLBARS_VERTICAL_ONLY)
    val logArea = new TextArea("", 12, 50, TextArea.SCROLLBARS_VERTICAL_ONLY)

    private val cards   = new CardLayout
    private val cardBox = new Panel(cards)

    Mood.values.foreach(m => moodChoice.add(m.label))
    Kind.values.foreach(k => watchChoice.add(k.label))
    List(transcript, watchOutput, logArea).foreach(_.setEditable(false))
    registrationLabel.setBackground(Color.LIGHT_GRAY)
    callLabel.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 16))
    timerLabel.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 18))
    numberField.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 16))

    private def row(parts: Component*): Panel = {
      val p = new Panel(new FlowLayout(FlowLayout.LEFT, 6, 4))
      parts.foreach(p.add)
      p
    }

    private def card(name: String, body: Panel): Unit =
      cardBox.add(body, name)

    locally {
      val top = row(
        new Label("Presence"),
        moodChoice,
        new Label("Registration"),
        registrationLabel
      )

      val keys = new Panel(new GridLayout(4, 4, 4, 4))
      keypad.foreach((_, b) => keys.add(b))
      val status = new Panel(new GridLayout(3, 1))
      List(callLabel, timerLabel, dtmfLabel).foreach(status.add)
      val dial = new Panel(new BorderLayout(4, 8))
      dial.add(
        row(numberField, callButton, answerButton, hangupButton, holdButton),
        BorderLayout.NORTH
      )
      dial.add(keys, BorderLayout.CENTER)
      dial.add(status, BorderLayout.SOUTH)

      val chat = new Panel(new BorderLayout(4, 4))
      chat.add(row(new Label("To"), toField), BorderLayout.NORTH)
      chat.add(transcript, BorderLayout.CENTER)
      chat.add(row(textField, sendButton), BorderLayout.SOUTH)
      card("Messages", chat)

      val watch = new Panel(new BorderLayout(4, 4))
      watch.add(
        row(watchField, watchChoice, watchButton, unwatchButton),
        BorderLayout.NORTH
      )
      watch.add(watchOutput, BorderLayout.CENTER)
      card("Watch", watch)

      val logPanel = new Panel(new BorderLayout)
      logPanel.add(logArea, BorderLayout.CENTER)
      card("Log", logPanel)

      val tabs = new Panel(new FlowLayout(FlowLayout.LEFT))
      List("Messages", "Watch", "Log").foreach { name =>
        val b = new Button(name)
        b.addActionListener((_: ActionEvent) => cards.show(cardBox, name))
        tabs.add(b)
      }
      val right = new Panel(new BorderLayout)
      right.add(tabs, BorderLayout.NORTH)
      right.add(cardBox, BorderLayout.CENTER)

      frame.setLayout(new BorderLayout(8, 8))
      frame.add(top, BorderLayout.NORTH)
      frame.add(dial, BorderLayout.WEST)
      frame.add(right, BorderLayout.CENTER)
      frame.setSize(980, 560)
      mode(Mode.Idle)
    }

    def onClose(f: => Unit): Unit =
      frame.addWindowListener(new WindowAdapter {
        override def windowClosing(e: WindowEvent): Unit = {
          frame.setVisible(false)
          f
        }
      })

    def show: IO[Unit] = IO(
      EventQueue.invokeLater(() => frame.setVisible(true))
    )

    def onUi(f: => Unit): IO[Unit] = IO(EventQueue.invokeLater(() => f))

    private def setText(label: Label, text: String): Unit = {
      label.setText(text)
      frame.validate()
    }

    private def stamp: String = LocalTime.now.withNano(0).toString

    def log(line: String): IO[Unit] =
      onUi(logArea.append(s"$stamp  $line\n"))

    def say(area: TextArea, line: String): IO[Unit] =
      onUi(area.append(s"$stamp  $line\n"))

    def callState(text: String): IO[Unit] =
      onUi(setText(callLabel, text)) >> log(text)

    def timer(text: String): IO[Unit] = onUi(setText(timerLabel, text))

    def dtmf(text: String): IO[Unit] = onUi(setText(dtmfLabel, text))

    def holdText(text: String): IO[Unit] = onUi(holdButton.setLabel(text))

    def select(mood: Mood): IO[Unit] = onUi(moodChoice.select(mood.ordinal))

    def reply(to: String): IO[Unit] = onUi(toField.setText(to))

    def clearText: IO[Unit] = onUi(textField.setText(""))

    def read(field: TextField): IO[String] = IO(field.getText.trim)

    def registration(state: RegistrationState): IO[Unit] = {
      val (text, color) = state match {
        case RegistrationState.Unregistered =>
          ("Unregistered", Color.LIGHT_GRAY)
        case RegistrationState.Registering => ("Registering...", Color.YELLOW)
        case RegistrationState.Registered(lifetime) =>
          (s"Registered (${lifetime.toSeconds} s)", Color.GREEN)
        case RegistrationState.Failed(failure, retryIn) =>
          (s"Failed: $failure, retry in ${retryIn.toSeconds} s", Color.PINK)
        case RegistrationState.Unregistering =>
          ("Unregistering...", Color.YELLOW)
      }
      onUi {
        setText(registrationLabel, text)
        registrationLabel.setBackground(color)
      } >> log(s"Registration: $text")
    }

    def mode(m: Mode): IO[Unit] = onUi(applyMode(m))

    private def applyMode(m: Mode): Unit = {
      callButton.setEnabled(m == Mode.Idle)
      numberField.setEnabled(m == Mode.Idle)
      answerButton.setEnabled(m == Mode.Ringing)
      hangupButton.setEnabled(m != Mode.Idle)
      holdButton.setEnabled(m == Mode.Active)
      keypad.foreach((_, b) => b.setEnabled(m == Mode.Active))
    }
  }

  // ------------------------------------------------------------ behaviour

  private final class Phone(
      ui: Ui,
      softphone: Softphone[IO],
      publication: Publication[IO],
      sound: SoundStreaming[IO],
      format: AudioFormat,
      line: Ref[IO, Line],
      hold: Ref[IO, Hold],
      mood: Ref[IO, Mood],
      watches: Ref[IO, Map[String, IO[Unit]]]
  ) {

    private val account = softphone.accounts.head

    /** Cancels what is still running before the softphone is closed. */
    val shutdown: IO[Unit] =
      watches.get.flatMap(_.values.toList.sequence_)

    /** Plays `file` in a loop until it is cancelled. */
    private def loop(file: String): IO[Unit] =
      Stream
        .eval(
          IO.fromEither(
            ResourceName(file).left.map(new IllegalArgumentException(_))
          )
        )
        .flatMap(AudioFile[IO](_, format).source)
        .repeat
        .through(sound.sink)
        .compile
        .drain

    /** Heard while the other side rings, on a call we place. */
    private val ringback: IO[Unit] = loop("progress.wav")

    /** Heard while someone calls us. */
    private val ringtone: IO[Unit] = loop("ringtone.wav")

    // ---- presence

    def setMood(m: Mood): IO[Unit] =
      mood.set(m) >> ui.select(m) >>
        publication
          .updatePresence(presenceOf(account, m))
          .flatMap(_.fold(e => ui.log(s"Presence not published: $e"), _.pure))
          .handleErrorWith(e => ui.log(s"Presence failed: ${message(e)}"))

    /** Says "on the phone" for as long as `io` runs. */
    private def onThePhone(io: IO[Unit]): IO[Unit] =
      mood.get.flatMap(before =>
        setMood(Mood.OnThePhone) >> io.guarantee(setMood(before))
      )

    // ---- calls

    private def claim: IO[Boolean] =
      line.modify {
        case Line.Idle => (Line.Claimed, true)
        case other     => (other, false)
      }

    private def finish(io: IO[Unit]): IO[Unit] =
      io.handleErrorWith(e => ui.callState(s"Call failed: ${message(e)}"))
        .guarantee(
          line.set(Line.Idle) >> hold.set(Hold.Off) >> ui.holdText("Hold") >>
            ui.mode(Mode.Idle) >> ui.timer("")
        )

    private def media(call: Call[IO]): IO[Unit] =
      sound.source
        .through(call.sink)
        .concurrently(call.source.through(sound.sink))
        .concurrently(
          call.dtmfEvents.evalMap(d => ui.dtmf(s"Received ${d.symbol}"))
        )
        .concurrently(
          Stream
            .awakeEvery[IO](1.second)
            .evalMap(t => ui.timer(clock(t)))
        )
        .compile
        .drain

    private def talk(call: Call[IO], who: String, ringing: IO[Unit]): IO[Unit] =
      for {
        ended <- Deferred[IO, Unit]
        _     <- line.set(Line.InCall(call, ended.complete(()).void))
        _     <- ui.mode(Mode.Active)
        _     <- ui.callState(s"Calling $who")
        connected = call.awaitPickup.race(ringing) >>
          ui.callState(s"Connected to $who") >> media(call)
        _ <- onThePhone(
          connected.race(ended.get.race(call.awaitHangup)).void
        )
      } yield ()

    private def session(
        acquire: Resource[IO, Call[IO]],
        who: String,
        ringing: IO[Unit]
    ): IO[Unit] =
      acquire.use(talk(_, who, ringing)) >> ui.callState("Call ended")

    val dial: IO[Unit] =
      ui.read(ui.numberField).flatMap { text =>
        Callee(text) match {
          case Left(error)   => ui.callState(error)
          case Right(callee) =>
            claim.flatMap(
              if _ then
                finish(session(softphone.call(callee), callee.value, ringback))
              else IO.unit
            )
        }
      }

    def onIncoming(call: IncomingCall[IO]): IO[Unit] =
      claim.flatMap {
        case false => call.reject(FailureStatus.BusyHere)
        case true  =>
          val who = call.caller.displayName.fold("")(_ + " ") +
            call.caller.uri.render
          finish(for {
            decision <- Deferred[IO, Decision]
            _        <- line.set(Line.Ringing(d => decision.complete(d).void))
            _        <- ui.mode(Mode.Ringing)
            _        <- ui.callState(s"Incoming call from $who")
            _        <- call.ringing
            outcome  <- ringtone.background.use(_ =>
              decision.get.race(call.cancelled)
            )
            _ <- outcome match {
              case Right(())              => ui.callState(s"$who gave up")
              case Left(Decision.Decline) =>
                call.reject() >> ui.callState("Call declined")
              case Left(Decision.Answer) =>
                session(call.accept, who, IO.never)
            }
          } yield ())
      }

    val answer: IO[Unit] = line.get.flatMap {
      case Line.Ringing(decide) => decide(Decision.Answer)
      case _                    => IO.unit
    }

    val hangup: IO[Unit] = line.get.flatMap {
      case Line.Ringing(decide) => decide(Decision.Decline)
      case Line.InCall(_, end)  => end
      case _                    => IO.unit
    }

    val toggleHold: IO[Unit] = line.get.flatMap {
      case Line.InCall(call, _) =>
        hold.get.flatMap {
          case Hold.Off =>
            call.hold >> hold.set(Hold.On) >> ui.holdText("Resume") >>
              ui.callState("On hold")
          case Hold.On =>
            call.resume >> hold.set(Hold.Off) >> ui.holdText("Hold") >>
              ui.callState("Resumed")
        }
      case _ => IO.unit
    }

    def sendDigit(digit: DtmfDigit): IO[Unit] = line.get.flatMap {
      case Line.InCall(call, _) =>
        call
          .sendDtmf(digit)
          .flatMap(how => ui.dtmf(s"Sent ${digit.symbol} ($how)"))
      case _ => IO.unit
    }

    // ---- messages

    val sendMessage: IO[Unit] =
      for {
        to   <- ui.read(ui.toField)
        text <- ui.read(ui.textField)
        _    <- Callee(to) match {
          case Left(error)   => ui.say(ui.transcript, error)
          case Right(callee) =>
            softphone.messages
              .send(callee, MessageContent.plain(text))
              .flatMap {
                case Right(()) =>
                  ui.say(ui.transcript, s"me to ${callee.value}: $text") >>
                    ui.clearText
                case Left(failure) =>
                  ui.say(ui.transcript, s"not sent: ${failure.message}")
              }
        }
      } yield ()

    val receiveMessages: IO[Unit] =
      softphone.messages.incoming
        .evalMap { m =>
          val from = m.from.uri.render
          ui.say(ui.transcript, s"$from: ${m.content.text}") >> ui.reply(from)
        }
        .compile
        .drain

    val receiveCalls: IO[Unit] =
      softphone.incoming
        .parEvalMapUnordered(8)(onIncoming)
        .compile
        .drain

    // ---- watching others

    private def watching[A](
        target: Callee,
        pkg: EventPackage[A],
        name: String,
        show: A => String
    ): IO[Unit] =
      softphone.subscriptions.watch(target, pkg).use { watch =>
        watch.states
          .evalMap(state =>
            ui.say(
              ui.watchOutput,
              s"$name: ${state.fold(e => s"unreadable ($e)", show)}"
            )
          )
          .concurrently(
            watch.subscription.status.discrete
              .evalMap(s => ui.say(ui.watchOutput, s"$name: subscription $s"))
          )
          .compile
          .drain
      }

    private def showPresence(p: Presence): String = {
      val basic = p.tuples.flatMap(_.basic).map(_.toString.toLowerCase)
      val doing =
        p.activities.map(a => Activity.element(a).getOrElse(a.toString))
      (basic ++ doing ++ p.notes).mkString(", ")
    }

    private def showSummary(m: MessageSummary): String =
      s"waiting: ${m.waiting}; " + m.counts
        .map((c, n) => s"${c.field} ${n.newMessages} new, ${n.oldMessages} old")
        .mkString("; ")

    private def showDialogs(info: DialogInfo): String =
      if info.dialogs.isEmpty then "no dialogs"
      else
        info.dialogs
          .map(d =>
            s"${d.state.toString.toLowerCase} " +
              d.remote.flatMap(_.identity).getOrElse("")
          )
          .mkString("; ")

    private def kindAt(index: Int): Kind = Kind.fromOrdinal(index)

    private def key(kind: Kind, target: Callee): String =
      s"${kind.label} ${target.value}"

    val watch: IO[Unit] =
      for {
        text  <- ui.read(ui.watchField)
        index <- IO(ui.watchChoice.getSelectedIndex)
        _     <- Callee(text) match {
          case Left(error)   => ui.say(ui.watchOutput, error)
          case Right(target) =>
            val kind = kindAt(index)
            val name = key(kind, target)
            val work = kind match {
              case Kind.PresenceOf =>
                watching(target, Presence.Package, name, showPresence)
              case Kind.MessageWaiting =>
                watching(target, MessageSummary.Package, name, showSummary)
              case Kind.Dialogs =>
                watching(target, DialogInfo.Package, name, showDialogs)
            }
            watches.get.flatMap { running =>
              if running.contains(name) then
                ui.say(ui.watchOutput, s"already watching $name")
              else
                work
                  .handleErrorWith(e =>
                    ui.say(ui.watchOutput, s"$name: ${message(e)}")
                  )
                  .guarantee(watches.update(_ - name))
                  .start
                  .flatMap(fiber =>
                    watches.update(_ + (name -> fiber.cancel)) >>
                      ui.say(ui.watchOutput, s"watching $name")
                  )
            }
        }
      } yield ()

    val unwatch: IO[Unit] =
      for {
        text  <- ui.read(ui.watchField)
        index <- IO(ui.watchChoice.getSelectedIndex)
        _     <- Callee(text) match {
          case Left(error)   => ui.say(ui.watchOutput, error)
          case Right(target) =>
            val name = key(kindAt(index), target)
            watches.get.flatMap(
              _.get(name).fold(ui.say(ui.watchOutput, s"not watching $name"))(
                stop => stop >> ui.say(ui.watchOutput, s"stopped $name")
              )
            )
        }
      } yield ()
  }

  // ------------------------------------------------------------------ app

  private def loadConfig: IO[SoftphoneConfig] =
    IO.fromEither(
      ConfigSource.default
        .at("demo.softphone")
        .load[SoftphoneConfig]
        .left
        .map(f => new IllegalArgumentException(f.prettyPrint()))
    )

  private def wire(ui: Ui, phone: Phone, done: Deferred[IO, Unit])(using
      dispatcher: Dispatcher[IO]
  ): IO[Unit] = IO {
    def run(io: IO[Unit]): Unit =
      dispatcher.unsafeRunAndForget(
        io.handleErrorWith(e => ui.log(s"Error: ${message(e)}"))
      )
    def on(button: Button)(io: => IO[Unit]): Unit =
      button.addActionListener((_: ActionEvent) => run(io))

    on(ui.callButton)(phone.dial)
    ui.numberField.addActionListener((_: ActionEvent) => run(phone.dial))
    on(ui.answerButton)(phone.answer)
    on(ui.hangupButton)(phone.hangup)
    on(ui.holdButton)(phone.toggleHold)
    ui.keypad.foreach((digit, button) => on(button)(phone.sendDigit(digit)))
    on(ui.sendButton)(phone.sendMessage)
    ui.textField.addActionListener((_: ActionEvent) => run(phone.sendMessage))
    on(ui.watchButton)(phone.watch)
    on(ui.unwatchButton)(phone.unwatch)
    ui.moodChoice.addItemListener((_: ItemEvent) =>
      run(phone.setMood(Mood.fromOrdinal(ui.moodChoice.getSelectedIndex)))
    )
    ui.onClose(run(done.complete(()).void))
  }

  override def run: IO[Unit] =
    (for {
      config    <- Resource.eval(loadConfig)
      logger    <- Resource.eval(Slf4jLogger.create[IO])
      format    <- Resource.eval(audioFormat)
      sound     <- SoundStreaming.resource[IO](format, logger)
      softphone <- SoftphoneStreaming.resource(config, format, logger)
      account = softphone.accounts.head
      ui <- Resource.make(IO(new Ui(account.render)))(ui =>
        IO(ui.frame.dispose())
      )
      registration <- softphone.register
      publication  <- softphone.publications.publishPresence(
        presenceOf(account, Mood.Available)
      )
      line    <- Resource.eval(Ref.of[IO, Line](Line.Idle))
      hold    <- Resource.eval(Ref.of[IO, Hold](Hold.Off))
      mood    <- Resource.eval(Ref.of[IO, Mood](Mood.Available))
      watches <- Resource.eval(Ref.of[IO, Map[String, IO[Unit]]](Map.empty))
      phone = Phone(
        ui,
        softphone,
        publication,
        sound,
        format,
        line,
        hold,
        mood,
        watches
      )
      _ <- Resource.onFinalize(phone.shutdown)
      _ <- registration.state.discrete
        .evalTap(state => logger.info(s"Registration: $state"))
        .evalMap(ui.registration)
        .compile
        .drain
        .background
      _          <- phone.receiveCalls.background
      _          <- phone.receiveMessages.background
      done       <- Resource.eval(Deferred[IO, Unit])
      dispatcher <- Dispatcher.parallel[IO]
      _          <- Resource.eval(wire(ui, phone, done)(using dispatcher))
      _          <- Resource.eval(ui.show)
    } yield done).use(_.get)
}
