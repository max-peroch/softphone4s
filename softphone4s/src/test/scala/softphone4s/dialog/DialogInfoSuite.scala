package softphone4s.dialog

import munit.FunSuite
import softphone4s.model.MediaType
import softphone4s.subscription.{
  EventPackage,
  Notification,
  NotifyState,
  PackageError
}

import scala.concurrent.duration.*

class DialogInfoSuite extends FunSuite {

  private val ns = """xmlns="urn:ietf:params:xml:ns:dialog-info""""

  private def notification(body: String) =
    Notification(NotifyState.Active(None), Some(MediaType.DialogInfoXml), body)

  private def read(body: String) = DialogInfo.Package.decode(notification(body))

  private def malformed(body: String): Boolean =
    read(body).left.exists {
      case PackageError.Malformed(_) => true; case _ => false
    }

  /** A document of `state` and `version` about alice, with these dialogs. */
  private def document(
      dialogs: String,
      version: String = "1",
      state: String = "full"
  ) =
    s"""<?xml version="1.0"?>
       |<dialog-info $ns version="$version" state="$state" entity="sip:alice@example.com">
       |$dialogs
       |</dialog-info>""".stripMargin

  private def dialog(id: String, state: String, extra: String = "") =
    s"""<dialog id="$id" call-id="call-$id" local-tag="l$id" direction="initiator">
       |  <state>$state</state>$extra
       |</dialog>""".stripMargin

  private def entry(id: String, state: DialogState): DialogEntry =
    DialogEntry(
      id,
      Some(s"call-$id"),
      Some(s"l$id"),
      None,
      Some(DialogDirection.Initiator),
      state,
      None,
      None,
      None,
      None,
      None,
      None,
      None
    )

  private def info(
      version: Long,
      coverage: Coverage,
      dialogs: DialogEntry*
  ) = DialogInfo("sip:alice@example.com", version, coverage, dialogs.toList)

  test("a confirmed dialog, in the way RFC 4235 gives its example") {
    val body = document(
      """<dialog id="as7d900as8" call-id="a84b4c76e66710" local-tag="hh76a" direction="initiator">
        |  <state>confirmed</state>
        |  <local>
        |    <identity display="Alice">sip:alice@example.com</identity>
        |    <target uri="sip:alice@pc33.example.com"/>
        |  </local>
        |  <remote>
        |    <identity display="Bob">sip:bob@example.org</identity>
        |    <target uri="sip:bob@192.0.2.4"/>
        |  </remote>
        |</dialog>""".stripMargin
    )
    val expected = DialogEntry(
      "as7d900as8",
      Some("a84b4c76e66710"),
      Some("hh76a"),
      None,
      Some(DialogDirection.Initiator),
      DialogState.Confirmed,
      None,
      None,
      None,
      None,
      None,
      Some(
        Party(
          Some("sip:alice@example.com"),
          Some("Alice"),
          Some("sip:alice@pc33.example.com")
        )
      ),
      Some(
        Party(
          Some("sip:bob@example.org"),
          Some("Bob"),
          Some("sip:bob@192.0.2.4")
        )
      )
    )
    assertEquals(
      read(body),
      Right(
        DialogInfo("sip:alice@example.com", 1, Coverage.Full, List(expected))
      )
    )
  }

  test(
    "every state, how a dialog ended, its duration, what it replaces and who referred it"
  ) {
    val body = document(
      dialog("a", "trying") + dialog("b", "proceeding") + dialog("c", "early") +
        dialog(
          "d",
          "terminated",
          "<duration>95</duration>"
        ).replace(
          """<state>terminated</state>""",
          """<state event="remote-bye" code="487">terminated</state>"""
        ) +
        dialog(
          "e",
          "confirmed",
          """<replaces call-id="old" local-tag="x" remote-tag="y"/><referred-by display="Carol">sip:carol@example.com</referred-by>"""
        )
    )
    val read1 = read(body).fold(e => fail(e.message), identity)
    assertEquals(
      read1.dialogs.map(_.state),
      List(
        DialogState.Trying,
        DialogState.Proceeding,
        DialogState.Early,
        DialogState.Terminated,
        DialogState.Confirmed
      )
    )
    val d = read1.dialog("d").get
    assertEquals(d.event, Some(EndEvent.RemoteBye))
    assertEquals(d.code, Some(487))
    assertEquals(d.duration, Some(95.seconds))
    assert(d.isTerminated)
    val e = read1.dialog("e").get
    assertEquals(e.replaces, Some(Replaces("old", Some("x"), Some("y"))))
    assertEquals(e.referredBy, Some("sip:carol@example.com"))
    assertEquals(read1.ongoing.map(_.id), List("a", "b", "c", "e"))
    assert(read1.isBusy)
  }

  test("a document with no dialog says the entity has none") {
    val result = read(document(""))
    assertEquals(result, Right(info(1, Coverage.Full)))
    assert(!result.exists(_.isBusy))
  }

  test("partial documents are read as partial, with their version") {
    assertEquals(
      read(
        document(dialog("a", "confirmed"), version = "7", state = "partial")
      ),
      Right(info(7, Coverage.Partial, entry("a", DialogState.Confirmed)))
    )
  }

  test("tokens are read without regard to case or the spaces around them") {
    val body = document(
      """<dialog id="a" direction="RECIPIENT"><state> Confirmed </state></dialog>""",
      state = "FULL"
    )
    val got = read(body).fold(e => fail(e.message), identity)
    assertEquals(got.coverage, Coverage.Full)
    assertEquals(got.dialogs.head.direction, Some(DialogDirection.Recipient))
    assertEquals(got.dialogs.head.state, DialogState.Confirmed)
  }

  test(
    "a prefixed namespace is read, and extensions and unknown elements are skipped"
  ) {
    val body =
      """<d:dialog-info xmlns:d="urn:ietf:params:xml:ns:dialog-info" xmlns:x="urn:example:ext"
        |    version="2" state="full" entity="sip:alice@example.com">
        |  <x:mood>happy</x:mood>
        |  <d:dialog id="a">
        |    <d:state>early</d:state>
        |    <d:future-element>ignored</d:future-element>
        |    <x:dialog id="not-ours"><x:state>confirmed</x:state></x:dialog>
        |  </d:dialog>
        |</d:dialog-info>""".stripMargin
    val got = read(body).fold(e => fail(e.message), identity)
    assertEquals(
      got.dialogs.map(d => d.id -> d.state),
      List("a" -> DialogState.Early)
    )
  }

  test("an event that is not one of the known ones is left out, not an error") {
    val body = document(
      """<dialog id="a"><state event="something-new">terminated</state></dialog>"""
    )
    assertEquals(read(body).map(_.dialogs.head.event), Right(None))
  }

  test("a DOCTYPE is refused, and an entity it would declare is not expanded") {
    assert(
      malformed(
        s"""<!DOCTYPE dialog-info [<!ENTITY e "boom">]>
           |<dialog-info $ns version="1" state="full" entity="&e;"/>""".stripMargin
      )
    )
    assert(
      malformed(s"""<dialog-info $ns version="1" state="full" entity="&e;"/>""")
    )
  }

  test("the entities XML defines, and character references, are text") {
    val body =
      s"""<dialog-info $ns version="1" state="full" entity="sip:a&amp;b@example.com&#33;"/>"""
    assertEquals(read(body).map(_.entity), Right("sip:a&b@example.com!"))
  }

  test("what is not a dialog-info document of the right namespace is refused") {
    assert(
      malformed(
        """<presence xmlns="urn:ietf:params:xml:ns:pidf" entity="e"/>"""
      )
    )
    assert(malformed("""<dialog-info version="1" state="full" entity="e"/>"""))
    assert(
      malformed(
        """<dialog-info xmlns="urn:other" version="1" state="full" entity="e"/>"""
      )
    )
  }

  test("a document missing what the RFC requires is refused") {
    assert(malformed(s"""<dialog-info $ns version="1" state="full"/>"""))
    assert(malformed(s"""<dialog-info $ns state="full" entity="e"/>"""))
    assert(malformed(s"""<dialog-info $ns version="1" entity="e"/>"""))
    assert(malformed(document("""<dialog><state>early</state></dialog>""")))
    assert(malformed(document("""<dialog id="a"/>""")))
  }

  test("values the RFC does not allow are refused") {
    def bad(version: String = "1", state: String = "full", inner: String = "") =
      assert(
        malformed(document(inner, version, state)),
        s"$version $state $inner"
      )
    bad(version = "-1")
    bad(version = "one")
    bad(version = "99999999999999999999") // more than a Long
    bad(state = "some")
    bad(inner =
      """<dialog id="a" direction="sideways"><state>early</state></dialog>"""
    )
    bad(inner = """<dialog id="a"><state>ringing</state></dialog>""")
    bad(inner = """<dialog id="a"><state code="99">early</state></dialog>""")
    bad(inner = """<dialog id="a"><state code="700">early</state></dialog>""")
    bad(inner =
      """<dialog id="a"><state>early</state><duration>-5</duration></dialog>"""
    )
    bad(inner =
      """<dialog id="a"><state>early</state><duration>soon</duration></dialog>"""
    )
    bad(inner = """<dialog id="a"><state>early</state><replaces/></dialog>""")
  }

  test(
    "a duration too long to be a duration is refused, and no exception is thrown"
  ) {
    val body = document(
      """<dialog id="a"><state>confirmed</state><duration>9223372036854775807</duration></dialog>"""
    )
    assert(malformed(body))
    // the longest there can be is read
    val most = Long.MaxValue / 1_000_000_000L
    assertEquals(
      read(
        document(
          s"""<dialog id="a"><state>confirmed</state><duration>$most</duration></dialog>"""
        )
      ).map(_.dialogs.head.duration),
      Right(Some(most.seconds))
    )
  }

  test("text that is not XML is refused, and so is XML that is cut off") {
    assert(malformed("not xml at all"))
    assert(
      malformed(s"""<dialog-info $ns version="1" state="full" entity="e">""")
    )
    // an empty document is no document, and not the same as a NOTIFY with no body
    assert(
      DialogInfo.parse("").left.exists {
        case PackageError.Malformed(_) => true; case _ => false
      }
    )
  }

  test("no body, or another type, is said so") {
    assertEquals(
      DialogInfo.Package.decode(
        Notification(
          NotifyState.Active(None),
          Some(MediaType.DialogInfoXml),
          ""
        )
      ),
      Left(PackageError.NoBody)
    )
    assertEquals(
      DialogInfo.Package.decode(
        Notification(NotifyState.Active(None), Some(MediaType.PidfXml), "<x/>")
      ),
      Left(PackageError.UnexpectedType(Some(MediaType.PidfXml)))
    )
    assertEquals(
      DialogInfo.Package.decode(
        Notification(NotifyState.Active(None), None, "<x/>")
      ),
      Left(PackageError.UnexpectedType(None))
    )
  }

  test("a document over the limit is refused, and one at it is read") {
    val open  = s"""<dialog-info $ns version="1" state="full" entity="e"><x>"""
    val close = "</x></dialog-info>"
    def padded(length: Int) =
      open + "x" * (length - open.length - close.length) + close
    val most = EventPackage.MaxBodyLength
    assert(DialogInfo.parse(padded(most)).isRight)
    assertEquals(
      DialogInfo.parse(padded(most + 1)),
      Left(PackageError.TooLarge(most + 1, most))
    )
  }

  test("the package asks for dialog and takes dialog-info") {
    assertEquals(DialogInfo.Package.event.render, "dialog")
    assertEquals(DialogInfo.Package.accept, List(MediaType.DialogInfoXml))
    assertEquals(MediaType.DialogInfoXml.render, "application/dialog-info+xml")
  }

  // ---- Keeping track ------------------------------------------------------------

  test("a full update is the state, and what has ended is not kept") {
    val first = info(
      1,
      Coverage.Full,
      entry("a", DialogState.Confirmed),
      entry("b", DialogState.Terminated)
    )
    assertEquals(
      DialogInfo.track(None, first),
      Right(info(1, Coverage.Full, entry("a", DialogState.Confirmed)))
    )
    val replacement = info(5, Coverage.Full, entry("c", DialogState.Early))
    assertEquals(
      DialogInfo.track(Some(first), replacement),
      Right(replacement)
    )
  }

  test(
    "a partial update changes the dialogs it names: replaces, adds, and drops one that ended"
  ) {
    val held = info(
      3,
      Coverage.Full,
      entry("a", DialogState.Early),
      entry("b", DialogState.Confirmed)
    )
    val update = info(
      4,
      Coverage.Partial,
      entry("a", DialogState.Confirmed),  // changed
      entry("b", DialogState.Terminated), // over
      entry("c", DialogState.Trying)      // new
    )
    assertEquals(
      DialogInfo.track(Some(held), update),
      Right(
        info(
          4,
          Coverage.Full,
          entry("a", DialogState.Confirmed),
          entry("c", DialogState.Trying)
        )
      )
    )
  }

  test(
    "a partial update that is not the next one, or has nothing full to amend, is a missed update"
  ) {
    val held = info(3, Coverage.Full, entry("a", DialogState.Early))
    assertEquals(
      DialogInfo.track(
        Some(held),
        info(5, Coverage.Partial, entry("a", DialogState.Confirmed))
      ),
      Left(MissedUpdate(Some(3), 5))
    )
    assertEquals(
      DialogInfo.track(
        None,
        info(2, Coverage.Partial, entry("a", DialogState.Confirmed))
      ),
      Left(MissedUpdate(None, 2))
    )
  }

  test(
    "an update that is not newer than the state held is a repeat or came late, and changes nothing"
  ) {
    val held = info(4, Coverage.Full, entry("a", DialogState.Confirmed))
    for version <- List(4L, 3L, 0L) do {
      assertEquals(
        DialogInfo.track(
          Some(held),
          info(version, Coverage.Full, entry("z", DialogState.Early))
        ),
        Right(held)
      )
      assertEquals(
        DialogInfo.track(
          Some(held),
          info(version, Coverage.Partial, entry("a", DialogState.Terminated))
        ),
        Right(held)
      )
    }
  }

  test(
    "a run of updates read from documents ends in the state the notifier meant"
  ) {
    val documents = List(
      document(dialog("a", "trying") + dialog("b", "confirmed"), "1", "full"),
      document(dialog("a", "early"), "2", "partial"),
      document(
        dialog("a", "confirmed") + dialog("c", "trying"),
        "3",
        "partial"
      ),
      document(dialog("b", "terminated"), "4", "partial")
    )
    val result = documents.foldLeft(Option.empty[DialogInfo]) { (known, text) =>
      val update = DialogInfo.parse(text).fold(e => fail(e.message), identity)
      DialogInfo.track(known, update).fold(m => fail(m.toString), Some(_))
    }
    assertEquals(
      result.map(i => i.version -> i.dialogs.map(d => d.id -> d.state)),
      Some(4L -> List("a" -> DialogState.Confirmed, "c" -> DialogState.Trying))
    )
  }
}
