package softphone4s.presence

import scala.collection.immutable.ListMap

/** What a person is doing (RFC 4480 §3.2): the `activities` of rich presence,
  * which is where Busy, Away and On the phone are said. A person can be doing
  * several things at once.
  *
  * The values are the elements of the schema, named as it names them, plus
  * [[Activity.Other]], which carries text, for the `other` element and for
  * names of that namespace this does not know (the prose of the RFC lists
  * `lunch`, which its schema does not have).
  */
enum Activity {
  case Appointment
  case Away
  case Breakfast
  case Busy
  case Dinner
  case Holiday
  case InTransit
  case LookingForWork
  case Meal
  case Meeting
  case OnThePhone
  case Performance
  case PermanentAbsence
  case Playing
  case Presentation
  case Shopping
  case Sleeping
  case Spectator
  case Steering
  case Travel
  case Tv
  case Unknown
  case Vacation
  case Working
  case Worship

  /** Anything else, as the text the notifier gave. */
  case Other(text: String)
}

object Activity {

  /** The element of the schema for each value that has one, in the order of the
    * schema.
    */
  private val Elements: ListMap[Activity, String] = ListMap(
    Appointment      -> "appointment",
    Away             -> "away",
    Breakfast        -> "breakfast",
    Busy             -> "busy",
    Dinner           -> "dinner",
    Holiday          -> "holiday",
    InTransit        -> "in-transit",
    LookingForWork   -> "looking-for-work",
    Meal             -> "meal",
    Meeting          -> "meeting",
    OnThePhone       -> "on-the-phone",
    Performance      -> "performance",
    PermanentAbsence -> "permanent-absence",
    Playing          -> "playing",
    Presentation     -> "presentation",
    Shopping         -> "shopping",
    Sleeping         -> "sleeping",
    Spectator        -> "spectator",
    Steering         -> "steering",
    Travel           -> "travel",
    Tv               -> "tv",
    Unknown          -> "unknown",
    Vacation         -> "vacation",
    Working          -> "working",
    Worship          -> "worship"
  )

  /** Every activity the schema names, which is every one but [[Other]]. */
  val Known: List[Activity] = Elements.keys.toList

  private val ByElement: Map[String, Activity] =
    Elements.map((activity, name) => name -> activity)

  /** The name of the element of the schema for this activity, or None for
    * [[Other]], which is written as `<other>` with its text.
    */
  def element(activity: Activity): Option[String] = Elements.get(activity)

  /** The activity an element of the namespace of RPID names: a value of the
    * schema, or [[Other]] for `other` (with its `text`) and for a name that is
    * not known, which is kept as the text. None for `other` with no text.
    */
  def fromElement(name: String, text: String): Option[Activity] =
    ByElement.get(name).orElse {
      val said = if name == "other" then text.trim else name
      Option.when(said.nonEmpty)(Other(said))
    }
}
