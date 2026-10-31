package softphone4s.sdp

/** An `a=` line: a property (`sendrecv`) or a value (`rtpmap:0 PCMU/8000`). */
final case class Attribute(name: String, value: Option[String]) {

  def render: String = s"a=$name${value.fold("")(v => s":$v")}"
}

object Attribute {

  def parse(text: String): Attribute =
    text.indexOf(':') match {
      case -1 => Attribute(text.trim, None)
      case i  => Attribute(text.take(i).trim, Some(text.drop(i + 1).trim))
    }

  def flag(name: String): Attribute = Attribute(name, None)

  def valued(name: String, value: String): Attribute =
    Attribute(name, Some(value))
}
