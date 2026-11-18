package softphone4s.model

/** A set of ASCII characters, for matching tokens without a regex. */
private[softphone4s] final class CharSet private (allowed: Array[Boolean]) {

  def contains(c: Char): Boolean = c < CharSet.Size && allowed(c)

  /** This set plus the characters of `extra`. */
  def +(extra: String): CharSet = {
    val copy = allowed.clone()
    extra.foreach(c => copy(c) = true)
    new CharSet(copy)
  }
}

private[softphone4s] object CharSet {

  private val Size = 128

  val AlphaNumeric: CharSet = {
    val a = new Array[Boolean](Size)
    ('a' to 'z').foreach(c => a(c) = true)
    ('A' to 'Z').foreach(c => a(c) = true)
    ('0' to '9').foreach(c => a(c) = true)
    new CharSet(a)
  }
}
