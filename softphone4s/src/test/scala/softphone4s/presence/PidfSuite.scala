package softphone4s.presence

import munit.FunSuite
import softphone4s.model.MediaType
import softphone4s.subscription.{Notification, NotifyState, PackageError}

class PidfSuite extends FunSuite {

  private def pidf(body: String) =
    Notification(
      NotifyState.Active(None),
      Some(MediaType.PidfXml),
      body
    )

  private def read(body: String) = Pidf.Package.decode(pidf(body))

  private def malformed(body: String): Boolean =
    read(body).left.exists {
      case PackageError.Malformed(_) => true; case _ => false
    }

  test("the example of RFC 3863: an open tuple with a contact") {
    val body =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<presence xmlns="urn:ietf:params:xml:ns:pidf"
        |          entity="pres:someone@example.com">
        |  <tuple id="sg89ae">
        |    <status><basic>open</basic></status>
        |    <contact priority="0.8">tel:09012345678</contact>
        |  </tuple>
        |</presence>""".stripMargin
    assertEquals(
      read(body),
      Right(
        Presence(
          "pres:someone@example.com",
          List(
            PresenceTuple(
              "sg89ae",
              Some(Availability.Open),
              Some("tel:09012345678"),
              Nil
            )
          ),
          Nil
        )
      )
    )
    assert(read(body).exists(_.isOpen))
  }

  test("closed tuples, notes on tuples and on the whole, and several tuples") {
    val body =
      """<presence xmlns="urn:ietf:params:xml:ns:pidf" entity="pres:a@b">
        |  <tuple id="t1"><status><basic>closed</basic></status><note>gone home</note></tuple>
        |  <tuple id="t2"><status><basic>OPEN</basic></status></tuple>
        |  <note>overall</note>
        |</presence>""".stripMargin
    val found = read(body).fold(e => fail(e.message), identity)
    assertEquals(
      found.tuples.map(t => t.id -> t.basic),
      List("t1" -> Some(Availability.Closed), "t2" -> Some(Availability.Open))
    )
    assertEquals(found.tuples.head.notes, List("gone home"))
    assertEquals(found.notes, List("overall"))
    assert(found.isOpen)
  }

  test(
    "an element that leaves the PIDF namespace is not PIDF, though it is named as PIDF is"
  ) {
    val body =
      """<presence xmlns="urn:ietf:params:xml:ns:pidf" entity="e">
        |  <tuple id="a"><status xmlns="urn:other"><basic>open</basic></status></tuple>
        |  <tuple xmlns="urn:other" id="b"><status><basic>open</basic></status></tuple>
        |</presence>""".stripMargin
    // The first tuple has no status of ours; the second is not a tuple of ours.
    assert(malformed(body))
    val back =
      """<presence xmlns="urn:ietf:params:xml:ns:pidf" entity="e">
        |  <x xmlns="urn:other"><tuple id="n"/></x>
        |  <tuple id="a" xmlns:p="urn:ietf:params:xml:ns:pidf"><p:status><basic>open</basic></p:status></tuple>
        |</presence>""".stripMargin
    // Only the second tuple is ours, and its status is spelt with a prefix that
    // the root did not use; <basic> falls in the default namespace, which is ours.
    assertEquals(
      read(back).map(_.tuples.map(t => t.id -> t.basic)),
      Right(List("a" -> Some(Availability.Open)))
    )
  }

  test("a prefixed namespace is read, and extensions in others are skipped") {
    val body =
      """<p:presence xmlns:p="urn:ietf:params:xml:ns:pidf"
        |            xmlns:r="urn:ietf:params:xml:ns:pidf:rpid" entity="pres:a@b">
        |  <p:tuple id="x"><p:status><p:basic>open</p:basic></p:status>
        |    <r:activities><r:busy/></r:activities>
        |  </p:tuple>
        |  <r:person><r:note>not ours</r:note></r:person>
        |</p:presence>""".stripMargin
    val found = read(body).fold(e => fail(e.message), identity)
    assertEquals(found.tuples.map(_.id), List("x"))
    assertEquals(found.notes, Nil)
  }

  test(
    "a tuple that says nothing of its basic status has none, and no tuples is allowed"
  ) {
    val none =
      """<presence xmlns="urn:ietf:params:xml:ns:pidf" entity="e"><tuple id="a"><status/></tuple></presence>"""
    assertEquals(read(none).map(_.tuples.map(_.basic)), Right(List(None)))
    assert(!read(none).exists(_.isOpen))
    val empty = """<presence xmlns="urn:ietf:params:xml:ns:pidf" entity="e"/>"""
    assertEquals(read(empty), Right(Presence("e", Nil, Nil)))
  }

  test("the five entities, character references and CDATA are text") {
    val body =
      """<presence xmlns="urn:ietf:params:xml:ns:pidf" entity="pres:a@b">
        |  <note>fish &amp; chips &lt;3 &#x41;&#66;</note>
        |  <note><![CDATA[<raw & text>]]></note>
        |</presence>""".stripMargin
    assertEquals(
      read(body).map(_.notes),
      Right(List("fish & chips <3 AB", "<raw & text>"))
    )
  }

  test("a DOCTYPE is refused, and an entity it would declare is not expanded") {
    val bomb =
      """<?xml version="1.0"?>
        |<!DOCTYPE presence [<!ENTITY a "aaaaaaaaaa"><!ENTITY b "&a;&a;&a;&a;">]>
        |<presence xmlns="urn:ietf:params:xml:ns:pidf" entity="e"><note>&b;</note></presence>""".stripMargin
    assert(malformed(bomb))
    val external =
      """<!DOCTYPE presence SYSTEM "file:///etc/passwd">
        |<presence xmlns="urn:ietf:params:xml:ns:pidf" entity="e"/>""".stripMargin
    assert(malformed(external))
  }

  test("an entity that is not declared is refused") {
    assert(
      malformed(
        """<presence xmlns="urn:ietf:params:xml:ns:pidf" entity="e"><note>&nbsp;</note></presence>"""
      )
    )
  }

  test("what is not a presence document of the right namespace is refused") {
    assert(malformed("""<presence entity="e"/>"""))
    assert(malformed("""<presence xmlns="urn:other" entity="e"/>"""))
    assert(malformed("""<status xmlns="urn:ietf:params:xml:ns:pidf"/>"""))
  }

  test("a document missing what the RFC requires is refused") {
    val ns = """xmlns="urn:ietf:params:xml:ns:pidf""""
    assert(malformed(s"""<presence $ns/>"""), "no entity")
    assert(
      malformed(
        s"""<presence $ns entity="e"><tuple><status/></tuple></presence>"""
      ),
      "no id"
    )
    assert(
      malformed(s"""<presence $ns entity="e"><tuple id="a"/></presence>"""),
      "no status"
    )
    assert(
      malformed(
        s"""<presence $ns entity="e"><tuple id="a"><status><basic>maybe</basic></status></tuple></presence>"""
      ),
      "bad basic"
    )
  }

  test("text that is not XML is refused, and so is XML that is cut off") {
    assert(malformed("hello"))
    assert(
      malformed(
        """ <presence xmlns="urn:ietf:params:xml:ns:pidf" entity="e"><tuple id="a">"""
      )
    )
    assert(malformed("""<a/><b/>"""))
    assert(
      malformed(
        """<presence xmlns="urn:ietf:params:xml:ns:pidf" entity="e"></tuple></presence>"""
      )
    )
  }

  test(
    "a document nested very deep, or very wide, is answered and does not overflow"
  ) {
    val ns   = """xmlns="urn:ietf:params:xml:ns:pidf""""
    val deep =
      s"""<presence $ns entity="e">""" + "<x>" * 20000 + "</x>" * 20000 + "</presence>"
    assertEquals(read(deep), Right(Presence("e", Nil, Nil)))
    val wide =
      s"""<presence $ns entity="e">""" + "<tuple id=\"a\"><status/></tuple>" * 20000 + "</presence>"
    assertEquals(read(wide).map(_.tuples.size), Right(20000))
  }

  test("no body, or another type, is said so") {
    assertEquals(read(""), Left(PackageError.NoBody))
    assertEquals(
      Pidf.Package.decode(
        Notification(
          NotifyState.Active(None),
          Some(MediaType.Plain),
          "<presence/>"
        )
      ),
      Left(PackageError.UnexpectedType(Some(MediaType.Plain)))
    )
  }

  test("a document is read outside a NOTIFY too") {
    assertEquals(
      Presence.parse(
        """<presence xmlns="urn:ietf:params:xml:ns:pidf" entity="e"/>"""
      ),
      Right(Presence("e", Nil, Nil))
    )
  }

  test("the package asks for presence and takes PIDF") {
    assertEquals(Presence.Package.event.render, "presence")
    assertEquals(Presence.Package.accept, List(MediaType.PidfXml))
    assertEquals(MediaType.PidfXml.render, "application/pidf+xml")
  }
}
