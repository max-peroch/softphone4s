package softphone4s.presence

import munit.FunSuite
import softphone4s.xml.XmlTree
import softphone4s.xml.XmlTree.Element

/** The activities of rich presence (RFC 4480), in PIDF. */
class RpidSuite extends FunSuite {

  private val Pidf = "urn:ietf:params:xml:ns:pidf"
  private val Dm   = "urn:ietf:params:xml:ns:pidf:data-model"
  private val Rpid = "urn:ietf:params:xml:ns:pidf:rpid"

  /** The elements the schema of RFC 4480 §5.1 has for activities. */
  private val SchemaNames = List(
    "appointment",
    "away",
    "breakfast",
    "busy",
    "dinner",
    "holiday",
    "in-transit",
    "looking-for-work",
    "meal",
    "meeting",
    "on-the-phone",
    "performance",
    "permanent-absence",
    "playing",
    "presentation",
    "shopping",
    "sleeping",
    "spectator",
    "steering",
    "travel",
    "tv",
    "unknown",
    "vacation",
    "working",
    "worship"
  )

  private def document(inside: String) =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<presence xmlns="$Pidf" xmlns:dm="$Dm" xmlns:rpid="$Rpid" entity="pres:alice@example.com">
       |$inside
       |</presence>""".stripMargin

  private def read(body: String): Presence =
    Presence.parse(body).fold(e => fail(e.message), identity)

  private def person(inside: String) =
    s"""<dm:person id="p1"><rpid:activities>$inside</rpid:activities></dm:person>"""

  private val alice = Presence("pres:alice@example.com", Nil, Nil)

  private def tree(xml: String): Element =
    XmlTree.parse(xml).fold(e => fail(e), identity)

  private def rendered(presence: Presence): String =
    Presence.render(presence).fold(e => fail(e), identity)

  test("away, with a note and times, as in the example of RFC 4480") {
    val body = document(
      """<tuple id="t1"><status><basic>open</basic></status></tuple>
        |<dm:person id="p1">
        |  <rpid:activities from="2005-05-30T12:00:00+05:00" until="2005-05-30T17:00:00+05:00">
        |    <rpid:note>Far away</rpid:note>
        |    <rpid:away/>
        |  </rpid:activities>
        |</dm:person>""".stripMargin
    )
    val presence = read(body)
    // the note and the times are not read, and the tuple is as it was
    assertEquals(presence.activities, List(Activity.Away))
    assert(presence.isOpen)
  }

  test("several activities, in the order given, and text for other") {
    val body = document(
      person(
        """<rpid:vacation/><rpid:breakfast/><rpid:other>reading</rpid:other>"""
      )
    )
    assertEquals(
      read(body).activities,
      List(Activity.Vacation, Activity.Breakfast, Activity.Other("reading"))
    )
  }

  test("every activity of the schema is read by its name, and written by it") {
    val all = SchemaNames.map(n => s"<rpid:$n/>").mkString
    val got = read(document(person(all))).activities
    assertEquals(got.flatMap(Activity.element), SchemaNames)
    assertEquals(Activity.Known.flatMap(Activity.element), SchemaNames)
  }

  test(
    "a name of the namespace that is not known, such as lunch, is kept as other text"
  ) {
    // The prose of RFC 4480 lists lunch; its schema does not have it.
    assertEquals(
      read(document(person("<rpid:lunch/>"))).activities,
      List(Activity.Other("lunch"))
    )
  }

  test("an other with no text says nothing") {
    assertEquals(
      read(
        document(person("<rpid:other/><rpid:other>  </rpid:other>"))
      ).activities,
      Nil
    )
  }

  test("elements of other namespaces are skipped, and prefixes may differ") {
    val body =
      s"""<p:presence xmlns:p="$Pidf" xmlns:d="$Dm" xmlns:r="$Rpid" xmlns:x="urn:example:ext" entity="e">
         |  <d:person id="a">
         |    <r:activities>
         |      <x:mood>happy</x:mood>
         |      <r:busy/>
         |      <x:busy/>
         |    </r:activities>
         |  </d:person>
         |</p:presence>""".stripMargin
    assertEquals(read(body).activities, List(Activity.Busy))
  }

  test(
    "activities are the person's: those of a tuple or a device are not read"
  ) {
    val body = document(
      s"""<tuple id="t"><status><basic>open</basic></status><rpid:activities><rpid:busy/></rpid:activities></tuple>
         |<dm:device id="d"><rpid:activities><rpid:away/></rpid:activities></dm:device>
         |<rpid:activities><rpid:meeting/></rpid:activities>""".stripMargin
    )
    assertEquals(read(body).activities, Nil)
  }

  test("the activities of every person are read, in order") {
    val body = document(
      person("<rpid:busy/>") +
        """<dm:person id="p2"><rpid:activities><rpid:on-the-phone/></rpid:activities></dm:person>"""
    )
    assertEquals(
      read(body).activities,
      List(Activity.Busy, Activity.OnThePhone)
    )
  }

  test("a document that says nothing of activities has none") {
    assertEquals(
      read(
        document(
          """<tuple id="t"><status><basic>closed</basic></status></tuple>"""
        )
      ).activities,
      Nil
    )
    assertEquals(read(document("")).activities, Nil)
  }

  // ---- Writing ------------------------------------------------------------------

  test(
    "a presence with no activities is written as it was: no person, no namespaces"
  ) {
    val xml = rendered(
      alice.copy(tuples =
        List(PresenceTuple("t", Some(Availability.Open), None, Nil))
      )
    )
    assert(!xml.contains("rpid"), xml)
    assert(!xml.contains("dm:"), xml)
  }

  test(
    "activities are written under a person, in the namespaces of RFC 4479 and 4480"
  ) {
    val xml = rendered(
      alice.copy(activities = List(Activity.Busy, Activity.OnThePhone))
    )
    val person = tree(xml).children.find(_.is(Dm, "person"))
    assert(person.isDefined, xml)
    val activities =
      person.toList.flatMap(_.children.filter(_.is(Rpid, "activities")))
    assertEquals(activities.size, 1)
    assertEquals(
      activities.flatMap(_.children).map(c => c.namespace -> c.name.local),
      List(Some(Rpid) -> "busy", Some(Rpid) -> "on-the-phone")
    )
  }

  test("what is written is read back as it was, for every activity") {
    val presence =
      alice.copy(activities = Activity.Known :+ Activity.Other("reading"))
    assertEquals(Presence.parse(rendered(presence)), Right(presence))
  }

  test(
    "the person's id is none of the tuples', which are ids of the same document"
  ) {
    val tuples = List("p1", "p2").map(id => PresenceTuple(id, None, None, Nil))
    val xml    =
      rendered(alice.copy(tuples = tuples, activities = List(Activity.Busy)))
    def idsIn(e: Element): List[String] =
      e.attribute("id").toList ++ e.children.flatMap(idsIn)
    val ids = idsIn(tree(xml))
    assertEquals(ids.size, ids.distinct.size, ids.toString)
    assert(ids.contains("p3"), ids.toString)
  }

  test("text with markup in an other is escaped, and read back as it was") {
    val text     = """</rpid:other><rpid:busy/> & "quoted" <x>"""
    val presence = alice.copy(activities = List(Activity.Other(text)))
    val xml      = rendered(presence)
    assertEquals(Presence.parse(xml), Right(presence))
    // and the markup in it did not become an activity
    assertEquals(read(xml).activities, List(Activity.Other(text)))
  }

  test(
    "an activity that cannot be written is refused: text XML lacks, or no text"
  ) {
    assert(
      Presence
        .render(alice.copy(activities = List(Activity.Other("bell\u0007"))))
        .isLeft
    )
    assert(
      Presence
        .render(alice.copy(activities = List(Activity.Other("  "))))
        .isLeft
    )
  }

  test("an activity does not change whether the entity is open") {
    val open = PresenceTuple("t", Some(Availability.Open), None, Nil)
    assert(
      alice.copy(tuples = List(open), activities = List(Activity.Away)).isOpen
    )
  }

  test("what the notifier names is mapped by Activity.fromElement") {
    assertEquals(Activity.fromElement("busy", ""), Some(Activity.Busy))
    assertEquals(
      Activity.fromElement("other", " hi "),
      Some(Activity.Other("hi"))
    )
    assertEquals(Activity.fromElement("other", ""), None)
    assertEquals(Activity.fromElement("nap", ""), Some(Activity.Other("nap")))
  }
}
