package softphone4s.model

import munit.FunSuite

class CalleeSuite extends FunSuite {

  test("apply accepts digits only") {
    assert(Callee("1234").isRight)
  }

  test("apply accepts letters only") {
    assert(Callee("alice").isRight)
  }

  test("apply accepts a leading plus") {
    assert(Callee("+15551234567").isRight)
  }

  test("apply accepts star and hash") {
    assert(Callee("*67#").isRight)
  }

  test("apply accepts user@host") {
    assert(Callee("alice@sip.example.com").isRight)
  }

  test("apply accepts user@host:port") {
    assert(Callee("alice@192.168.1.10:5060").isRight)
  }

  test("apply rejects empty string") {
    assert(Callee("").isLeft)
  }

  test("apply rejects spaces") {
    assert(Callee("555 1234").isLeft)
    assert(Callee("alice@ho st").isLeft)
  }

  test("apply rejects pause/wait dial modifiers") {
    assert(Callee("1,800,555,1234").isLeft)
    assert(Callee("555!1234").isLeft)
  }

  test("apply rejects an empty host after @") {
    assert(Callee("alice@").isLeft)
  }

  test("apply rejects a non-numeric port") {
    assert(Callee("alice@host:abc").isLeft)
  }

  test("apply rejects multiple @ signs") {
    assert(Callee("alice@bob@host").isLeft)
  }

  test("value extension returns underlying string") {
    val callee = Callee.unsafe("alice@sip.example.com")
    assertEquals(callee.value, "alice@sip.example.com")
  }
}
