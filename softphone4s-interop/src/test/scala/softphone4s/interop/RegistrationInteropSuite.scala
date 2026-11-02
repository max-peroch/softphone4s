package softphone4s.interop

import scala.concurrent.duration.*

class RegistrationInteropSuite extends InteropSuite {

  test("a softphone registers with Asterisk, which then holds its contact") {
    phoneAndServer.use { (phone, asterisk) =>
      phone.register.use { registration =>
        for {
          _        <- registration.awaitRegistered.timeout(20.seconds)
          contacts <- asterisk.cli("pjsip show contacts")
        } yield assert(contacts.contains("alice"), contacts)
      }
    }
  }
}
