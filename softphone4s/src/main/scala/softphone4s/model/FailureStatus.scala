package softphone4s.model

import cats.{Eq, Show}

/** A final failure status code, 300 to 699, with the reason phrase we send. */
opaque type FailureStatus = Int

object FailureStatus {

  def apply(code: Int): Either[String, FailureStatus] =
    Either.cond(
      code >= 300 && code <= 699,
      code,
      s"not a failure status: $code"
    )

  val Forbidden: FailureStatus              = 403
  val NotFound: FailureStatus               = 404
  val TemporarilyUnavailable: FailureStatus = 480
  val RequestTerminated: FailureStatus      = 487
  val NotAcceptableHere: FailureStatus      = 488
  val BusyHere: FailureStatus               = 486
  val ServerInternalError: FailureStatus    = 500
  val NotImplemented: FailureStatus         = 501
  val Decline: FailureStatus                = 603
  val BadRequest: FailureStatus             = 400
  val CallDoesNotExist: FailureStatus       = 481
  val RequestPending: FailureStatus         = 491

  private val phrases: Map[Int, String] = Map(
    400 -> "Bad Request",
    403 -> "Forbidden",
    404 -> "Not Found",
    480 -> "Temporarily Unavailable",
    481 -> "Call/Transaction Does Not Exist",
    486 -> "Busy Here",
    487 -> "Request Terminated",
    488 -> "Not Acceptable Here",
    491 -> "Request Pending",
    500 -> "Server Internal Error",
    501 -> "Not Implemented",
    503 -> "Service Unavailable",
    603 -> "Decline"
  )

  extension (s: FailureStatus) {
    def value: Int = s

    /** The phrase sent with the code; a generic one for codes without a name.
      */
    def reason: String = phrases.getOrElse(
      s,
      if s < 400 then "Redirection"
      else if s < 500 then "Request Failure"
      else if s < 600 then "Server Failure"
      else "Global Failure"
    )
  }

  given Eq[FailureStatus]   = Eq.fromUniversalEquals
  given Show[FailureStatus] = Show.show(s => s"${s.value} ${s.reason}")
}
