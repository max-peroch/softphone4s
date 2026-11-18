package softphone4s.transaction

import softphone4s.model.SipResponse

/** The three kinds of response a transaction treats differently. */
enum ResponseClass {
  case Provisional, Success, Failure
}

object ResponseClass {

  /** 1xx, 2xx and 3xx-6xx. */
  def of(response: SipResponse): ResponseClass =
    if response.statusCode < 200 then Provisional
    else if response.statusCode < 300 then Success
    else Failure
}
