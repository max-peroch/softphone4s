package softphone4s.sdp

/** Our answer to an offer, and what it settles. */
final case class Answered(description: SessionDescription, plan: MediaPlan)
