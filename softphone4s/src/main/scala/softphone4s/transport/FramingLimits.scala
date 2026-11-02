package softphone4s.transport

import pureconfig.ConfigReader
import softphone4s.model.ByteLimit

/** The largest message a stream may carry before the connection is dropped. */
final case class FramingLimits(
    maxHeaderBytes: ByteLimit = ByteLimit.Headers,
    maxBodyBytes: ByteLimit = ByteLimit.Body
) derives ConfigReader
