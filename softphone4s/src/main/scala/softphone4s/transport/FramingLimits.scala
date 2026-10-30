package softphone4s.transport

import pureconfig.ConfigReader

/** The largest message a stream may carry before the connection is dropped. */
final case class FramingLimits(
    maxHeaderBytes: Int = 16 * 1024,
    maxBodyBytes: Int = 1024 * 1024
) derives ConfigReader
