package softphone4s.model

import scala.collection.immutable.ListMap

/** Header parameters in order of appearance; a flag has no value. */
type HeaderParams = ListMap[ParamName, Option[HeaderParamValue]]
