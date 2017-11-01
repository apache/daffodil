package org.apache.daffodil.api

import org.apache.daffodil.util.Maybe._
import org.apache.daffodil.util.Maybe

/**
 * Exceeding these limits is not a back-trackable parse error. It's more severe
 * than that. But it is not a schema-definition error really either.
 */
final class TunableLimitExceededError(limitName: String, cause: Option[Throwable], formatString: String, val args: Any*)
  extends Diagnostic(Nope, Nope, cause, Maybe(formatString), args: _*) {

  def isError = true
  def modeName = "Tunable Limit"

  def this(limitName: String, msg: String, args: Any*) = this(limitName, None, msg, args: _*)

  def this(limitName: String, cause: Throwable) = this(limitName, Some(cause), "")

}
