package edu.illinois.ncsa.daffodil.util

import edu.illinois.ncsa.daffodil.exceptions.Assert

/**
 * Define an OK or Error type.
 *
 * This is almost exactly a Maybe[String], but the sense of
 * 'isDefined' is backward in a Maybe type. The Nope would represent Ok, so
 * isEmpty would mean Ok, and isDefined would mean error, and that's so
 * unintuitive, that we define our own Maybe-like value class here
 * so that isOK and isError are the tests.
 */
object OKOrError {
  private val okValue: String = null

  val OK = new OKOrError(okValue)

  def Error(s: String) = {
    Assert.usage(s ne null)
    Assert.usage(s != okValue)
    new OKOrError(s)
  }
}

class OKOrError private (val errMsg: String) extends AnyVal {
  @inline def isOK = this.errMsg eq OKOrError.okValue
  @inline def isError = !isOK
}
