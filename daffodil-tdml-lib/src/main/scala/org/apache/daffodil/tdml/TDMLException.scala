/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.tdml

import org.apache.daffodil.lib.iapi.Diagnostic
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Misc

object TDMLException {

  def msgWithImpl(msg: String, implementation: Option[String]) =
    implementation
      .map { impl =>
        "(Implementation: " + impl + ") " + msg
      }
      .getOrElse(msg)

  def apply(msg: String, implementation: Option[String]) =
    new TDMLExceptionImpl(msg, implementation)
  def apply(cause: Throwable, implementation: Option[String]) =
    new TDMLExceptionImpl(cause, implementation)
  def apply(causes: Seq[Throwable], implementation: Option[String]) =
    new TDMLExceptionImpl(causes, implementation)
}

/**
 * This is a trait so that we can create these from various exception classes
 * already defined by JUnit or other systems. For example, allows us to
 * define TDMLTestNotCompatibleException, as well as have a family of other
 * exceptions.
 */
trait TDMLException { self: Exception =>
  def msg: String
  def causes: Seq[Throwable]
  def implementation: Option[String]

  /**
   * These method prototypes are here because while this has self-type of Exception
   * that doesn't give it an implementation of all the methods of Exception. The methods
   * we want exposed from Exception have to be mentioned here. They are implemented when
   * this trait is mixed into an Exception class.
   */
  def getMessage(): String
  def getCause(): Throwable

  def asException = self
}

class TDMLExceptionImpl(
  override val msg: String,
  override val causes: Seq[Throwable],
  override val implementation: Option[String]
) extends Exception(
    TDMLException.msgWithImpl(msg, implementation),
    if (causes.length > 0) causes(0) else null
  )
  with TDMLException {

  def this(msg: String, implementation: Option[String]) = this(msg, Nil, implementation)

  def this(cause: Throwable, implementation: Option[String]) =
    this(Misc.getNameFromClass(cause) + ": " + cause.getMessage(), List(cause), implementation)

  def this(causes: Seq[Throwable], implementation: Option[String]) = this(
    causes
      .map { cause => Misc.getNameFromClass(cause) + ": " + cause.getMessage() }
      .mkString("\n"),
    causes,
    implementation
  )
}

/**
 * Use when TDML Runner must add to diagnostic lists held by other objects.
 */
class TDMLDiagnostic(diag: String, implementation: Option[String])
  extends Diagnostic(
    Maybe.Nope,
    Maybe.Nope,
    Maybe.Nope,
    Maybe(TDMLException.msgWithImpl(diag, implementation))
  ) {
  override def isError = true
  override def modeName = "TDML"
}

/**
 * Used to determine when a test will not be run due to not being compatible
 * with the implementation. Useful since this isn't necessarily a failure and
 * may want to be treated differently in some cases.
 *
 * Carries causes because a failure to detect compatibility can be due to
 * failures to reflectively create a junit org.junit.AssumptionViolatedException, and
 * if that is the case, we may need the exception to figure out the reason why
 * the reflective access failed.
 */
class TDMLTestNotCompatibleException(
  testName: String,
  override val implementation: Option[String],
  cause: Option[Throwable] = None
) extends TDMLExceptionImpl(
    "Test '%s' not compatible with implementation.".format(testName),
    cause.toSeq,
    implementation
  )
