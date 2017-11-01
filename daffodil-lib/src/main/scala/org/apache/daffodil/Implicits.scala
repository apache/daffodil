/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package org.apache.daffodil

import java.io.{ ByteArrayInputStream, BufferedInputStream }

import org.apache.daffodil.xml.NS
import org.apache.daffodil.exceptions.Assert
import scala.language.reflectiveCalls
import scala.language.implicitConversions
import scala.language.{ implicitConversions, reflectiveCalls } // silences scala 2.10 warnings

object Implicits {

  object ImplicitsSuppressUnusedImportWarning {
    def apply() = if (scala.math.random.isNaN()) Assert.impossible()
  }

  /**
   * Namespace NS objects automatically convertible to string
   */
  implicit def ns2String(ns: NS): String = ns.toString

  /**
   * Convenience: automatically create buffered stream when needed.
   */
  implicit def byteArrayInputStream2bufferedInputStream(bais: ByteArrayInputStream): BufferedInputStream =
    new BufferedInputStream(bais)

  /**
   * Used for reading/writing to database, files, etc.
   * Code From the book "Beginning Scala"
   * http://www.amazon.com/Beginning-Scala-David-Pollak/dp/1430219890
   */
  def using[A <: { def close(): Unit }, B](param: A)(f: A => B): B =
    try { f(param) } finally { param.close() }

  // TODO: move these to TestUtils object in daffodil-lib-unittest
  // add test->test dependency on that

  /**
   * Based on JUnitSuite intercept
   */
  def intercept[T <: AnyRef](body: => Any)(implicit tag: scala.reflect.ClassTag[T]): T = {
    val clazz = tag.runtimeClass.asInstanceOf[Class[T]]
    val caught = try {
      body
      None
    } catch {
      case npe: NullPointerException => throw npe
      case s: scala.util.control.ControlThrowable => throw s
      case u: Throwable => {
        if (!clazz.isAssignableFrom(u.getClass)) {
          throw new InterceptFailedException(
            "Failed to intercept expected exception. Expected '%s' but got '%s'.".format(clazz.getName, u.getClass.getName))
        } else {
          Some(u)
        }
      }
    }
    caught match {
      case None => throw new InterceptFailedException("Failed to intercept any exceptions.")
      case Some(e) => e.asInstanceOf[T]
    }
  }

  class InterceptFailedException(msg: String) extends RuntimeException(msg)

}
