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
