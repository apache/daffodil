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
package org.apache.daffodil.lib.util

import java.io.PrintWriter
import java.io.StringWriter
import java.util.ServiceConfigurationError
import java.util.ServiceLoader
import scala.collection.mutable.ArrayBuffer

trait SimpleNamedLoadableService { def name(): String }

/**
 * Contains methods for dynamic loading of classes from the class path.
 *
 * It is a constrained simple model where a service is provided by a class having a name() method,
 * and found by users by way of a simple map from names to an instance of the class.
 *
 * The class must have a zero-arg constructor.
 */
object SimpleNamedServiceLoader {

  /**
   * Load all instances of a particular class from the class path that are
   * declared service providers
   * @param clazz The class to be loaded. E.g., classOf[LayerCompiler]
   * @tparam T The type to be loaded. Usually this is inferred and isn't explicitly supplied.
   *           It must have a name member returning a string. It must have a default (no-arg) constructor.
   * @return A map from the name (string) to the corresponding instance of the class.
   */
  def loadClass[T <: SimpleNamedLoadableService](clazz: Class[T]): Map[String, T] = {
    val thingName = Misc.getNameGivenAClassObject(clazz)
    val iter = ServiceLoader.load(clazz).iterator()
    val instanceBuf = new ArrayBuffer[T]
    while (iter.hasNext()) { // a throw from hasNext() just propagates. It's fatal.
      val compilerOpt =
        try {
          instanceBuf += iter.next()
        } catch {
          case e: ServiceConfigurationError => {
            Logger.log.warn(
              s"Named service $thingName failed to load: ${e.getMessage}. Enable debug logging for more details"
            )
            Logger.log.debug({
              val sw = new StringWriter()
              val pw = new PrintWriter(sw, true)
              e.printStackTrace(pw)
              pw.close()
              sw.toString()
            })
          }
        }
    }
    val instancesFound: Map[String, Seq[T]] = instanceBuf.toSeq.groupBy { _.name() }
    val instanceMap: Map[String, T] = instancesFound.toSeq.flatMap {
      case (name, Seq(lc)) => Some((name, lc))
      case (name, seq) => {
        Logger.log.warn(
          s"Duplicate classes for $thingName found. Ignored: ${seq.map { Misc.getNameFromClass(_) }.mkString(", ")}."
        )
        None
      }
    }.toMap
    instanceMap
  }
}
