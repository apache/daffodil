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

import sbt.util.Level
import sbt.util.Logger
import scala.collection.mutable.ArrayBuffer

/**
 * Logger only to be used in ForkOptions to capture stderr and stdout. Anything
 * logged to info is captured as stdout, anything logged to error is captured
 * as stderr. When used in ForkOptions, all other levels or logging functions
 * should never be used.
 */
case class ForkCaptureLogger() extends Logger {

  val stderr = ArrayBuffer[String]()
  val stdout = ArrayBuffer[String]()

  override def log(level: Level.Value, message: => String): Unit = {
    level match {
      case Level.Info => stdout.append(message)
      case Level.Error => stderr.append(message)
      case _ => sys.error("Should not be possible")
    }
  }
  override def success(message: => String): Unit = sys.error("Should not be possible")
  override def trace(t: => Throwable): Unit = sys.error("Should not be possible")

}
