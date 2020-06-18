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

package org.apache.daffodil.example

import org.apache.daffodil.sapi.logger._

import scala.collection.mutable.ListBuffer

class LogWriterForSAPITest2 extends LogWriter {
  val errors = new ListBuffer[String]()
  val warnings = new ListBuffer[String]()
  val infos = new ListBuffer[String]()
  val others = new ListBuffer[String]()

  def write(level: LogLevel.Value, logID: String, msg: String): Unit = {
    level match {
      case LogLevel.Error => errors.append(msg)
      case LogLevel.Warning => warnings.append(msg)
      case LogLevel.Info => infos.append(msg)
      case _ => others.append(msg)
    }
  }

  override def prefix(level: LogLevel.Value, logID: String): String = {
    val prefix = level match {
      case LogLevel.Error => "[error] "
      case LogLevel.Warning => "[warning] "
      case LogLevel.Info => "[info] "
      case LogLevel.Compile => "[compile] "
      case LogLevel.Debug => "[debug] "
      case LogLevel.DelimDebug => "[delimdebug] "
      case LogLevel.OOLAGDebug => "[oolagdebug] "
      case _ => "[unknown] "
    }
    "[SAPI LOG] " + prefix
  }

  override def suffix(level: LogLevel.Value, logID: String): String = {
    " [END]"
  }

}
