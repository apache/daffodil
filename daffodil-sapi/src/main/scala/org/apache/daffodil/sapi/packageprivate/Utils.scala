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

package org.apache.daffodil.sapi.packageprivate
// The compiler plugin that converts Scala code to Java code for Javadoc
// generation has no concept of package private. So if there is anything that
// should be package private and thus not part of the public API (e.g.
// utilities for converting from Scala to Java), there isn't a way to exclude
// them from Javadocs. So, when we generate Javadocs, we manually exclude
// anything in the sapi.packageprivate package. So anything that should be package
// private should go in this package.

import org.apache.daffodil.sapi._
import org.apache.daffodil.sapi.logger._
import org.apache.daffodil.sapi.debugger._
import org.apache.daffodil.api.{ ValidationMode => SValidationMode }
import org.apache.daffodil.util.{ LogLevel => SLogLevel }
import org.apache.daffodil.util.{ LogWriter => SLogWriter }
import org.apache.daffodil.debugger.{ InteractiveDebugger => SInteractiveDebugger }
import org.apache.daffodil.debugger.{ InteractiveDebuggerRunner => SInteractiveDebuggerRunner }

private[sapi] object LoggingConversions {

  def levelToScala(lvl: LogLevel.Value): SLogLevel.Type = {
    val slvl: SLogLevel.Type = lvl match {
      case LogLevel.Error => SLogLevel.Error
      case LogLevel.Warning => SLogLevel.Warning
      case LogLevel.Info => SLogLevel.Info
      case LogLevel.Compile => SLogLevel.Compile
      case LogLevel.Resolver => SLogLevel.Resolver
      case LogLevel.Debug => SLogLevel.Debug
      case LogLevel.OOLAGDebug => SLogLevel.OOLAGDebug
      case LogLevel.DelimDebug => SLogLevel.DelimDebug
    }
    slvl
  }

  def levelFromScala(slvl: SLogLevel.Type): LogLevel.Value = {
    val lvl: LogLevel.Value = slvl match {
      case SLogLevel.Error => LogLevel.Error
      case SLogLevel.Warning => LogLevel.Warning
      case SLogLevel.Info => LogLevel.Info
      case SLogLevel.Resolver => LogLevel.Resolver
      case SLogLevel.Compile => LogLevel.Compile
      case SLogLevel.Debug => LogLevel.Debug
      case SLogLevel.OOLAGDebug => LogLevel.OOLAGDebug
      case SLogLevel.DelimDebug => LogLevel.DelimDebug
    }
    lvl
  }
}

private[sapi] object ValidationConversions {

  def modeToScala(mode: ValidationMode.Value): SValidationMode.Type = {
    val smode: SValidationMode.Type = mode match {
      case ValidationMode.Off => SValidationMode.Off
      case ValidationMode.Limited => SValidationMode.Limited
      case ValidationMode.Full => SValidationMode.Full
      case ValidationMode.Custom(v) => SValidationMode.Custom(v)
    }
    smode
  }

  def modeFromScala(smode: SValidationMode.Type): ValidationMode.Value = {
    val mode: ValidationMode.Value = smode match {
      case SValidationMode.Off => ValidationMode.Off
      case SValidationMode.Limited => ValidationMode.Limited
      case SValidationMode.Full => ValidationMode.Full
      case SValidationMode.Custom(v) => ValidationMode.Custom(v)
    }
    mode
  }
}

/* A wrapper log writer that scala logging can talk to, which is then forwarded
 * onto the java LogWriter, if a user implements their own log writer in java.
 */
private[sapi] class JavaLogWriter(logWriter: LogWriter)
    extends SLogWriter {

  protected def write(msg: String): Unit = {
    //do nothing
  }

  override def log(lvl: SLogLevel.Type, logID: String, msg: String, args: Seq[Any]): Unit = {
    if (logWriter != null) {
      logWriter.log(LoggingConversions.levelFromScala(lvl), logID, msg, args)
    }
  }
}

/* A wrapper interctive debugger that scala debugging can talk to, which is
 * then forwarded onto the java interactive debugger, if a user implements
 * their own debugger in java.
 */
private[sapi] class JavaInteractiveDebuggerRunner(dr: DebuggerRunner)
    extends SInteractiveDebuggerRunner {
  def init(id: SInteractiveDebugger): Unit = dr.init
  def getCommand: String = dr.getCommand
  def lineOutput(line: String): Unit = dr.lineOutput(line)
  def fini(): Unit = dr.fini
}

