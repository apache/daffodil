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

package org.apache.daffodil.slf4j

import java.io.PrintStream
import java.util.concurrent.ConcurrentHashMap

import org.slf4j.ILoggerFactory
import org.slf4j.IMarkerFactory
import org.slf4j.Logger
import org.slf4j.Marker
import org.slf4j.event.Level
import org.slf4j.helpers.BasicMarkerFactory
import org.slf4j.helpers.AbstractLogger
import org.slf4j.helpers.MessageFormatter
import org.slf4j.helpers.NOPMDCAdapter
import org.slf4j.spi.MDCAdapter
import org.slf4j.spi.SLF4JServiceProvider

class DaffodilSLF4JServiceProvider extends SLF4JServiceProvider {
  override lazy val getLoggerFactory: ILoggerFactory = new DaffodilLoggerFactory()
  override lazy val getMarkerFactory: IMarkerFactory = new BasicMarkerFactory()
  override lazy val getMDCAdapter: MDCAdapter = new NOPMDCAdapter()
  override lazy val getRequestedApiVersion: String = "2.0.99"
  override def initialize(): Unit = {}
}

class DaffodilLoggerFactory extends ILoggerFactory {

  private val loggerMap = new ConcurrentHashMap[String, DaffodilLogger]()

  override def getLogger(name: String): Logger = {
    val daffodilLogger = loggerMap.get(name)
    if (daffodilLogger != null) {
      daffodilLogger
    } else {
      val newInstance = new DaffodilLogger(name)
      val oldInstance = loggerMap.putIfAbsent(name, newInstance)
      if (oldInstance == null) {
        newInstance
      } else {
        oldInstance
      }
    }
  }
}

/**
 * A logger that should only be used by specifically by Daffodil (e.g. CLI, TDML
 * Runner) to support thread specific log levels and log streams. This is mostly
 * useful for testing where we want to allow each test thread to log at a
 * different level and stream. To set the level and stream, one should call:
 *
 *   daffodilLogger.setThreadLoggerConfig(level, stream)
 *
 * When done, the config should be removed by calling:
 *
 *   daffodilLogger.removeThreadLoggerConfig()
 */
class DaffodilLogger(name: String) extends AbstractLogger {

  case class LoggerConfig(level: Level, stream: PrintStream)

  private val perThreadLoggerConfig = new ThreadLocal[LoggerConfig] {
    // defaults to null which disables log writing. If logs need to be written
    // to a stream (e.g. output for CLI, capture for testing), then the thread
    // requiring it should call the setThreadLoggerConfig and
    // removeThreadLoggerConfig funcctions.
    override def initialValue() = null
  }

  /**
   * Set the log level and stream to use for the calling thread
   */
  def setThreadLoggerConfig(level: Level, stream: PrintStream): Unit = {
    val loggerConfig = LoggerConfig(level, stream)
    perThreadLoggerConfig.set(loggerConfig)
  }

  /**
   * Clean up logger config state associated with this thread
   */
  def removeThreadLoggerConfig(): Unit = {
    perThreadLoggerConfig.remove()
  }

  override protected def getFullyQualifiedCallerName(): String = null

  override protected def handleNormalizedLoggingCall(
    level: Level,
    marker: Marker,
    msg: String,
    arguments: Array[Object],
    throwable: Throwable): Unit = {

    val loggerConfig = perThreadLoggerConfig.get
    if (loggerConfig != null) {
      val buf = new StringBuilder()

      val levelStr = level.toString.toLowerCase
      buf.append('[')
      buf.append(levelStr)
      buf.append("] ")

      // a weird quirk of the scala-logging library is that if s-interpolation is used and
      // the last interpolated variable is a Throwable, then it thinks we're calling the
      // Throwable variant of a log function, and so the Throwable isn't in the arguments
      // array but is in the throwable variable. We don't use the Throwable variant in
      // Daffodil, so if there is a throwable, just append it to the arguments array and
      // the MessageFormatter will convert it to a string as intended
      val fixedArgs =
        if (throwable != null) {
          if (arguments == null) {
            Array[Object](throwable)
          } else {
            arguments :+ throwable
          }
        } else {
          arguments
        }

      val formattedMessage = MessageFormatter.basicArrayFormat(msg, fixedArgs)
      buf.append(formattedMessage)

      loggerConfig.stream.println(buf.toString)
      loggerConfig.stream.flush()
    }
  }

  override def isErrorEnabled(): Boolean = isLevelEnabled(Level.ERROR)

  override def isErrorEnabled(marker: Marker): Boolean = isErrorEnabled()

  override def isWarnEnabled(): Boolean = isLevelEnabled(Level.WARN)

  override def isWarnEnabled(marker: Marker): Boolean = isWarnEnabled()

  override def isInfoEnabled(): Boolean = isLevelEnabled(Level.INFO)

  override def isInfoEnabled(marker: Marker): Boolean = isInfoEnabled()

  override def isDebugEnabled(): Boolean = isLevelEnabled(Level.DEBUG)

  override def isDebugEnabled(marker: Marker): Boolean = isDebugEnabled()

  override def isTraceEnabled(): Boolean = isLevelEnabled(Level.TRACE)

  override def isTraceEnabled(marker: Marker): Boolean = isTraceEnabled()

  private def isLevelEnabled(level: Level): Boolean = {
    val loggerConfig = perThreadLoggerConfig.get
    loggerConfig != null && loggerConfig.level.toInt <= level.toInt
  }

}
