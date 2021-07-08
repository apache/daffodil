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

package org.apache.daffodil.japi.logger

import java.io.File
import scala.collection.JavaConverters._

/**
 * Abstract log writer, which can be overridden to create a custom log writer.
 */
@deprecated("Use Log4j for logging", "3.2.0")
abstract class LogWriter {
  /**
   * Implement this method to determine how the log message is written.
   *
   * @param level level of the message
   * @param logID identification of class that created the log message
   * @param msg log message
   */
  protected def write(level: LogLevel, logID: String, msg: String): Unit

  /**
   * Override to change the prefix string of the log message.
   *
   * By default, the prefix is the empty string.
   *
   * @param level level of the message
   * @param logID identification of class that created the log message
   * @return the prefix to use for log messages. Defaults to the empty string if not overridden.
   */
  protected def prefix(level: LogLevel, logID: String): String = ""

  /**
   * Override to change the suffix string of the log message.
   *
   * By default, the suffix is the empty string.
   *
   * @param level level of the message
   * @param logID identification of class that created the log message
   * @return the suffix to use for log messages. Defaults to the empty string if not overridden.
   */
  protected def suffix(level: LogLevel, logID: String): String = ""

  /**
   * Generates a log message as a string and calls the write method.
   *
   * The default implementation generates a log message based on the prefix,
   * suffix, message string, and log arguments, and passes the generated log
   * message, level, and logId to [[LogWriter#write]].
   *
   * Can be overridden if more control is needed over the logging mechanism and/or
   * log arguments are needed as separate entities.
   *
   * @param level level of the message
   * @param logID identification of class that created the log message
   * @param msg printf-style format string
   * @param args arguments passed to the logger, matching the format string
   */
  def log(level: LogLevel, logID: String, msg: String, args: java.util.List[Any]): Unit = {
    val message =
      if (args.size > 0) {
        msg.format(args.asScala: _*)
      } else {
        msg
      }
    val p = prefix(level, logID)
    val s = suffix(level, logID)
    write(level, logID, p + message + s)
  }
}

/*
 * These three classes are all empty and are not ever actually used. They are
 * just place holders. Whenever the Java API uses one of these, it is
 * translated to the appropriate Scala log writer. They are marked final so
 * that they cannot be extended, since the Scala pattern matcher would still
 * match and use the equivalent Scala log writers and lose any added
 * functionality. One must extend the LogWriter to create their own log writer.
 */

/**
 * [[LogWriter]] that writes log messages to stdout
 */
@deprecated("Use Log4j for logging", "3.2.0")
final class ConsoleLogWriter extends LogWriter {
  protected def write(level: LogLevel, logID: String, msg: String): Unit = {
    //do nothing
  }
}

/**
 * [[LogWriter]] that drops all log messages
 */
@deprecated("Use Log4j for logging", "3.2.0")
final class NullLogWriter extends LogWriter {
  protected def write(level: LogLevel, logID: String, msg: String): Unit = {
    //do nothing
  }
}

/**
 * [[LogWriter]] that writes all log messages to a file.
 *
 * @param file the file to write log messages to
 */
@deprecated("Use Log4j for logging", "3.2.0")
final class FileLogWriter(file: File) extends LogWriter {
  /**
   * Retrieve the file the log writer writes to
   *
   * @return the file the log writer writes to
   */
  def getFile = file
  protected def write(level: LogLevel, logID: String, msg: String): Unit = {
    //do nothing
  }
}
