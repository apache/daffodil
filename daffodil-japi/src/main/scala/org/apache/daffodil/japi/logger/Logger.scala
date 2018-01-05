/* Copyright (c) 2013 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.japi.logger

import java.io.File
import scala.collection.JavaConversions._

/**
 * Abstract log writer, which can be overridden to create a custom log writer.
 */
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
        msg.format(args: _*)
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
final class ConsoleLogWriter extends LogWriter {
  protected def write(level: LogLevel, logID: String, msg: String): Unit = {}
}

/**
 * [[LogWriter]] that drops all log messages
 */
final class NullLogWriter extends LogWriter {
  protected def write(level: LogLevel, logID: String, msg: String): Unit = {}
}

/**
 * [[LogWriter]] that writes all log messages to a file.
 *
 * @param file the file to write log messages to
 */
final class FileLogWriter(file: File) extends LogWriter {
  /**
   * Retrieve the file the log writer writes to
   *
   * @return the file the log writer writes to
   */
  def getFile = file
  protected def write(level: LogLevel, logID: String, msg: String): Unit = {}
}
