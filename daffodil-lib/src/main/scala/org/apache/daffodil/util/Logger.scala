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

package edu.illinois.ncsa.daffodil.util

import java.text.SimpleDateFormat
import java.util.Date
import java.io.File
import java.io.PrintStream
import java.io.FileOutputStream
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException

/**
 * Simple logging system evolved from code found on Stack Overflow, on the web.
 * http://stackoverflow.com/questions/2018528/logging-in-scala
 * Mostly based on the contribution of Don Mackenzie.
 *
 * Extensively modified to use Macros for performance now.
 */

object LogLevel extends Enum {
  sealed abstract class Type(val lvl: Int) extends EnumValueType with Ordered[Type] {
    def compare(that: LogLevel.Type) = this.lvl - that.lvl
  }

  case object Error extends Type(10)
  case object Warning extends Type(20)
  case object Info extends Type(30)
  case object Resolver extends Type(35)
  case object Compile extends Type(40)
  case object Debug extends Type(50)
  case object OOLAGDebug extends Type(60)
  case object DelimDebug extends Type(70)
}

trait Identity {
  def logID: String
}

abstract class LogWriter {
  protected def write(msg: String): Unit
  protected val tstampFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss:SSS ")

  protected def tstamp = tstampFormat.format(new Date)

  protected def prefix(lvl: LogLevel.Type, logID: String): String = {
    val areStamping = lvl < LogLevel.Debug
    val pre = (if (areStamping) tstamp + " " else "")
    pre + logID + " " + lvl + "["
  }

  protected def suffix(logID: String): String = {
    "]"
  }

  def log(lvl: LogLevel.Type, logID: String, msg: String, args: Seq[Any]) {
    try {
      val mess = Glob.stringify(msg, args)
      val p = prefix(lvl, logID)
      val s = suffix(logID)
      write(p + mess + s)
    } catch {
      case s: scala.util.control.ControlThrowable => throw s
      case u: UnsuppressableException => throw u
      case e: Exception => {
        val (estring, argStrings) =
          try {
            (e.toString, args.map { _.toString })
          } catch {
            case s: scala.util.control.ControlThrowable => throw s
            case u: UnsuppressableException => throw u
            case _: Throwable => (e.getClass.getName, Nil)
          }
        System.err.println("Exception while logging: " + estring)
        System.err.println("msg='%s' args=%s".format(msg, argStrings))
        Assert.abort("Exception while logging")
      }
    }
  }
}

object ForUnitTestLogWriter extends LogWriter {
  var loggedMsg: String = null
  //  protected val writer = actor { loop { react { case msg : String =>
  //    loggedMsg = msg
  //    Console.out.println("Was Logged: " + loggedMsg)
  //    Console.out.flush()
  //    } } }
  def write(msg: String) {
    loggedMsg = msg
  }
}

object NullLogWriter extends LogWriter {
  //protected val writer = actor { loop { react { case msg : String => } } }
  def write(msg: String) {
    // do nothing.
  }
}

object ConsoleWriter extends LogWriter {

  def write(msg: String) {
    Console.err.println(msg)
    Console.flush
  }
}

class FileWriter(val file: File) extends LogWriter {
  require(file != null)
  require(file.canWrite)

  // protected val writer = actor { loop { react { case msg : String => destFile.println(msg); destFile.flush case _ => } } }
  def write(msg: String) {
    destFile.println(msg)
    destFile.flush
  }

  private val destFile = {
    try { new PrintStream(new FileOutputStream(file)) }
    catch {
      case s: scala.util.control.ControlThrowable => throw s
      case u: UnsuppressableException => throw u
      case e: Throwable => {
        ConsoleWriter.log(LogLevel.Error, "FileWriter", "Unable to create FileWriter for file %s exception was %s", Seq(file, e))
        Console.out
      }
    }
  }

}

object Glob {

  // for now: quick and dirty English-centric approach.
  // In the future, use msg to index into i18n resource bundle for
  // properly i18n-ized string. Can use context to avoid ambiguities.
  def stringify(msg: String, args: Seq[Any]) = {
    val res =
      try { // this can fail, if for example the string uses % for other than
        // formats (so if the string is something that mentions DFDL entities,
        // which use % signs in their syntax.
        val str = {
          if (args.size > 0) msg.format(args: _*)
          else msg
        }
        str
      } catch {
        case s: scala.util.control.ControlThrowable => throw s
        case u: UnsuppressableException => throw u
        case e: Exception => {
          val estring = try { e.toString } catch {
            case s: scala.util.control.ControlThrowable => throw s
            case u: UnsuppressableException => throw u
            case _: Throwable => e.getClass.getName
          }
          Assert.abort("An exception occurred whilst logging. Exception: " + estring)
        }
      }
    res
  }
}

trait Logging extends Identity {

  lazy val logID = {
    val className = getClass().getName()
    if (className endsWith "$") className.substring(0, className.length - 1)
    else className
  }

  // Note: below can't be private or protected because macro expansions refer to them,
  // and they would be unreachable from the location of the expansion of the macro.
  var logWriter: Maybe[LogWriter] = Nope
  var logLevel: Maybe[LogLevel.Type] = Nope

  def setLoggingLevel(level: LogLevel.Type) { logLevel = One(level) }

  final def getLoggingLevel(): LogLevel.Type = {
    if (logLevel.isDefined) logLevel.get
    else LoggingDefaults.logLevel
  }

  def setLogWriter(lw: LogWriter) { logWriter = One(lw) }

  def getLogWriter(): LogWriter = {
    if (logWriter.isDefined) logWriter.get
    else LoggingDefaults.logWriter
  }

  final def areLogging(lvl: LogLevel.Type) : Boolean = {
    val l = lvl.lvl
    getLoggingLevel().lvl >= l
  }
  
  protected def doLogging(lvl: LogLevel.Type, msg: String, args: Seq[Any]) =
    getLogWriter.log(lvl, logID, msg, args)

  final def log(lvl: LogLevel.Type, msg: String, args: Any*): Unit = macro LoggerMacros.logMacro
  //    {
  //      val l = level.lvl
  //      if (getLoggingLevel().lvl >= l)
  //        doLogging(level, msg, args.toSeq)
  //    }

  /**
   * Use to make debug printing over small code regions convenient. Turns on
   * your logging level of choice over a lexical region of code. Makes sure it is reset
   * to whatever it was on the exit, even if it throws.
   *
   * Call with no log level argument to turn it off (when done debugging). That way you
   * can leave it sitting there.
   */
  def withLoggingLevel[S](newLevel: LogLevel.Type = getLoggingLevel)(body: => S): Unit = macro LoggerMacros.withLoggingLevelMacro

}

object LoggingDefaults {

  var logLevel: LogLevel.Type = LogLevel.Info
  var logWriter: LogWriter = ConsoleWriter

  def setLoggingLevel(level: LogLevel.Type) { logLevel = level }

  def setLogWriter(lw: LogWriter) {
    Assert.usage(lw != null)
    logWriter = lw
  }
}
