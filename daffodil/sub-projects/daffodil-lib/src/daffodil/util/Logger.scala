package daffodil.util
import scala.actors.Actor
import scala.actors.Actor._
import java.text.SimpleDateFormat
import java.util.Date
import java.io.File
import java.io.PrintStream
import java.io.FileOutputStream

/**
 * Simple logging system found on Stack Overflow, on the web.
 */

object LogLevel extends Enumeration {
  val Error = Value(10)
  val Warning = Value(20)
  val Info = Value(30)
  val Debug = Value(40) // does not time-stamp messages. 
}

object Error {
  def apply(msg : String, args : Any*) = new Glob(LogLevel.Error, msg, args)
}

object Warning {
  def apply(msg : String, args : Any*) = new Glob(LogLevel.Warning, msg, args)
}

object Info {
  def apply(msg : String, args : Any*) = new Glob(LogLevel.Info, msg, args)
}

object Debug {
  def apply(msg : String, args : Any*) = new Glob(LogLevel.Debug, msg, args)
}

trait Identity {
  def logID : String
}

abstract class LogWriter {
  protected val writer : Actor
  protected val tstampFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss ")

  def tstamp = tstampFormat.format(new Date)

  def log(logID : String, glob : daffodil.util.Glob) {
    val mess = glob.stringify
    val areStamping = glob.lvl < LogLevel.Debug
    val prefix = if (areStamping) tstamp + " " + logID + " " + glob.lvl else ""
    writer ! (prefix + " [" + mess + "]")
  }
}

object ForUnitTestLogWriter extends LogWriter {
  var loggedMsg : String = null
  protected val writer = actor { loop { react { case msg : String => 
    loggedMsg = msg
    Console.out.println("Was Logged: " + loggedMsg)
    Console.out.flush()
    } } }
}

object NullLogWriter extends LogWriter {
  protected val writer = actor { loop { react { case msg : String => } } }
}

object ConsoleWriter extends LogWriter {
  protected val writer = actor {
    loop {
      react {
        case msg : String =>
          Console.out.println(msg);
          Console.flush case _ =>
      }
    }
  }
}

class FileWriter(val file : File) extends LogWriter {
  require(file != null)
  require(file.canWrite)

  protected val writer = actor { loop { react { case msg : String => destFile.println(msg); destFile.flush case _ => } } }

  private val destFile = {
    try { new PrintStream(new FileOutputStream(file)) }
    catch {
      case e =>
        ConsoleWriter.log("FileWriter", Error("Unable to create FileWriter for file %s exception was %s", file, e)); Console.out
    }
  }

}

/**
 * i18n support for logging.
 *
 * Instead of creating a string, worrying about locale-based reinterpretation, etc.
 * Just make a Glob (short for globalized message). This is intended to be passed by name,
 *
 */
class Glob(val lvl : LogLevel.Value, val msg : String, args : Seq[Any]) {
  // for now: quick and dirty English-centric approach.
  // In the future, use msg to index into i18n resource bundle for 
  // properly i18n-ized string. Can use context to avoid ambiguities.
  def stringify = {
    val str = msg.format(args : _*)
    str
  }
}

trait Logging extends Identity {

  lazy val logID = {
    val className = getClass().getName()
    if (className endsWith "$") className.substring(0, className.length - 1)
    else className
  }

  protected var logWriter : LogWriter = LoggingDefaults.logWriter
  protected var logLevel = LoggingDefaults.logLevel

  def setLoggingLevel(level : LogLevel.Value) { logLevel = level }

  def setLogWriter(lw : LogWriter) { if (lw != null) logWriter = lw }

  def log(glob : => Glob) { if (logLevel  >= glob.lvl) logWriter.log(logID, glob)
  }

}

object LoggingDefaults {

  var logLevel = LogLevel.Info
  var logWriter : LogWriter = ConsoleWriter

  def setLoggingLevel(level : LogLevel.Value) { logLevel = level }

  def setLogWriter(lw : LogWriter) { if (lw != null) logWriter = lw }
}
