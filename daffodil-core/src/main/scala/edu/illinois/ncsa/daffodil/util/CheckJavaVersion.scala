package edu.illinois.ncsa.daffodil.util

import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE

object CheckJavaVersion {

  def minMajorJVersion = 1
  def minMinorJVersion = 7
  def checkJavaVersion(context: ThrowsSDE) = {
    val jVersion = {
      try { System.getProperty("java.version") }
      catch {
        case se: SecurityException => context.SDE("Attempted to read property 'java.version' failed due to a SecurityException: \n%s".format(se.getMessage()))
        case _: Throwable => context.SDE("An invalid 'key' was passed to System.getProperty.")
      }
    }
    val javaVersion = """([0-9])\.([0-9])\.(.*)""".r
    jVersion match {
      case javaVersion(major, minor, x) => {

        if (major.toInt < minMajorJVersion) {
          context.SDE("You must run Java 7 (1.7) or higher. You are currently running %s".format(jVersion))
        }
        if (minor.toInt < minMinorJVersion) {
          context.SDE("You must run Java 7 (1.7) or higher. You are currently running %s".format(jVersion))
        }
      }
      case _ => {
        context.SDE("Failed to obtain the Java version.  You must run Java 7 (1.7) or higher.")
      }
    }
  }

}