package edu.illinois.ncsa.daffodil.util

import scala.reflect.macros.blackbox.Context

object LoggerMacros {

  def logMacro(c: Context)(lvl: c.Tree, msg: c.Tree, args: c.Tree*) = {
    import c.universe._
    q"""
    {
      val level = $lvl
      val l = level.lvl
      if (getLoggingLevel().lvl >= l)
        doLogging(level, $msg, Seq(..$args))
    }
    """
  }

  /**
   * Use to make debug printing over small code regions convenient. Turns on
   * your logging level of choice over a lexical region of code. Makes sure it is reset
   * to whatever it was on the exit, even if it throws.
   */
  def withLoggingLevelMacro(c: Context)(newLevel: c.Tree)(body: c.Tree) = {

    import c.universe._
    q"""{
    val previousLogLevel = logLevel
    logLevel = One($newLevel)
    try $body
    finally {
      logLevel = previousLogLevel
    }
    }"""
  }

}
