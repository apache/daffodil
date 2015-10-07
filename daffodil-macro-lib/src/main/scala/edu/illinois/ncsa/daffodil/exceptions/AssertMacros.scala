package edu.illinois.ncsa.daffodil.exceptions

import scala.reflect.macros.blackbox.Context

object AssertMacros {

  def usageMacro2(c: Context)(testAbortsIfFalse: c.Tree, message: c.Tree): c.Tree = {
    import c.universe._

    val testAsString = testAbortsIfFalse.toString

    q"""
    if (!($testAbortsIfFalse)) {
         Assert.abort2("Usage error: " + $message, $testAsString)
    }
    """
  }

  def usageMacro1(c: Context)(testAbortsIfFalse: c.Tree): c.Tree = {
    import c.universe._

    val testAsString = testAbortsIfFalse.toString

    q"""
    if (!($testAbortsIfFalse)) {
         Assert.abort("Usage error: " + $testAsString)
    }
    """
  }

  def invariantMacro1(c: Context)(testAbortsIfFalse: c.Tree): c.Tree = {
    import c.universe._

    val testAsString = testAbortsIfFalse.toString

    q"""
    if (!($testAbortsIfFalse)) {
         Assert.abort("Invariant broken: " + $testAsString)
    }
    """
  }

  def notYetImplementedMacro0(c: Context)(): c.Tree = {
    import c.universe._

    q"""
         Assert.nyi()
    """
  }

  def notYetImplementedMacro1(c: Context)(testThatWillThrowIfTrue: c.Tree): c.Tree = {
    import c.universe._

    val testAsString = testThatWillThrowIfTrue.toString

    q"""
    if ($testThatWillThrowIfTrue){
         Assert.nyi($testAsString)
    }
    """
  }

  def notYetImplementedMacro2(c: Context)(testThatWillThrowIfTrue: c.Tree, msg: c.Tree): c.Tree = {
    import c.universe._

    val testAsString = testThatWillThrowIfTrue.toString

    q"""
    if ($testThatWillThrowIfTrue){
         Assert.nyi($msg + " (" + $testAsString + ")")
    }
    """
  }

}
