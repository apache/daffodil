package edu.illinois.ncsa.daffodil.io

import scala.reflect.macros.blackbox.Context

object IOMacros {

  /**
   * Used to temporarily vary the bit length limit.
   *
   * Implementing as a macro eliminates the creation of a downward function object every time this
   * is called.
   *
   */
  def withBitLengthLimitMacro(c: Context)(lengthLimitInBits: c.Tree)(body: c.Tree) = {

    import c.universe._

    val dStream = TermName(c.freshName)
    val newLengthLimit = TermName(c.freshName)
    val savedLengthLimit = TermName(c.freshName)
    // c.prefix is the expression this macro was expanded on. Not quite same thing as 'this' because we have to be
    // careful not to use it more than once or it will evaluate more than once.
    val selfExp = c.prefix

    q"""{
    import edu.illinois.ncsa.daffodil.util.MaybeULong

    val $dStream = $selfExp
    val $newLengthLimit = $lengthLimitInBits
    val $savedLengthLimit = $dStream.bitLimit0b

    if (!$dStream.setBitLimit0b(MaybeULong($dStream.bitPos0b + $newLengthLimit))) false
    else {
      try {
        $body
      } finally {
        $dStream.resetBitLimit0b($savedLengthLimit)
      }
      true
    }
  }"""
  }
}
