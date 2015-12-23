package edu.illinois.ncsa.daffodil.io

import scala.reflect.macros.blackbox.Context

object IOMacros {

  /**
   * Used to temporarily vary the bit length limit.
   *
   * Implementing as a macro eliminates the creation of a downward function object every time this
   * is called.
   *
   * ISSUE: this macro really wants to use a self reference to `this`. But when a macro is expanded
   * the object that `this` represents changes. Until a better way to do this comes about, we have to pass
   * the `this` object to the `self` argument, which makes calls look like:
   *     dis.withBigLengthLimit(dis, newLimit){... body ...}
   * That looks redundant, and it is, but it's more important to get the allocation of this downward function
   * object out of inner loops.
   */
  def withBitLengthLimitMacro(c: Context)(self: c.Tree, lengthLimitInBits: c.Tree)(body: c.Tree) = {

    import c.universe._

    q"""{
    import edu.illinois.ncsa.daffodil.util.MaybeULong

    val ___dStream = $self
    val ___newLengthLimit = $lengthLimitInBits
    val ___savedLengthLimit = ___dStream.bitLimit0b

    if (!___dStream.setBitLimit0b(MaybeULong(___dStream.bitPos0b + ___newLengthLimit))) false
    else {
      try {
        $body
      } finally {
        ___dStream.resetBitLimit0b(___savedLengthLimit)
      }
      true
    }
  }"""
  }
}
