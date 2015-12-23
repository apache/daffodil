package edu.illinois.ncsa.daffodil.io

import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.MaybeULong
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import java.nio.CharBuffer

trait DataInputStreamImplMixin extends DataInputStream
  with DataStreamCommonImplMixin
  with LocalBufferMixin {

  /**
   * Returns true if it fills all remaining space in the char buffer.
   *
   * Convenience method since this idiom is so common due to the
   * way fillCharBuffer works to return early when decode errors are
   * encountered.
   */
  protected final def fillCharBufferLoop(cb: CharBuffer): Boolean = {
    var maybeN: MaybeULong = MaybeULong(0)
    var total: Long = 0
    val nChars = cb.remaining
    while (maybeN.isDefined && total < nChars) {
      maybeN = fillCharBuffer(cb)
      if (maybeN.isDefined) total += maybeN.get.toLong
    }
    total == nChars
  }

  final def getString(nChars: Long): Maybe[String] = {
    withLocalCharBuffer { lcb =>
      val cb = lcb.getBuf(nChars)
      val gotAll = fillCharBufferLoop(cb)
      val res = if (!gotAll) Nope
      else Maybe(cb.flip.toString)
      res
    }
  }

  final def getSomeString(nChars: Long): Maybe[String] = {
    withLocalCharBuffer { lcb =>
      val cb = lcb.getBuf(nChars)
      fillCharBufferLoop(cb)
      if (cb.position() == 0) Nope
      else Maybe(cb.flip.toString)
      // TODO: Performance - we need to copy here. Consider return type of Maybe[StringBuilder]
      // as that will allow for non-copying trim and other manipulations of the string
      // without further copyies.
    }
  }

}
