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

package edu.illinois.ncsa.daffodil.processors.unparsers
import scala.collection.JavaConversions._
import edu.illinois.ncsa.daffodil.api._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors._

/**
 * Vast majority of unparsers (all?) are actually Term unparsers.
 */
abstract class TermUnparser(val termRuntimeData: TermRuntimeData) extends Unparser(termRuntimeData)

abstract class Unparser(val context: RuntimeData)
  extends Processor {

  def unparse(ustate: UState): Unit

  final def unparse1(ustate: UState, context: RuntimeData): Unit = {
    // TODO: Debugger hooks
    // Debugger.before(ustate, this)
    // 
    // Since the state is being overwritten (in most case) now,
    // we must explicitly make a copy so we can compute a delta
    // after
    //
    // val beforeState = if (Debugger.getDebugging()) ustate.duplicate() else ustate
    unparse(ustate)
    // Debugger.after(beforeState, ustate, this)
  }

  def UE(ustate: UState, s: String, args: Any*) = {
    UnparseError(One(context.schemaFileLocation), One(ustate), s, args: _*)
  }
}

// No-op, in case an optimization lets one of these sneak thru. 
// TODO: make this fail, and test optimizer sufficiently to know these 
// do NOT get through.
class EmptyGramUnparser(context: RuntimeData = null) extends Unparser(context) {
  def unparse(ustate: UState) {
    Assert.invariantFailed("EmptyGramUnparsers are all supposed to optimize out!")
  }
  override lazy val childProcessors = Nil

  override def toBriefXML(depthLimit: Int = -1) = "<empty/>"
  override def toString = toBriefXML()
}

class ErrorUnparser(context: RuntimeData = null) extends Unparser(context) {
  def unparse(ustate: UState) {
    Assert.abort("Error Unparser")
  }
  override lazy val childProcessors = Nil

  override def toBriefXML(depthLimit: Int = -1) = "<error/>"
  override def toString = "Error Unparser"
}

class SeqCompUnparser(context: RuntimeData, val childUnparsers: Seq[Unparser])
  extends Unparser(context)
  with ToBriefXMLImpl {

  override lazy val childProcessors = childUnparsers

  override def nom = "seq"

  def unparse(ustate: UState): Unit = {
    childUnparsers.foreach { unparser =>
      {
        unparser.unparse1(ustate, context)
      }
    }
  }

  override def toString: String = {
    val strings = childUnparsers map { _.toString }
    strings.mkString(" ~ ")
  }
}

case class DummyUnparser(primitiveName: String) extends Unparser(null) {

  def unparse(state: UState): Unit = state.SDE("Unparser (%s) is not yet implemented.", primitiveName)

  override lazy val childProcessors = Nil
  override def toBriefXML(depthLimit: Int = -1) = "<dummy primitive=\"%s\"/>".format(primitiveName)
  override def toString = "DummyUnparser (%s)".format(primitiveName)
}
//
//  /**
//   * For testing it is convenient to just hand it strings for data.
//   */
//  def createInitialState(rootElemDecl: GlobalElementDecl, data: String, document: org.jdom2.Document): UState = {
//    ???
//    /*
//    val out = Misc.stringToWritableByteChannel(data)
//    createInitialState(rootElemDecl, out, document, data.length)
//*/
//  }
//
//  /**
//   * Construct our OutStream object and initialize the state block.
//   */
//  def createInitialState(rootElemDecl: GlobalElementDecl, output: DFDL.Output, document: org.jdom2.Document, sizeHint: Long = -1): UState = {
//    ???
//    /*
//    val outStream =
//      if (sizeHint != -1) new OutStreamFromByteChannel(rootElemDecl, output, sizeHint)
//      else new OutStreamFromByteChannel(rootElemDecl, output)
//    createInitialState(rootElemDecl, outStream, document)
//*/
//  }
//}
//
///**
// * Encapsulates the I/O as an abstraction that works something like a java.nio.CharBuffer, but a
// * bit more specialized for DFDL needs.
// */
//trait OutStream {
//  def setEncoder(encoder: CharsetEncoder)
//  def write()
//  def charBufferToByteBuffer()
//
//  def getData(): String
//  def clearCharBuffer()
//  def fillCharBuffer(str: String)
//  def fillByteBuffer[T](num: T, name: String, order: java.nio.ByteOrder)
//}
//
///*
// * Not thread safe. We're depending on the CharBuffer being private to us.
// */
//class OutStreamFromByteChannel(context: ElementBase, outStream: DFDL.Output, sizeHint: Long = 1024 * 128, bufPos: Int = 0) extends OutStream with Logging { // 128K characters by default.
//  val maxCharacterWidthInBytes = 4 //FIXME: worst case. Ok for testing. Don't use this pessimistic technique for real data.
//  var cbuf = CharBuffer.allocate(maxCharacterWidthInBytes * sizeHint.toInt) // FIXME: all these Int length limits are too small for large data blobs
//  var bbuf = ByteBuffer.allocate(0)
//  var encoder: CharsetEncoder = null //FIXME
//  var charBufPos = bufPos //pointer to end of CharBuffer
//
//  def setEncoder(enc: CharsetEncoder) { encoder = enc }
//
//  /*
//   * Writes unparsed data in CharBuffer to outputStream.
//   */
//  def write() {
//    //if CharBuffer was used, move contents to ByteBuffer
//    //FIXME: CharBuffer overwrites ByteBuffer for mixed data
//    if (encoder != null) charBufferToByteBuffer()
//
//    outStream.write(bbuf)
//  }
//
//  /*
//   * Takes unparsed data from CharBuffer and encodes it in ByteBuffer
//   */
//  def charBufferToByteBuffer() {
//    bbuf = ByteBuffer.allocate(cbuf.length() * maxCharacterWidthInBytes)
//    encoder.reset()
//
//    val cr1 = encoder.encode(cbuf, bbuf, true) // true means this is all the input you get.
//    cbuf.clear() //remove old data from previous element
//    charBufPos = bufPos //reset pointer to end of CharBuffer
//
//    log(LogLevel.Debug, "Encode Error1: " + cr1.toString())
//    if (cr1 != CoderResult.UNDERFLOW) {
//      if (cr1 == CoderResult.OVERFLOW) {
//        // it's ok. It just means we've satisfied the byte buffer.
//      } //else
//      // Assert.abort("Something went wrong while encoding characters: CoderResult = " + cr1)   
//    }
//    val cr2 = encoder.flush(bbuf)
//    log(LogLevel.Debug, "Encode Error2: " + cr2.toString())
//    if (cr2 != CoderResult.UNDERFLOW) {
//      // Something went wrong
//      // Assert.abort("Something went wrong while encoding characters: CoderResult = " + cr2) 
//      // FIXME: proper handling of errors. Some of which are 
//      // to be suppressed, other converted, others skipped, etc. 
//    }
//    encoder = null
//    bbuf.flip() // so the caller can read
//  }
//
//  def getData(): String = {
//    cbuf.toString
//  }
//
//  def clearCharBuffer() {
//    cbuf.clear()
//    charBufPos = 0
//  }
//
//  /*
//   * Moves data to CharBuffer, resizing as necessary.
//   */
//  def fillCharBuffer(str: String) {
//    var isTooSmall = true
//    val temp =
//      if (charBufPos != 0) cbuf.toString()
//      else ""
//
//    while (isTooSmall) {
//      try { //writing all data to char buffer
//        cbuf.position(charBufPos)
//        cbuf.put(str, 0, str.length())
//        charBufPos = cbuf.position()
//
//        cbuf.flip()
//        isTooSmall = false
//      } catch { //make sure buffer was not written to capacity
//        case u: UnsuppressableException => throw u
//        case e: Exception => {
//          cbuf = CharBuffer.allocate(cbuf.position() * 4) // TODO: more efficient algorithm than size x4
//          if (temp != "")
//            cbuf.put(temp)
//        }
//      }
//    }
//  }
//
//  /*
//   * Moves data to ByteBuffer by data's type and byte order.
//   */
//  def fillByteBuffer[T](num: T, name: String, order: java.nio.ByteOrder) {
//
//    name match {
//      case "byte" | "unsignedByte" => {
//        bbuf = ByteBuffer.allocate(1)
//        bbuf.order(order)
//        bbuf.put(num.asInstanceOf[Byte])
//      }
//      case "short" | "unsignedShort" => {
//        bbuf = ByteBuffer.allocate(2)
//        bbuf.order(order)
//        bbuf.putShort(num.asInstanceOf[Short])
//      }
//      case "int" | "unsignedInt" => {
//        bbuf = ByteBuffer.allocate(4)
//        bbuf.order(order)
//        bbuf.putInt(num.asInstanceOf[Int])
//      }
//      case "long" | "unsignedLong" => {
//        bbuf = ByteBuffer.allocate(8)
//        bbuf.order(order)
//        bbuf.putLong(num.asInstanceOf[Long])
//      }
//      case "double" => {
//        bbuf = ByteBuffer.allocate(8)
//        bbuf.order(order)
//        bbuf.putDouble(num.asInstanceOf[Double])
//      }
//      case "float" => {
//        bbuf = ByteBuffer.allocate(4)
//        bbuf.order(order)
//        bbuf.putFloat(num.asInstanceOf[Float])
//      }
//      case "hexBinary" => {
//        val bytes = num.asInstanceOf[Array[Byte]]
//        bbuf = ByteBuffer.allocate(bytes.length)
//        bbuf.order(order)
//        for (i <- 0 until bytes.length) {
//          bbuf.put(bytes(i))
//        }
//      }
//      case _ => {
//        Assert.notYetImplemented()
//      }
//    }
//    bbuf.flip() //so bbuf can be read
//  }
//}

