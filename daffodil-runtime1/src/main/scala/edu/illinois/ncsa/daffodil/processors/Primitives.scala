package edu.illinois.ncsa.daffodil.processors

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import java.util.regex.Pattern
import java.nio.{ CharBuffer, ByteBuffer }
import java.nio.charset.Charset
import java.nio.charset.MalformedInputException
import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.{ YesNo, LengthKind, ByteOrder, LengthUnits, AlignmentUnits }
import edu.illinois.ncsa.daffodil.util.{ Debug, LogLevel, Logging, Info }
import edu.illinois.ncsa.daffodil.util.Misc.bytes2Hex
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import edu.illinois.ncsa.daffodil.grammar.Terminal
import scala.util.parsing.input.{ Reader }
import java.sql.Timestamp
import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextTrimKind
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextStringJustification
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextNumberJustification
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextCalendarJustification
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TextBooleanJustification
import edu.illinois.ncsa.daffodil.processors.{ Parser => DaffodilParser }
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.schema.annotation.props.AlignmentType
import scala.xml.Node
import edu.illinois.ncsa.daffodil.api._
import scala.collection.JavaConversions._
import edu.illinois.ncsa.daffodil.processors.xpath.DFDLCheckConstraintsFunction
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.processors.dfa.TextPaddingParser

/**
 * *
 * ChoiceElementBegin allows us to get away without adding the additional
 * choice element required of the unordered sequence transformation.
 *
 * This prevents us from having to worry about navigating around additional
 * choice elements in the infoset.  We effectively don't add them if they
 * were constructed as a result of an unordered sequence.
 */
case class ChoiceElementBegin(e: ElementBase) extends Terminal(e, true) {

  val isHidden = e.isHidden

  def parser: DaffodilParser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit: Int = -1): String = {
      "<ChoiceElementBegin name='" + e.name + "'/>"
    }

    /**
     * ElementBegin just adds the element we are constructing to the infoset and changes
     * the state to be referring to this new element as what we're parsing data into.
     */
    def parse(start: PState): PState = {
      val currentElement = Infoset.newElement(e, isHidden)

      log(LogLevel.Debug, "currentElement = %s", currentElement)
      start
    }
  }

  def unparser = new DummyUnparser(e)
}

case class ElementBegin(e: ElementBase) extends Terminal(e, true) {

  val isHidden = e.isHidden

  def parser: DaffodilParser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit: Int = -1): String = {
      "<ElementBegin name='" + e.name + "'/>"
    }

    /**
     * ElementBegin just adds the element we are constructing to the infoset and changes
     * the state to be referring to this new element as what we're parsing data into.
     */
    def parse(start: PState): PState = {
      val currentElement = Infoset.newElement(e, isHidden)

      log(LogLevel.Debug, "currentElement = %s", currentElement)
      val priorElement = start.infoset
      priorElement.addElement(currentElement)
      log(LogLevel.Debug, "priorElement = %s", priorElement)
      val postState = start.withParent(currentElement)
      postState
    }
  }

  def unparser = new DummyUnparser(e)
  //  def unparser: Unparser = new Unparser(e) {
  //    override def toString = "<" + e.name + ">"
  //
  //    /**
  //     * Changes the state to refer to the next element in the infoset as the element to unparse.
  //     */
  //    def unparse(start: UState): UState = {
  //      val nextElement = {
  //        //
  //        // TODO FIXME: THis can't be correct. The elementBegin shouldn't be writing out element contents.
  //        // That should happen in content unparsers. This unparser should just set things up so content
  //        // unparsers see the correct element to take the contents of. Which really means just changing the 
  //        // parent pointer in the UState.
  //        //
  //        // TODO: should not try/catch - should return a failed UState on error
  //        try { //if content contains elements
  //          if (!start.childIndexStack.isEmpty) {
  //            if (start.childPos != 1) { //if not first child, write unparsed result of previous child to outputStream
  //              //              val encoder = e.knownEncodingEncoder
  //              //              start.outStream.setEncoder(encoder)
  //              start.outStream.write()
  //            }
  //            start.currentElement.getContent().get(start.childPos.asInstanceOf[Int] - 1).asInstanceOf[org.jdom2.Element]
  //          } else {
  //            //            val encoder = e.knownEncodingEncoder
  //            //            start.outStream.setEncoder(encoder)
  //            start.currentElement.getContent().get(0).asInstanceOf[org.jdom2.Element]
  //          }
  //        } catch {
  //          case u: UnsuppressableException => throw u
  //          case e: Exception => start.currentElement //if content is text
  //        }
  //      }
  //
  //      start.withCurrent(nextElement).moveOverByOneElement
  //    }
  //  }
}

abstract class ElementEndBase(e: ElementBase) extends Terminal(e, true) {
  def toPrettyString = "</" + e.name + prettyStringModifier + ">"
  def prettyStringModifier: String

  def move(pstate: PState): PState // implement for different kinds of "moving over to next thing"
  def kindString = "ElementEnd"

  def validate(pstate: PState): PState = {
    val currentElement = pstate.parentElement
    val resultState = DFDLCheckConstraintsFunction.validate(pstate) match {
      case Right(boolVal) => {
        log(LogLevel.Debug, "Validation succeeded for %s", currentElement.toBriefXML)
        pstate // Success, do not mutate state.
      }
      case Left(failureMessage) => {
        log(LogLevel.Debug,
          "Validation failed for %s due to %s. The element value was %s.",
          e.toString, failureMessage, currentElement.toBriefXML)
        pstate.withValidationError("%s failed dfdl:checkConstraints due to %s",
          e.toString, failureMessage)
      }
    }

    resultState
  }

  def localParse(start: PState): PState = {
    val currentElement = start.parentElement

    val shouldValidate = start.dataProc.getValidationMode != ValidationMode.Off
    val postValidate =
      if (shouldValidate && e.isSimpleType) {
        // Execute checkConstraints
        val resultState = validate(start)
        resultState
      } else start

    // Assert.invariant(currentElement.getName() != "_document_" )
    val priorElement = currentElement.parent
    log(LogLevel.Debug, "priorElement = %s", priorElement)
    val postState = move(postValidate.withParent(priorElement))
    postState
  }

  def parser: DaffodilParser = new PrimParser(this, e) {

    override def toBriefXML(depthLimit: Int = -1): String = {
      "<" + kindString + " name='" + e.name + "'/>"
    }

    override def toString = toPrettyString

    /**
     * ElementEnd just moves back to the parent element of the current one.
     */
    def parse(start: PState): PState = localParse(start)
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = "</" + e.name + ">"

    /**
     * Changes state to refer to parent element of the current one.
     */
    def unparse(start: UState): UState = {
      val postState = {
        if (start.currentElement.getName != start.rootName) {
          val parent = start.currentElement.getParentElement()
          val state = start.withCurrent(parent)
          state
        } else {
          start
        }
      }
      postState
    }
  }
}

/**
 * *
 * ChoiceElementEnd allows us to get away without adding the additional
 * choice element required of the unordered sequence transformation.
 *
 * This prevents us from having to worry about navigating around additional
 * choice elements in the infoset.  We effectively don't add them if they
 * were constructed as a result of an unordered sequence.
 */
case class ChoiceElementEnd(e: ElementBase) extends ElementEndBase(e) {
  def move(pstate: PState) = pstate
  def prettyStringModifier = ""
  override def kindString = "ChoiceElementEnd"

  // We don't want to modify the state here except
  // for validation.
  override def localParse(start: PState): PState = {
    val currentElement = start.parentElement

    val shouldValidate = start.dataProc.getValidationMode != ValidationMode.Off
    val postValidate =
      if (shouldValidate && e.isSimpleType) {
        // Execute checkConstraints
        val resultState = validate(start)
        resultState
      } else start

    val priorElement = currentElement.parent
    log(LogLevel.Debug, "priorElement = %s", priorElement)
    val postState = move(postValidate)
    postState
  }
}

case class ElementEnd(e: ElementBase) extends ElementEndBase(e) {
  def move(pstate: PState) = pstate.moveOverByOneElement
  def prettyStringModifier = ""
}

case class ElementEndNoRep(e: ElementBase) extends ElementEndBase(e) {
  // if there is no rep (inputValueCalc), then we do create a new child so that index must advance,
  // but we don't create anything new as far as the group is concerned, and we don't want 
  // the group 'thinking' that there's a prior sibling inside the group and placing a 
  // separator after it. So in the case of NoRep, we don't advance group child, just element child.
  def move(pstate: PState) = pstate.moveOverOneElementChildOnly
  def prettyStringModifier = "(NoRep)"
}

//case class ComplexElementEndPattern(e: ElementBase) extends Terminal(e, e.isComplexType == true && e.lengthKind == LengthKind.Pattern) {
//  // TODO: Should this be more generic; is there a way to detect state from the current element to tell us if it's time
//  //       to pop the input stack?
//
//  def parser: DaffodilParser = new PrimParser(this, e) {
//    override def toString = "</" + e.name + " dfdl:lengthKind='pattern'>"
//
//    /**
//     * ElementEnd just moves back to the parent element of the current one.
//     */
//    def parse(start: PState): PState = {
//      val currentElement = start.parentElement
//      log(LogLevel.Debug, "currentElement = %s", currentElement))
//      var priorElement = currentElement.parent
//      log(LogLevel.Debug, "priorElement = %s", priorElement))
//      val postState = start.withParent(priorElement).moveOverByOneElement.withLastInStream()
//      postState
//    }
//  }
//
//  def unparser: Unparser = new DummyUnparser(e)
//}

///**
// * The I/O layer should be written to use Java's NIO Channels, and Direct ByteBuffers for file I/O. This is the
// * fastest stuff in the Java stack.
// *
// * The basic design for known-length parsing
// * is to do a bounds check on whether we have enough buffer space to accomplish the reading of data
// * within the buffer. If not, repositioning/refreshing the buffer. If so, then we want to issue a
// * single block read per element which both reads and converts to the right character encoding, or
// * directly to the right type (float, long, double, etc) when the result type is a primitive.
// *
// * For types with more complexity, (binary dates for example) then the previous would still be done
// * followed by a conversion of some sort.
// */
//
//case class StringFixedLengthInBytes(e: ElementBase, nBytes: Long)
//  extends Terminal(e, true)
//  with TextReader
//  with Padded
//  with WithParseErrorThrowing {
//
//  val charset = e.knownEncodingCharset
//
//  def parser: DaffodilParser = new PrimParser(this, e) {
//
//    override def toBriefXML(depthLimit: Int = -1) = {
//      "<StringFixedLengthInBytesParser length='" + nBytes + "'/>"
//    }
//
//    override def toString = toBriefXML()
//
//    val codepointWidth = e.knownEncodingWidthInBits
//    Assert.invariant(codepointWidth != -1)
//
//    def parse(start: PState): PState = withParseErrorThrowing(start) {
//      // withLoggingLevel(LogLevel.Info) 
//      {
//
//        log(LogLevel.Debug, "StringFixedLengthInBytes - Parsing starting at bit position: %s", start.bitPos))
//
//        // if (start.bitPos % 8 != 0) { return PE(start, "StringFixedLengthInBytes - not byte aligned.") }
//
//        val in = start.inStream
//
//        val bytePos = (start.bitPos >> 3).toInt
//
//        val decoder = charset.newDecoder()
//
//        val d = new DelimParser(e.knownEncodingStringBitLengthFunction)
//
//        try {
//          //
//          // nBytes can be a little bit longer that the decoder actually consumes
//          // because some characters are less than a byte wide (7-bit ascii packed variant)
//          //
//          // Note: This code goes around the DFDLCharReader layer because that layer doesn't provide
//          // us a way to limit the number of bytes. (Perhaps that could be changed so this could use it?)
//          //
//
//          val reader = getReader(charset, start.bitPos, start)
//          val bytes = in.getBytes(start.bitPos, nBytes.toInt)
//          decoder.reset()
//          val cb = decoder.decode(ByteBuffer.wrap(bytes))
//          val result = cb.toString()
//          val endBitPos = start.bitPos + (result.length * codepointWidth) // handles 7-bit or wider chars
//          log(LogLevel.Debug, "Parsed: %s", result))
//          log(LogLevel.Debug, "Ended at bit position %s", endBitPos))
//          val endCharPos = start.charPos + result.length
//          val currentElement = start.parentElement
//          val trimmedResult = d.removePadding(result, justificationTrim, padChar)
//          // Assert.invariant(currentElement.getName != "_document_")
//          // Note: this side effect is backtracked, because at points of uncertainty, pre-copies of a node are made
//          // and when backtracking occurs they are used to replace the nodes modified by sub-parsers.
//          currentElement.setDataValue(trimmedResult)
//          // 
//          // if the number of bytes was a multiple of the codepointWidth then 
//          // we will have parsed all the bytes, so the endBitPos and endCharPos 
//          // are synchronized still. 
//          // 
//          val postState =
//            if ((endBitPos - start.bitPos) == (8 * nBytes)) {
//              start.withPos(endBitPos, endCharPos, Some(reader))
//            } else {
//              Assert.invariant((endBitPos - start.bitPos) < (8 * nBytes))
//              start.withPos(endBitPos, -1, None)
//              // -1 means a subsequent primitive will have to construct
//              // a new reader at said bitPosition              
//            }
//          return postState
//        } catch {
//          case m: java.nio.charset.MalformedInputException => { return PE(start, "StringFixedLengthInBytes - Malformed data. Could not decode into %s characters.", charset.name()) }
//          case e: IndexOutOfBoundsException => { return PE(start, "StringFixedLengthInBytes - Insufficient Bits in field: IndexOutOfBounds: \n%s", e.getMessage()) }
//          case u: UnsuppressableException => throw u
//          // case e: Exception => { return PE(start, "StringFixedLengthInBytes - Exception: \n%s", e.getMessage()) }
//        }
//      }
//    }
//  }
//
//  def unparser: Unparser = new Unparser(e) {
//    override def toString = "StringFixedLengthInBytesUnparser(" + nBytes + ")"
//
//    def unparse(start: UState): UState = {
//      // setLoggingLevel(LogLevel.Info)
//
//      val data = start.currentElement.getText
//
//      val encoder = charset.newEncoder()
//      start.outStream.setEncoder(encoder)
//      start.outStream.fillCharBuffer(data)
//
//      log(LogLevel.Debug, "Unparsed: " + start.outStream.getData))
//      start
//    }
//  }
//}
//

abstract class Primitive(e: AnnotatedSchemaComponent, guard: Boolean = false)
  extends Terminal(e, guard) {
  override def toString = "Prim[" + name + "]"
  def parser: DaffodilParser = DummyParser(e)
  def unparser: Unparser = DummyUnparser(e)

}

abstract class DelimParserBase(e: Term, guard: Boolean) extends Terminal(e, guard) {
  override def toString = "DelimParserBase[" + name + "]"
  val dp = new DFDLDelimParser(e.knownEncodingStringBitLengthFunction)

  private def isPrefixOf(possiblePrefix: String, string: String): Boolean = {
    string.startsWith(possiblePrefix)
  }

  private def checkPadCharDistinctness(
    padChar: Option[String],
    escChar: Option[String],
    escEscChar: Option[String],
    escBlockStart: Option[String],
    escBlockEnd: Option[String],
    terminatingMarkup: Seq[String],
    context: ThrowsSDE): Unit = {

    padChar.foreach(pc => {
      escChar.foreach(ec => {
        if (pc == ec || isPrefixOf(pc, ec))
          context.SDE("The pad character (%s) cannot be the same as (or a prefix of) the escape character.", pc)
      })
      escEscChar.foreach(eec => {
        if (pc == eec || isPrefixOf(pc, eec))
          context.SDE("The pad character (%s) cannot be the same as (or a prefix of) the escape escape character.", pc)
      })
      escBlockStart.foreach(ebs => {
        if (pc == ebs || isPrefixOf(pc, ebs))
          context.SDE("The pad character (%s) cannot be the same as (or a prefix of) the escape block start.", pc)
      })
      escBlockEnd.foreach(ebe => {
        if (pc == ebe || isPrefixOf(pc, ebe))
          context.SDE("The pad character (%s) cannot be the same as (or a prefix of) the escape block end.", pc)
      })
      terminatingMarkup.foreach(tm => {
        if (tm == pc || isPrefixOf(pc, tm)) {
          context.SDE("The pad character (%s) cannot be the same as (or a prefix of) a piece of the terminating markup (%s).", pc, tm)
        }
      })
    })
  }

  private def checkEscCharDistinctness(
    escChar: Option[String],
    terminatingMarkup: Seq[String],
    context: ThrowsSDE): Unit = {

    escChar.foreach(ec =>
      terminatingMarkup.foreach(tm => {
        if (tm == ec || isPrefixOf(ec, tm)) {
          context.SDE("The escapeCharacter (%s) cannot be the same as (or a prefix of) a piece of the terminating markup (%s).", ec, tm)
        }
      }))
  }

  private def checkEscEscCharDistinctness(
    escEscChar: Option[String],
    escBlockStart: Option[String],
    escBlockEnd: Option[String],
    terminatingMarkup: Seq[String],
    context: ThrowsSDE): Unit = {

    escEscChar.foreach(eec => {
      // GeneralPurposeFormat seems to disagree with the below, commenting out for now.
      //        escBlockStart match {
      //          case Some(ebs) if eec == ebs || isPrefixOf(eec, ebs) =>
      //            context.SDE("The escapeEscapeCharacter (%s) cannot be the same as (or a prefix of) the escape block start.",eec)
      //          case _ => // Nothing to do
      //        }
      //        escBlockEnd match {
      //          case Some(ebe) if eec == ebe || isPrefixOf(eec, ebe) =>
      //            context.SDE("The escapeEscapeCharacter (%s) cannot be the same as (or a prefix of) the escape block end.",eec)
      //          case _ => // Nothing to do
      //        }
      terminatingMarkup.foreach(tm => {
        if (tm == eec || isPrefixOf(eec, tm)) {
          context.SDE("The escapeEscapeCharacter (%s) cannot be the same as (or a prefix of) a piece of the terminating markup (%s).", eec, tm)
        }
      })
    })
  }

  private def checkEscapeBlockDistinctness(
    escBlockStart: Option[String],
    escBlockEnd: Option[String],
    terminatingMarkup: Seq[String],
    context: ThrowsSDE): Unit = {

    escBlockStart.foreach(ebs => terminatingMarkup.foreach(tm => {
      if (tm == ebs || isPrefixOf(ebs, tm)) {
        context.SDE("The escapeBlockStart (%s) cannot be the same as (or a prefix of) a piece of the terminating markup.", ebs)
      }
    }))

    escBlockEnd.foreach(ebe => terminatingMarkup.foreach(tm => {
      if (tm == ebe || isPrefixOf(ebe, tm)) {
        context.SDE("The escapeBlockEnd (%s) cannot be the same as (or a prefix of) a piece of the terminating markup.", ebe)
      }
    }))
  }

  private def checkDelimiterDistinctness_(
    padChar: Option[String],
    escChar: Option[String],
    escEscChar: Option[String],
    escBlockStart: Option[String],
    escBlockEnd: Option[String],
    terminatingMarkup: Seq[String],
    context: ThrowsSDE): Unit = {

    checkPadCharDistinctness(padChar, escChar, escEscChar, escBlockStart, escBlockEnd, terminatingMarkup, context)
    checkEscCharDistinctness(escChar, terminatingMarkup, context)
    checkEscEscCharDistinctness(escEscChar, escBlockStart, escBlockEnd, terminatingMarkup, context)
    checkEscapeBlockDistinctness(escBlockStart, escBlockEnd, terminatingMarkup, context)
  }

  def checkDelimiterDistinctness(
    escapeSchemeKind: EscapeSchemeKind.Type,
    optPadChar: Option[String],
    optEscChar: Option[String], // Could be a DFDL expression
    optEscEscChar: Option[String], // Could be a DFDL expression
    optEscBlkStart: Option[String],
    optEscBlkEnd: Option[String],
    terminatingMarkup: Seq[String],
    context: ThrowsSDE): Unit = {

    // TODO: DFDL-451 - After conversing with Mike B. about this, we're putting this on the backburner.
    // Leaving the code here, just commented out the entry point until we can decide what is the appropriate
    // behavior here.
    //
    //    escapeSchemeKind match {
    //      case EscapeSchemeKind.None =>
    //        checkDelimiterDistinctness_(optPadChar, None, None, None, None, terminatingMarkup, context)
    //      case EscapeSchemeKind.Character =>
    //        checkDelimiterDistinctness_(optPadChar, optEscChar, optEscEscChar, None, None, terminatingMarkup, context)
    //      case EscapeSchemeKind.Block =>
    //        checkDelimiterDistinctness_(optPadChar, None, optEscEscChar,
    //          optEscBlkStart, optEscBlkEnd, terminatingMarkup, context)
    //    }
  }
}

abstract class ZonedTextNumberPrim(e: ElementBase, guard: Boolean) extends Terminal(e, guard) {
  def parser: DaffodilParser = new PrimParser(this, e) {
    def parse(start: PState): PState = {
      Assert.notYetImplemented()
    }
  }

  def unparser: Unparser = new Unparser(e) {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}
case class ZonedTextBytePrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextShortPrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextIntPrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextLongPrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)

trait RuntimeExplicitLengthMixin[T] {
  self: Terminal =>
  def e: ElementBase

  // get at compile time, not runtime.
  val lUnits = e.lengthUnits

  // binary numbers will use this conversion. Others won't.
  lazy val toBits = lUnits match {
    case LengthUnits.Bits => 1
    case LengthUnits.Bytes => 8
    case _ => e.schemaDefinitionError("Binary Numbers must have length units of Bits or Bytes.")
  }

  def getBitLength(s: PState): (PState, Long) = {
    val R(nBytesAsAny, newVMap) = e.length.evaluate(s.parentElement, s.variableMap, s)
    val nBytes = nBytesAsAny.asInstanceOf[Long]
    val start = s.withVariables(newVMap)
    (start, nBytes * toBits)
  }
  def getLength(s: PState): (PState, Long) = {
    val R(nBytesAsAny, newVMap) = e.length.evaluate(s.parentElement, s.variableMap, s)
    val nBytes = nBytesAsAny.asInstanceOf[Long]
    val start = s.withVariables(newVMap)
    (start, nBytes)
  }
}

trait KnownLengthInBitsMixin[T] {
  self: BinaryNumberBase[T] =>
  def len: Long
  def getBitLength(s: PState) = (s, len) // already in bits, so no multiply by 8 for this one.
}

trait RuntimeExplicitByteOrderMixin[T] {
  self: BinaryNumberBase[T] =>
  def e: ElementBase
  def getByteOrder(s: PState): (PState, java.nio.ByteOrder) = {
    val R(byteOrderAsAny, newVMap) = e.byteOrder.evaluate(s.parentElement, s.variableMap, s)
    val dfdlByteOrderEnum = ByteOrder(byteOrderAsAny.toString, e)
    val byteOrder = dfdlByteOrderEnum match {
      case ByteOrder.BigEndian => java.nio.ByteOrder.BIG_ENDIAN
      case ByteOrder.LittleEndian => java.nio.ByteOrder.LITTLE_ENDIAN
    }
    val start = s.withVariables(newVMap)
    (start, byteOrder)
  }
}

trait SignedNumberMixin[T] {
  self: BinaryNumberBase[T] =>
  def convertValue(n: BigInt, msb: Int): T = {
    val signed = n.testBit(msb - 1) match { // msb is zero-based bit counting
      case true => n - (BigInt(1) << msb)
      case false => n
    }
    signed.asInstanceOf[T]
  }
}

trait UnsignedNumberMixin[T] {
  self: BinaryNumberBase[T] =>
  def convertValue(n: BigInt, msb: Int): T = n.asInstanceOf[T]
}

// TODO: Double Conversion as a Sign-Trait

abstract class BinaryNumberBase[T](val e: ElementBase) extends Terminal(e, true) {
  val primName = e.primType.name

  val (staticJByteOrder, label) = {
    if (e.byteOrder.isConstant) {
      val staticByteOrderString = e.byteOrder.constantAsString
      val staticByteOrder = ByteOrder(staticByteOrderString, context)
      staticByteOrder match {
        case ByteOrder.BigEndian => (java.nio.ByteOrder.BIG_ENDIAN, "BE")
        case ByteOrder.LittleEndian => (java.nio.ByteOrder.LITTLE_ENDIAN, "LE")
      }
    } else (null, "Runtime")
  }

  //def getNum(t: Number): BigInt
  protected def getBitLength(s: PState): (PState, Long)
  protected def getByteOrder(s: PState): (PState, java.nio.ByteOrder)
  protected def convertValue(n: BigInt, msb: Int): T
  override def toString = "binary(xs:" + primName + ", " + label + ")"
  val gram = this

  protected val GramName = e.primType.name
  protected val GramDescription = { GramName(0).toUpper + GramName.substring(1, GramName.length) }

  def parser = new PrimParser(this, e) {
    override def toString = gram.toString

    def parse(start0: PState): PState = withParseErrorThrowing(start0) {
      try {
        val (start1, nBits) = getBitLength(start0)
        val (start, bo) = getByteOrder(start1)

        //if (start.bitLimit != -1L && (start.bitLimit - start.bitPos < nBits)) PE(start, "Not enough bits to create an xs:" + primName)

        val (value, newPos) = start.inStream.getBitSequence(start.bitPos, nBits, bo)
        //if (GramName == "hexBinary") {
        //  val bytes = value.asInstanceOf[Array[Byte]]
        //  var asString: StringBuilder = new StringBuilder()
        //  for (i <- 0 until bytes.length) {
        //    val byte = String.format("%02X", bytes(i).asInstanceOf[java.lang.Byte])
        //    asString.append(byte)
        //  }
        //  start.parentElement.setDataValue(asString.toString())
        //} else
        val convertedValue: T = convertValue(value, nBits.toInt)
        start.parentElement.setDataValue(convertValueToString(convertedValue))
        start.withPos(newPos, -1, None)
      } catch {
        case e: IndexOutOfBoundsException => { return PE(start0, "BinaryNumber - Insufficient Bits for xs:%s : IndexOutOfBounds: \n%s", primName, e.getMessage()) }
        case u: UnsuppressableException => throw u
        case e: Exception => { return PE(start0, "BinaryNumber - Exception: \n%s", e) }
      }
    }
  }

  def convertValueToString(n: T): String = {
    n.toString
  }

  def unparser = DummyUnparser
}

class UnsignedRuntimeLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase) extends BinaryNumberBase[T](e)
  with RuntimeExplicitLengthMixin[T] with RuntimeExplicitByteOrderMixin[T] with UnsignedNumberMixin[T] {
}

class UnsignedKnownLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase, val len: Long) extends BinaryNumberBase[T](e)
  with RuntimeExplicitByteOrderMixin[T] with KnownLengthInBitsMixin[T] with UnsignedNumberMixin[T] {
}

class SignedRuntimeLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase) extends BinaryNumberBase[T](e)
  with RuntimeExplicitLengthMixin[T] with RuntimeExplicitByteOrderMixin[T] with SignedNumberMixin[T] {
}

class SignedKnownLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase, val len: Long) extends BinaryNumberBase[T](e)
  with RuntimeExplicitByteOrderMixin[T] with KnownLengthInBitsMixin[T] with SignedNumberMixin[T] {
}

// Not needed. No runtime-determined lengths for binary floats.
//class FloatingPointRuntimeLengthRuntimeByteOrderBinaryNumber[T](e: ElementBase) extends BinaryNumberBase[T](e)
//  with RuntimeExplicitLengthMixin[T] with RuntimeExplicitByteOrderMixin[T] with FloatingPointMixin[T] {
//}

class HexBinaryKnownLengthBinaryNumber(e: ElementBase, val len: Long)
  extends BinaryNumberBase[String](e) {
  override def toString = "hexBinary(xs:" + primName + ", " + label + ")"
  // get at compile time, not runtime.
  val lUnits = e.lengthUnits

  // binary numbers will use this conversion. Others won't.
  lazy val toBits = lUnits match {
    case LengthUnits.Bits => 1
    case LengthUnits.Bytes => 8
    case _ => e.schemaDefinitionError("Binary Numbers must have length units of Bits or Bytes.")
  }

  def getByteOrder(s: PState): (PState, java.nio.ByteOrder) = {
    (s, java.nio.ByteOrder.BIG_ENDIAN)
  }

  def getBitLength(s: PState): (PState, Long) = {
    (s, len * toBits)
  }
  def getLength(s: PState): (PState, Long) = {
    (s, len)
  }

  final def convertValue(n: BigInt, ignored_msb: Int): String = n.toString(16)
}

class HexBinaryRuntimeLengthBinaryNumber(e: ElementBase)
  extends BinaryNumberBase[String](e)
  with RuntimeExplicitLengthMixin[String] {

  override def toString = "hexBinary(xs:" + primName + ", " + label + ")"
  def getByteOrder(s: PState): (PState, java.nio.ByteOrder) = {
    (s, java.nio.ByteOrder.BIG_ENDIAN)
  }

  final def convertValue(n: BigInt, ignored_msb: Int): String = n.toString(16)
}

class FloatKnownLengthRuntimeByteOrderBinaryNumber(e: ElementBase, val len: Long)
  extends BinaryNumberBase[Float](e)
  with RuntimeExplicitByteOrderMixin[Float]
  with KnownLengthInBitsMixin[Float] {

  final def convertValue(n: BigInt, ignored_msb: Int): Float = {
    val nWith33rdBit = n | (BigInt(1) << 33) // make sure we have 5 bytes here. Then we'll ignore 5th byte.
    val ba = nWith33rdBit.toByteArray
    val bb = java.nio.ByteBuffer.wrap(ba)
    val res = ba.length match {
      case 5 => bb.getFloat(1)
      case _ => Assert.invariantFailed("byte array should be 5 long")
    }
    res
  }
}

class DoubleKnownLengthRuntimeByteOrderBinaryNumber(e: ElementBase, val len: Long)
  extends BinaryNumberBase[Double](e)
  with RuntimeExplicitByteOrderMixin[Double]
  with KnownLengthInBitsMixin[Double] {

  final def convertValue(n: BigInt, ignored_msb: Int): Double = {
    val nWith65thBit = n | (BigInt(1) << 65) // make sure we have 9 bytes of bigint here. Then we'll ignore the 9th byte.
    val ba = nWith65thBit.toByteArray
    val bb = java.nio.ByteBuffer.wrap(ba)
    val res = ba.length match {
      case 9 => bb.getDouble(1) // ignore first byte.
      case _ => Assert.invariantFailed("byte array should be 9 long")
    }
    res
  }
}

class DecimalKnownLengthRuntimeByteOrderBinaryNumber(e: ElementBase, val len: Long)
  extends BinaryNumberBase[BigDecimal](e)
  with RuntimeExplicitByteOrderMixin[BigDecimal]
  with KnownLengthInBitsMixin[BigDecimal] {

  final def convertValue(n: BigInt, ignored_msb: Int): BigDecimal = {
    val res = BigDecimal(n, e.binaryDecimalVirtualPoint)
    res
  }

  override def convertValueToString(n: BigDecimal): String = {
    n.underlying.toPlainString
  }
}

case class PackedIntPrim(e: ElementBase) extends Primitive(e, false)
case class BCDIntPrim(e: ElementBase) extends Primitive(e, false)

case class StartChildren(ct: ComplexTypeBase, guard: Boolean = true) extends Terminal(ct.element, guard) {

  def parser: DaffodilParser = new PrimParser(this, ct.element) {
    override def toString = "StartChildren"

    def parse(start: PState): PState = {
      val postState = start.withChildIndexStack(1L :: start.childIndexStack)
      postState
    }
  }

  def unparser: Unparser = new Unparser(ct.element) {
    override def toString = "StartChildren"

    def unparse(start: UState): UState = {
      val postState = start.withChildIndexStack(1L :: start.childIndexStack)
      postState
    }
  }
}

case class StartSequence(sq: Sequence, guard: Boolean = true) extends Terminal(sq, guard) {

  def parser: DaffodilParser = new PrimParser(this, sq) {
    override def toString = "StartSequence"

    def parse(start: PState): PState = {
      val postState = start.withGroupIndexStack(1L :: start.groupIndexStack)
      postState
    }
  }

  def unparser: Unparser = new Unparser(sq) {
    override def toString = "StartSequence"

    def unparse(start: UState): UState = {
      val postState = start.withGroupIndexStack(1L :: start.groupIndexStack)
      postState
    }
  }
}

class OptionalInfixSep(term: Term, sep: => Gram, guard: Boolean = true) extends Terminal(term, guard) {

  val sepParser = sep.parser

  def parser: DaffodilParser = new PrimParser(this, term) {

    override def toString = "<OptionalInfixSep>" + sepParser.toString() + "</OptionalInfixSep>"

    def parse(start: PState): PState = {
      if (start.arrayPos > 1) sepParser.parse1(start, term)
      else if (start.groupPos > 1) sepParser.parse1(start, term)
      else start
    }
  }

  def unparser = DummyUnparser(term)
}

case class EndChildren(ct: ComplexTypeBase, guard: Boolean = true) extends Terminal(ct.element, guard) {

  def parser: DaffodilParser = new PrimParser(this, ct.element) {
    override def toString = "EndChildren"

    def parse(start: PState): PState = {
      val postState = start.withChildIndexStack(start.childIndexStack.tail)
      postState
    }
  }

  def unparser: Unparser = new Unparser(ct.element) {
    override def toString = "EndChildren"

    def unparse(start: UState): UState = {
      val postState = start.withChildIndexStack(start.childIndexStack.tail)
      postState
    }
  }
}

case class EndSequence(sq: Sequence, guard: Boolean = true) extends Terminal(sq, guard) {

  def parser: DaffodilParser = new PrimParser(this, sq) {
    override def toString = "EndSequence"

    def parse(start: PState): PState = {
      // When we end a sequence group, we have created a group child in the parent
      // so we advance that index. 
      val postState = start.withGroupIndexStack(start.groupIndexStack.tail).moveOverOneGroupIndexOnly
      postState
    }
  }

  def unparser: Unparser = new Unparser(sq) {
    override def toString = "EndSequence"

    def unparse(start: UState): UState = {
      val postState = start.withGroupIndexStack(start.groupIndexStack.tail).moveOverOneGroupIndexOnly
      postState
    }
  }
}

case class StartArray(e: ElementBase, guard: Boolean = true) extends Terminal(e, guard) {

  def parser: DaffodilParser = new PrimParser(this, e) {
    override def toString = "StartArray"

    def parse(start: PState): PState = {
      val postState1 = start.withArrayIndexStack(1L :: start.arrayIndexStack)
      val postState2 = postState1.withOccursCountStack(DaffodilTunableParameters.occursCountMax :: postState1.occursCountStack)
      postState2
    }
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = "StartArray"

    def unparse(start: UState): UState = {
      val postState = start.withArrayIndexStack(1L :: start.arrayIndexStack)
      postState
    }
  }
}

case class EndArray(e: ElementBase, guard: Boolean = true) extends Terminal(e, guard) {

  def parser: DaffodilParser = new PrimParser(this, e) {
    override def toString = "EndArray"

    def parse(start: PState): PState = {
      val shouldValidate = start.dataProc.getValidationMode != ValidationMode.Off
      val actualOccurs = start.arrayIndexStack.headOption
      val postState1 = start.withArrayIndexStack(start.arrayIndexStack.tail)
      val postState2 = postState1.withOccursCountStack(postState1.occursCountStack.tail)

      val finalState = {
        if (shouldValidate) {
          e match {
            case led: LocalElementDecl => {
              val expectedMinOccurs = led.minOccurs
              val expectedMaxOccurs = led.maxOccurs
              val isUnbounded = expectedMaxOccurs == -1
              val postValidationState = actualOccurs match {
                case Some(o) => {
                  val occurrence = o - 1
                  val result =
                    if (isUnbounded && occurrence < expectedMinOccurs)
                      start.withValidationError("%s occurred '%s' times when it was expected to be a " +
                        "minimum of '%s' and a maximum of 'UNBOUNDED' times.", e,
                        occurrence, expectedMinOccurs)
                    else if (!isUnbounded && (occurrence < expectedMinOccurs || occurrence > expectedMaxOccurs))
                      start.withValidationError("%s occurred '%s' times when it was expected to be a " +
                        "minimum of '%s' and a maximum of '%s' times.", e,
                        occurrence, expectedMinOccurs, expectedMaxOccurs)
                    else
                      postState2

                  result
                }
                case None => start.withValidationError("No occurrence found for %s when it was expected to be a " +
                  "minimum of '%s' times and a maximum of '%s' times.", e,
                  expectedMinOccurs, if (isUnbounded) "UNBOUNDED" else expectedMaxOccurs)
              }
              postValidationState
            }
            case _ => postState2
          }
        } else postState2
      }
      finalState
    }
  }

  def unparser: Unparser = new Unparser(e) {
    override def toString = "EndArray"

    def unparse(start: UState): UState = {
      val postState = start.withArrayIndexStack(start.arrayIndexStack.tail)
      postState
    }
  }
}

case class NoValue(e: GlobalElementDecl, guard: Boolean = true) extends Primitive(e, guard)

case class SaveInputStream(e: ElementBase, guard: Boolean = true) extends Primitive(e, guard)

case class SetEmptyInputStream(e: ElementBase, guard: Boolean = true) extends Primitive(e, guard)

case class RestoreInputStream(e: ElementBase, guard: Boolean = true) extends Primitive(e, guard)

//case class Value(e: SchemaComponent, guard: Boolean = true) extends Primitive(e, guard) 

case class NotStopValue(e: ElementBase with LocalElementMixin) extends Primitive(e, e.hasStopValue)

case class StopValue(e: ElementBase with LocalElementMixin) extends Primitive(e, e.hasStopValue)

case class TheDefaultValue(e: ElementBase) extends Primitive(e, e.isDefaultable)

// As soon as you turn these on (by removing the false and putting the real guard), then schemas all need to have
// these properties in them, which is inconvenient until we have multi-file schema support and format references.
case class LeadingSkipRegion(e: Term) extends Terminal(e, true) {
  e.schemaDefinitionUnless(e.leadingSkip < DaffodilTunableParameters.maxSkipLength,
    "Property leadingSkip %s is larger than limit %s", e.leadingSkip, DaffodilTunableParameters.maxSkipLength)

  val alignment = e.alignmentUnits match {
    case AlignmentUnits.Bits => 1
    case AlignmentUnits.Bytes => 8
    case _ => 0 //SDE("Skip/Alignment values must have length units of Bits or Bytes.")
  }

  def parser: DaffodilParser = new PrimParser(this, e) {
    def parse(pstate: PState) = {
      val newBitPos = alignment * e.leadingSkip + pstate.bitPos
      pstate.withPos(newBitPos, -1, None)
    }

    override def toString = "leadingSkip(" + e.leadingSkip + ")"
  }

  def unparser: Unparser = new DummyUnparser(e)
}

case class AlignmentFill(e: Term) extends Terminal(e, true) {

  val alignment = e.alignmentValueInBits

  def isAligned(currBitPos: Long): Boolean = {
    if (alignment == 0 || currBitPos == 0) return true
    if ((currBitPos - alignment) < 0) return false
    if ((currBitPos % alignment) == 0) return true
    return false
  }

  def parser: Parser = new PrimParser(this, e) {
    def parse(pstate: PState) = {
      if (!isAligned(pstate.bitPos)) {
        val maxBitPos = pstate.bitPos + alignment - 1
        val newBitPos = maxBitPos - maxBitPos % alignment
        pstate.withPos(newBitPos, -1, None)
      } else
        pstate
    }
    override def toString = "aligningSkip(" + e.alignment + ")"
  }

  def unparser: Unparser = new DummyUnparser(e)
}

case class TrailingSkipRegion(e: Term) extends Terminal(e, true) {
  e.schemaDefinitionUnless(e.trailingSkip < DaffodilTunableParameters.maxSkipLength,
    "Property trailingSkip %s is larger than limit %s", e.trailingSkip, DaffodilTunableParameters.maxSkipLength)

  val lengthKindContext = e match {
    case eb: ElementBase => eb
    case _ => {
      Assert.invariant(e.nearestEnclosingElement != None) //root element is an ElementBase, all others have a nearestEnclosingElement
      e.nearestEnclosingElement.get
    }
  }
  e.schemaDefinitionWhen(lengthKindContext.lengthKind == LengthKind.Delimited && e.terminator.isConstant && e.terminator.constantAsString == "",
    "Property terminator must be defined when trailingSkip > 0 and lengthKind='delimited'")

  val alignment = e.alignmentUnits match {
    case AlignmentUnits.Bits => 1
    case AlignmentUnits.Bytes => 8
    case _ => 0 //SDE("Skip/Alignment values must have lenght units of Bits or Bytes")
  }
  def parser: Parser = new PrimParser(this, e) {
    def parse(pstate: PState) = {
      val newBitPos = alignment * e.trailingSkip + pstate.bitPos
      pstate.withPos(newBitPos, -1, None)
    }
    override def toString = "trailingSkip(" + e.trailingSkip + ")"
  }
  def unparser: Unparser = new DummyUnparser(e)
}

case class PrefixLength(e: ElementBase) extends Primitive(e, e.lengthKind == LengthKind.Prefixed)

case class UnicodeByteOrderMark(e: GlobalElementDecl) extends Primitive(e, false)

case class FinalUnusedRegion(e: ElementBase) extends Primitive(e, false)

abstract class NewVariableInstanceBase(decl: AnnotatedSchemaComponent, stmt: DFDLNewVariableInstance)
  extends Terminal(decl, true) {
  val (uri, localName) = XMLUtils.QName(decl.xml, stmt.ref, decl.schemaDocument)
  val expName = XMLUtils.expandedQName(uri, localName)
}

case class NewVariableInstanceStart(decl: AnnotatedSchemaComponent, stmt: DFDLNewVariableInstance)
  extends NewVariableInstanceBase(decl, stmt) {

  def parser: DaffodilParser = new PrimParser(this, decl) {
    stmt.notYetImplemented("newVariableInstance")
    def parse(pstate: PState) = {
      stmt.notYetImplemented("newVariableInstance")
    }
  }

  def unparser: Unparser = Assert.notYetImplemented()

}

case class NewVariableInstanceEnd(decl: AnnotatedSchemaComponent, stmt: DFDLNewVariableInstance)
  extends NewVariableInstanceBase(decl, stmt) {

  def parser: DaffodilParser = new PrimParser(this, decl) {
    stmt.notYetImplemented("newVariableInstance")
    def parse(pstate: PState) = stmt.notYetImplemented("newVariableInstance")
  }

  def unparser: Unparser = Assert.notYetImplemented()
}

abstract class AssertPatternBase(decl: AnnotatedSchemaComponent, stmt: DFDLAssertionBase)
  extends Terminal(decl, true)
  with WithParseErrorThrowing
  with TextReader {

  val eName = decl.prettyName
  val testPattern = stmt.testTxt
  val csName = decl.knownEncodingCharset.name()
  val charset = decl.knownEncodingCharset

  def parser: DaffodilParser
  def unparser: Unparser

}

case class AssertPatternPrim(decl: AnnotatedSchemaComponent, stmt: DFDLAssert)
  extends AssertPatternBase(decl, stmt) {

  val kindString = "AssertPatternPrim"

  val d = new DFDLDelimParser(decl.knownEncodingStringBitLengthFunction)

  def parser: DaffodilParser = new PrimParser(this, decl) {

    override def toBriefXML(depthLimit: Int = -1) = {
      "<" + kindString + ">" + testPattern + "</" + kindString + ">"
    }

    def parse(start: PState): PState =
      // withLoggingLevel(LogLevel.Info) 
      {
        withParseErrorThrowing(start) {
          val lastState = start // .withLastState
          val bytePos = (lastState.bitPos >> 3).toInt
          log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, lastState.bitPos)
          log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

          log(LogLevel.Debug, "%s - Looking for testPattern = %s", eName, testPattern)

          if (lastState.bitPos % 8 != 0) {
            return PE(lastState, "%s - not byte aligned.", eName)
          }

          log(LogLevel.Debug, "Retrieving reader")

          val reader = getReader(charset, start.bitPos, lastState)

          val result = d.parseInputPatterned(testPattern, reader, start)

          val postState = result match {
            case s: DelimParseSuccess => {
              val endBitPos = lastState.bitPos + s.numBits
              log(LogLevel.Debug, "Assert Pattern success for testPattern %s", testPattern)
              start
            }
            case f: DelimParseFailure => {
              log(LogLevel.Debug, "Assert Pattern fail for testPattern %s\nDetails: %s", testPattern, f.msg)
              val diag = new AssertionFailed(decl, start, stmt.message, Some(f.msg))
              start.failed(diag)
            }
          }
          postState
        }
      }
  }

  def unparser: Unparser = new Unparser(decl) {

    def unparse(start: UState): UState = {
      start
    }
  }
}

case class DiscriminatorPatternPrim(decl: AnnotatedSchemaComponent, stmt: DFDLAssertionBase)
  extends AssertPatternBase(decl, stmt) {

  val kindString = "DiscriminatorPatternPrim"

  lazy val d = new DFDLDelimParser(decl.knownEncodingStringBitLengthFunction)

  def parser: DaffodilParser = new PrimParser(this, decl) {
    
    val d = new DFDLDelimParser(decl.knownEncodingStringBitLengthFunction)
 

    override def toBriefXML(depthLimit: Int = -1) = {
      "<" + kindString + ">" + testPattern + "</" + kindString + ">"
    }

    def parse(start: PState): PState =
      // withLoggingLevel(LogLevel.Info) 
      {
        withParseErrorThrowing(start) {
          val lastState = start // .withLastState
          val bytePos = (lastState.bitPos >> 3).toInt
          log(LogLevel.Debug, "%s - Starting at bit pos: %s", eName, lastState.bitPos)
          log(LogLevel.Debug, "%s - Starting at byte pos: %s", eName, bytePos)

          log(LogLevel.Debug, "%s - Looking for testPattern = %s", eName, testPattern)

          if (lastState.bitPos % 8 != 0) {
            return PE(lastState, "%s - not byte aligned.", eName)
          }

          log(LogLevel.Debug, "Retrieving reader")

          val reader = getReader(charset, start.bitPos, lastState)

          val result = d.parseInputPatterned(testPattern, reader, start)

          // Only want to set the discriminator if it is true
          // we do not want to modify it unless it's true
          val finalState = result match {
            case s: DelimParseSuccess => start.withDiscriminator(true)
            case f: DelimParseFailure => {
              val diag = new AssertionFailed(decl, start, stmt.message, Some(f.msg))
              start.failed(diag)
            }
          }
          finalState
        }
      }
  }

  def unparser: Unparser = new Unparser(decl) {

    def unparse(start: UState): UState = {
      start
    }
  }
}

abstract class AssertBase(decl: AnnotatedSchemaComponent,
  exprWithBraces: String,
  xmlForNamespaceResolution: Node,
  scWherePropertyWasLocated: AnnotatedSchemaComponent,
  msg: String,
  discrim: Boolean, // are we a discriminator or not.
  assertKindName: String)
  extends ExpressionEvaluatorBase(scWherePropertyWasLocated) {

  def this(
    decl: AnnotatedSchemaComponent,
    foundProp: Found,
    msg: String,
    discrim: Boolean, // are we a discriminator or not.
    assertKindName: String) =
    this(decl, foundProp.value, foundProp.location.xml, decl, msg, discrim, assertKindName)

  override val baseName = assertKindName
  override lazy val expandedTypeName = XMLUtils.XSD_BOOLEAN
  override lazy val exprText = exprWithBraces
  override lazy val exprXMLForNamespace = xmlForNamespaceResolution
  override lazy val exprComponent = scWherePropertyWasLocated

  def unparser = DummyUnparser

  def parser: DaffodilParser = new ExpressionEvaluationParser(this, decl) {

    def parse(start: PState): PState =
      // withLoggingLevel(LogLevel.Info) 
      {
        withParseErrorThrowing(start) {
          log(LogLevel.Debug, "This is %s", toString)
          val R(res, newVMap) = eval(start)
          val testResult = res.asInstanceOf[Boolean]
          val postState = start.withVariables(newVMap)
          if (testResult) {
            postState.withDiscriminator(discrim)
          } else {
            // The assertion failed. Prepare a failure message etc. in case backtracking ultimately fails from here.
            val diag = new AssertionFailed(decl, postState, msg)
            postState.failed(diag)
          }
        }
      }
  }
}

abstract class AssertBooleanPrimBase(
  decl: AnnotatedSchemaComponent,
  stmt: DFDLAssertionBase,
  discrim: Boolean, // are we a discriminator or not.
  assertKindName: String) extends AssertBase(decl, Found(stmt.testTxt, stmt), stmt.message, discrim, assertKindName)

case class AssertBooleanPrim(
  decl: AnnotatedSchemaComponent,
  stmt: DFDLAssertionBase)
  extends AssertBooleanPrimBase(decl, stmt, false, "assert") {
}

case class DiscriminatorBooleanPrim(
  decl: AnnotatedSchemaComponent,
  stmt: DFDLAssertionBase)
  extends AssertBooleanPrimBase(decl, stmt, true, "discriminator")

// TODO: performance wise, initiated content is supposed to be faster
// than evaluating an expression. There should be a better way to say
// "resolve this point of uncertainty" without having to introduce
// an XPath evaluator that runs fn:true() expression.
case class InitiatedContent(
  decl: AnnotatedSchemaComponent)
  extends AssertBase(decl,
    "{ fn:true() }", <xml xmlns:fn={ XMLUtils.XPATH_FUNCTION_NAMESPACE }/>, decl,
    // always true. We're just an assertion that says an initiator was found.
    "initiatedContent. This message should not be used.",
    true,
    "initiatedContent")

case class SetVariable(decl: AnnotatedSchemaComponent, stmt: DFDLSetVariable)
  extends ExpressionEvaluatorBase(decl) {

  val baseName = "SetVariable[" + stmt.localName + "]"

  override lazy val exprText = stmt.value
  override lazy val exprXMLForNamespace = stmt.xml
  override lazy val exprComponent = stmt

  lazy val expandedTypeName = stmt.defv.extType

  def parser: DaffodilParser = new SetVariableParser(this, decl, stmt)
  def unparser = DummyUnparser
}

/**
 * Refactored primitives that use expressions to put expression evaluation in one place.
 * On this base (for the primitive), and a corresponding parser base class for the
 * actual evaluation.
 *
 * That fixed a bug where a SDE wasn't being reported until the parser was run that
 * could have been reported at compilation time.
 *
 * Anything being computed that involves the dsom or grammar objects or attributes of them,
 * should be done in the grammar primitives, and NOT in the parser.
 * This is important to insure errors are captured at compilation time and
 * reported on relevant objects.
 */
abstract class ExpressionEvaluatorBase(e: AnnotatedSchemaComponent) extends Terminal(e, true) {
  override def toString = baseName + "(" + exprText + ")"

  def toBriefXML(depthLimit: Int = -1) = {
    "<" + baseName + ">" + exprText + "</" + baseName + ">"
  }

  def baseName: String
  def exprXMLForNamespace: Node
  def exprComponent: SchemaComponent
  def expandedTypeName: String
  def exprText: String

  val expressionTypeSymbol = {
    // println(expandedTypeName)
    e.expressionCompiler.convertTypeString(expandedTypeName)
  }

  val expr = e.expressionCompiler.compile(expressionTypeSymbol, exprText, exprXMLForNamespace, exprComponent)
}

case class InputValueCalc(e: ElementBase)
  extends ExpressionEvaluatorBase(e) {

  val baseName = "InputValueCalc"
  lazy val exprProp = e.inputValueCalcOption match {
    case f: Found => f
    case _: NotFound => Assert.invariantFailed("must be a Found object")
  }

  override lazy val exprText = exprProp.value
  override lazy val exprXMLForNamespace = exprProp.location.xml
  override lazy val exprComponent = exprProp.location.asInstanceOf[SchemaComponent]

  lazy val pt = e.primType
  lazy val ptn = pt.name
  lazy val expandedTypeName = XMLUtils.expandedQName(XMLUtils.XSD_NAMESPACE, ptn)

  def parser: DaffodilParser = new IVCParser(this, e)
  def unparser = DummyUnparser
}

/**
 * Common parser base class for any parser that evaluates an expression.
 */
abstract class ExpressionEvaluationParser(context: ExpressionEvaluatorBase, e: AnnotatedSchemaComponent)
  extends Parser(e) with WithParseErrorThrowing {

  override def toBriefXML(depthLimit: Int = -1) = context.toBriefXML(depthLimit)

  def eval(start: PState) = {
    val currentElement = start.parentElement
    val R(res, newVMap) =
      context.expr.evaluate(currentElement, start.variableMap, start)
    // val result = res.toString // Everything in JDOM is a string!
    R(res, newVMap)
  }
}

class IVCParser(context: InputValueCalc, e: ElementBase)
  extends ExpressionEvaluationParser(context, e) {
  Assert.invariant(e.isSimpleType)

  def parse(start: PState): PState =
    // withLoggingLevel(LogLevel.Info) 
    {
      withParseErrorThrowing(start) {
        log(LogLevel.Debug, "This is %s", toString)
        val currentElement = start.parentElement
        val R(res, newVMap) = eval(start)

        currentElement.setDataValue(res.toString)
        val postState = start.withVariables(newVMap) // inputValueCalc consumes nothing. Just creates a value.
        postState
      }
    }
}

class SetVariableParser(context: ExpressionEvaluatorBase, decl: AnnotatedSchemaComponent, stmt: DFDLSetVariable)
  extends ExpressionEvaluationParser(context, decl) {

  def parse(start: PState): PState =
    // withLoggingLevel(LogLevel.Info) 
    {
      withParseErrorThrowing(start) {
        log(LogLevel.Debug, "This is %s", toString)
        val R(res, newVMap) = eval(start)
        val newVMap2 = newVMap.setVariable(stmt.defv.extName, res, decl)
        val postState = start.withVariables(newVMap2)
        postState
      }
    }
}

trait TextReader extends Logging {

  /**
   * Readers are stored in the PState within the InStream object.
   */
  def getReader(charset: Charset, bitPos: Long, state: PState): DFDLCharReader = {
    // withLoggingLevel(LogLevel.Info) 
    {
      val csName = charset.name()
      log(LogLevel.Debug, "Retrieving reader at bytePos %s", bitPos >> 3)
      // Do we already have a reader in the PState?
      val res = state.inStream.getCharReader(charset, bitPos)
      res
    }
  }

}

trait Padded { self: Terminal =>
  var padChar = ""
  val eBase = self.context.asInstanceOf[ElementBase]

  val justificationTrim: TextJustificationType.Type = eBase.textTrimKind match {
    case TextTrimKind.None => TextJustificationType.None
    case TextTrimKind.PadChar if eBase.isSimpleType => {
      val theJust = eBase.primType match {

        case PrimType.Int | PrimType.Byte | PrimType.Short | PrimType.Long |
          PrimType.Integer | PrimType.UInt | PrimType.UByte | PrimType.UShort |
          PrimType.ULong | PrimType.Double | PrimType.Float | PrimType.Decimal |
          PrimType.NonNegativeInteger => {
          padChar = eBase.textNumberPadCharacter
          eBase.textNumberJustification match {
            case TextNumberJustification.Left => TextJustificationType.Left
            case TextNumberJustification.Right => TextJustificationType.Right
            case TextNumberJustification.Center => TextJustificationType.Center
          }
        }
        case PrimType.String => {
          padChar = eBase.textStringPadCharacter
          eBase.textStringJustification match {
            case TextStringJustification.Left => TextJustificationType.Left
            case TextStringJustification.Right => TextJustificationType.Right
            case TextStringJustification.Center => TextJustificationType.Center
          }
        }
        case PrimType.DateTime | PrimType.Date | PrimType.Time => {
          padChar = eBase.textCalendarPadCharacter
          eBase.textCalendarJustification match {
            case TextCalendarJustification.Left => TextJustificationType.Left
            case TextCalendarJustification.Right => TextJustificationType.Right
            case TextCalendarJustification.Center => TextJustificationType.Center
          }
        }
        case PrimType.Boolean => {
          padChar = eBase.textBooleanPadCharacter
          eBase.textBooleanJustification match {
            case TextBooleanJustification.Left => TextJustificationType.Left
            case TextBooleanJustification.Right => TextJustificationType.Right
            case TextBooleanJustification.Center => TextJustificationType.Center
          }
        }
        case PrimType.HexBinary => TextJustificationType.None
      }
      theJust
    }
    case _ => TextJustificationType.None
  }

  val optPadChar = padChar match {
    case "" => None
    case x => Some(x)
  }

  def removeRightPadding(str: String): String = str.reverse.dropWhile(c => c == padChar.charAt(0)).reverse
  def removeLeftPadding(str: String): String = str.dropWhile(c => c == padChar.charAt(0))
  def removePadding(str: String): String = removeRightPadding(removeLeftPadding(str))

  def trimByJustification(str: String): String = {
    val result = justificationTrim match {
      case TextJustificationType.None => str
      case TextJustificationType.Right => removeLeftPadding(str)
      case TextJustificationType.Left => removeRightPadding(str)
      case TextJustificationType.Center => removePadding(str)
    }
    result
  }

}

//case class LeftPadding(e: ElementBase)
//  extends Terminal(e, true) with TextReader {
//  def parser: DaffodilParser = new PrimParser(this, e) {
//
//    val charset = e.knownEncodingCharset
//    val padChar = {
//      e.primType match {
//        case PrimType.Int | PrimType.Byte | PrimType.Short | PrimType.Long |
//          PrimType.Integer | PrimType.UInt | PrimType.UByte | PrimType.UShort |
//          PrimType.ULong | PrimType.Double | PrimType.Float | PrimType.Decimal |
//          PrimType.NonNegativeInteger => e.textNumberPadCharacter
//        case PrimType.String => e.textStringPadCharacter
//        case PrimType.DateTime | PrimType.Date | PrimType.Time => e.textCalendarPadCharacter
//        case PrimType.Boolean => e.textBooleanPadCharacter
//        case PrimType.HexBinary => null
//      }
//    }
//    val leftPaddingParser: TextPaddingParser = {
//      if (padChar != null)
//        new TextPaddingParser(padChar.charAt(0), e.knownEncodingStringBitLengthFunction)
//      else null
//    }
//
//    override def toBriefXML(depthLimit: Int = -1): String = {
//      "<LeftPadding name='" + e.name + "' padChar='" + padChar + "'/>"
//    }
//
//    def parse(start: PState): PState = {
//      if (padChar == null) return start
//
//      val reader = getReader(charset, start.bitPos, start)
//      val res = leftPaddingParser.parse(reader)
//      val finalState = res match {
//        case None => start
//        case Some(result) => {
//          val field = result.field.getOrElse("")
//          val numBits = result.numBits
//          log(LogLevel.Debug, "%s - Parsed: %s Parsed Bytes: %s (bits %s)", e.name, field, numBits / 8, numBits)
//          val endCharPos = if (start.charPos == -1) result.numCharsRead else start.charPos + result.numCharsRead
//          val endBitPos = start.bitPos + numBits
//          start.withPos(endBitPos, endCharPos, Some(result.next))
//        }
//      }
//      finalState
//    }
//  }
//
//  def unparser = new DummyUnparser(e)
//}

