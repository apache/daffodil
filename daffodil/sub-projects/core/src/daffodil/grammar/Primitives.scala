package daffodil.grammar

import daffodil.dsom._
import daffodil.exceptions.Assert
import daffodil.schema.annotation.props._
import daffodil.schema.annotation.props.gen._
import daffodil.xml._
import daffodil.processors._
import java.nio.CharBuffer
import com.ibm.icu.text._
import java.util.regex._
import java.text.{ ParseException, ParsePosition }
import java.math.BigInteger
import stringsearch.constructs._
import stringsearch.delimiter._

import daffodil.util._

import daffodil.dsom.EntityReplacer._
import scala.collection.mutable.Queue

case class ElementBegin(e: ElementBase) extends Terminal(e, e.isComplexType.value != true || e.lengthKind != LengthKind.Pattern) {
  def parser: Parser = new Parser(e) {

    override def toString = "<" + e.name + ">"

    /**
     * ElementBegin just adds the element we are constructing to the infoset and changes
     * the state to be referring to this new element as what we're parsing data into.
     */
    def parse(start: PState): PState = {
      
      val currentElement = new org.jdom.Element(e.name, e.namespace)
      log(Debug("currentElement = %s", currentElement))
      val priorElement = start.parent
      priorElement.addContent(currentElement)
      log(Debug("priorElement = %s", priorElement))
      val postState = start.withParent(currentElement)
      postState
    }
  }
}

case class ComplexElementBeginPattern(e: ElementBase) extends Terminal(e, e.isComplexType.value == true && e.lengthKind == LengthKind.Pattern) {
  Assert.invariant(e.isComplexType.value)
  def parser: Parser = new Parser(e) {
    override def toString = "<" + e.name + " dfdl:lengthKind='pattern'>"
    val decoder = e.knownEncodingDecoder
    var cbuf = CharBuffer.allocate(1024) // TODO: Performance: get a char buffer from a pool.
    val pattern = e.lengthPattern

    /**
     * ElementBegin just adds the element we are constructing to the infoset and changes
     * the state to be referring to this new element as what we're parsing data into.
     */
    def parse(start: PState): PState = {
      val in = start.inStream.asInstanceOf[InStreamFromByteChannel]
      val (result, endBitPos, theState) = in.fillCharBufferWithPatternMatch(cbuf, start.bitPos, decoder, pattern)

      val postState1 = theState match {
        case SearchResult.NoMatch => start.failed(this.toString() + ": No match found!")
        case SearchResult.PartialMatch => start.failed(this.toString() + ": Partial match found!")
        case SearchResult.FullMatch => {
          log(Debug("Parsed: " + result))
          log(Debug("Ended at bit position " + endBitPos))
          val limitedInStream = in.withLimit(start.bitPos, endBitPos)
          val count = ((endBitPos - start.bitPos + 7) / 8).asInstanceOf[Int]
          start withEndBitLimit(endBitPos) withInStream(new InStreamFromByteChannel(e, limitedInStream, count))
        }
      }

      val currentElement = new org.jdom.Element(e.name, e.namespace)
      log(Debug("currentElement = %s", currentElement))
      val priorElement = postState1.parent
      priorElement.addContent(currentElement)
      log(Debug("priorElement = %s", priorElement))
      val postState2 = postState1 withParent(currentElement)
      postState2
    }
  }
}

case class ElementEnd(e: ElementBase) extends Terminal(e, e.isComplexType.value != true || e.lengthKind != LengthKind.Pattern) {
  def parser: Parser = new Parser(e) {

    override def toString = "</" + e.name + ">"

    /**
     * ElementEnd just moves back to the parent element of the current one.
     */
    def parse(start: PState): PState = {
      val currentElement = start.parent
      Assert.invariant(currentElement.getName() != "_document_" )
      val priorElement = currentElement.getParent.asInstanceOf[org.jdom.Element]
      log(Debug("priorElement = %s", priorElement))
      val postState = start.withParent(priorElement).moveOverByOne
      postState
    }
  }
}

case class ComplexElementEndPattern(e: ElementBase) extends Terminal(e, e.isComplexType.value == true && e.lengthKind == LengthKind.Pattern) {
  // TODO: Should this be more generic; is there a way to detect state from the current element to tell us if it's time
  //       to pop the input stack?
  def parser: Parser = new Parser(e) {

    override def toString = "</" + e.name + " dfdl:lengthKind='pattern'>"

    /**
     * ElementEnd just moves back to the parent element of the current one.
     */
    def parse(start: PState): PState = {
      val currentElement = start.parent
      log(Debug("currentElement = %s", currentElement))
      Assert.invariant(currentElement.getName() != "_document_" )
      val priorElement = currentElement.getParent.asInstanceOf[org.jdom.Element]
      log(Debug("priorElement = %s", priorElement))
      val postState = start.withParent(priorElement).moveOverByOne.withLastInStream()
      postState
    }
  }
}

/**
 * The I/O layer should be written to use Java's NIO Channels, and Direct ByteBuffers for file I/O. This is the
 * fastest stuff in the Java stack.
 *
 * The basic design for known-length parsing
 * is to do a bounds check on whether we have enough buffer space to accomplish the reading of data
 * within the buffer. If not, repositioning/refreshing the buffer. If so, then we want to issue a
 * single block read per element which both reads and converts to the right character encoding, or
 * directly to the right type (float, long, double, etc) when the result type is a primitive.
 *
 * For types with more complexity, (binary dates for example) then the previous would still be done
 * followed by a conversion of some sort.
 */

case class StringFixedLengthInBytes(e: ElementBase, nBytes: Long) extends Terminal(e, true) with Logging {
  def parser: Parser = new Parser(e) {

    override def toString = "StringFixedLengthInBytesParser(" + nBytes + ")"
    val decoder = e.knownEncodingDecoder
    val cbuf = CharBuffer.allocate(nBytes.toInt) // TODO: Performance: get a char buffer from a pool. 

    def parse(start: PState): PState = {
      log(Debug("Parsing starting at bit position: " + start.bitPos))
      val in = start.inStream
      val endBitPos = in.fillCharBuffer(cbuf, start.bitPos, decoder)
      if (endBitPos < start.bitPos + nBytes * 8) {
        // Do Something Bad
        return PE(start, "Insufficent Bits in field; required " + nBytes * 8 + " received " + (endBitPos - start.bitPos))
      }
      val result = cbuf.toString
      log(Debug("Parsed: " + result))
      log(Debug("Ended at bit position " + endBitPos))
      val endCharPos = start.charPos + result.length
      val currentElement = start.parent
      Assert.invariant(currentElement.getName != "_document_")
      // Note: this side effect is backtracked, because at points of uncertainty, pre-copies of a node are made
      // and when backtracking occurs they are used to replace the nodes modified by sub-parsers.
      currentElement.addContent(new org.jdom.Text(result))
      val postState = start.withPos(endBitPos, endCharPos)
      postState
    }
  }
}

case class StringFixedLengthInBytesVariableWidthCharacters(e: ElementBase, nBytes: Long) extends Terminal(e, true) {
  Assert.notYetImplemented()
  def parser: Parser = new Parser(e) {
    def parse(start: PState): PState = {
      Assert.notYetImplemented()
    }

  }
}

//case class StringFixedLengthInVariableWidthCharacters(e: ElementBase, nChars: Long) extends Terminal(e, true) {
//  // TODO: Implement UTF-8
//  Assert.notYetImplemented()
//  def parser: Parser = new Parser(e) {
//    def parse(start: PState): PState = {
//      Assert.notYetImplemented()
//    }
//  }
//}

case class StringFixedLengthInVariableWidthCharacters(e: ElementBase, nChars: Long) extends Terminal(e, true) {
  // TODO: Implement UTF-8
  lazy val es = e.escapeScheme
  lazy val esObj = EscapeScheme.getEscapeScheme(es, e)
  
  def parser: Parser = new Parser(e) {
    
    override def toString = "StringFixedLengthInVariableWidthCharactersParser(" + nChars + ")"
    val decoder = e.knownEncodingDecoder
    val cbuf = CharBuffer.allocate(1024) // TODO: Performance: get a char buffer from a pool. 
    
    def parse(start: PState): PState = {
      setLoggingLevel(LogLevel.Debug)
      
      log(Debug(this.toString() + " - Parsing starting at bit position: " + start.bitPos))
      val in = start.inStream.asInstanceOf[InStreamFromByteChannel]
      var bitOffset = 0L

      val delimiters2 = e.allTerminatingMarkup.map(x => x.evaluate(start.parent, start.variableMap).asInstanceOf[String].split("\\s").toList)
      val delimiters = delimiters2.flatten(x => x)
      val delimsCooked: Queue[String] = new Queue
      
      delimiters.foreach(x => delimsCooked.enqueue(EntityReplacer.replaceAll(x)))
      
      log(Debug("StringFixedLengthInVariableWidthCharactersParser - Looking for: " + delimsCooked + " Count: " + delimsCooked.length))
    
      val (result, endBitPos, theState, theDelimiter) = in.fillCharBufferUntilDelimiterOrEnd(cbuf, start.bitPos, decoder, Set.empty, delimsCooked.toSet, esObj)
  
      System.err.println(result)
      
      if (result == null) {return start.failed(this.toString() + " - Result was null!")}
      
      if (result.length() < nChars){ return start.failed(this.toString() + " - Result(" + result.length() + ") was not at least nChars (" + nChars + ") long.")}
      
      val postState = theState match {
        case SearchResult.NoMatch => {
          // No Terminator, so last result is a field.
          val finalResult = result.substring(0, nChars.toInt)
          val finalResultBytes = finalResult.getBytes(decoder.charset()).length
          val finalBitPos = 8 * finalResultBytes + start.bitPos
          
          log(Debug(this.toString() + " - Parsed: " + finalResult))
          log(Debug(this.toString() + " - Ended at bit position " + finalBitPos))
          val endCharPos = start.charPos + nChars
          val currentElement = start.parent
          currentElement.addContent(new org.jdom.Text(finalResult))
          start.withPos(finalBitPos, endCharPos)
        } 
        case SearchResult.PartialMatch => start.failed(this.toString() + ": Partial match found!")
        case SearchResult.FullMatch => {
          val finalResult = result.substring(0, nChars.toInt)
          val finalResultBytes = finalResult.getBytes(decoder.charset()).length
          val finalBitPos = 8 * finalResultBytes + start.bitPos
          
          log(Debug(this.toString() + " - Parsed: " + finalResult))
          log(Debug(this.toString() + " - Ended at bit position " + finalBitPos))
          //val endCharPos = start.charPos + result.length()
          val endCharPos = start.charPos + nChars
          val currentElement = start.parent
          currentElement.addContent(new org.jdom.Text(finalResult))
          start.withPos(finalBitPos, endCharPos)
        }
        case SearchResult.EOD => {
          start.failed(this.toString() + " - Reached End Of Data.")
        }
      }
      postState
    }
  }
}

case class StringDelimitedEndOfData(e: ElementBase) extends Terminal(e, true) with Logging {

  lazy val es = e.escapeScheme
  lazy val esObj = EscapeScheme.getEscapeScheme(es, e)
  lazy val tm = e.terminatingMarkup
  lazy val cname = toString

  def parser: Parser = new Parser(e) {
    override def toString = cname + "(" + tm.map{_.prettyExpr} + ")"  
    val decoder = e.knownEncodingDecoder
    var cbuf = CharBuffer.allocate(1024) // TODO: Performance: get a char buffer from a pool.

    def parse(start: PState): PState = {
      
      log(Debug(this.toString() + " - Parsing starting at bit position: " + start.bitPos))
      val in = start.inStream.asInstanceOf[InStreamFromByteChannel]
      var bitOffset = 0L

      //val delimiters2 = e.terminatingMarkup.map(x => x.evaluate(start.parent, start.variableMap).asInstanceOf[String].split("\\s").toList)
      val delimiters2 = e.allTerminatingMarkup.map(x => x.evaluate(start.parent, start.variableMap).asInstanceOf[String].split("\\s").toList)
      val delimiters = delimiters2.flatten(x => x)
      val delimsCooked: Queue[String] = new Queue
      
      delimiters.foreach(x => delimsCooked.enqueue(EntityReplacer.replaceAll(x)))
      
      //log(Debug("StringDelimitedEndOfData - Looking for: " + delimiters + " Count: " + delimiters.length))
      log(Debug("StringDelimitedEndOfData - Looking for: " + delimsCooked + " Count: " + delimsCooked.length))
    
      val (result, endBitPos, theState, theDelimiter) = in.fillCharBufferUntilDelimiterOrEnd(cbuf, start.bitPos, decoder, Set.empty, delimsCooked.toSet, esObj)
      val postState = theState match {
        case SearchResult.NoMatch => {
          // TODO: Is this logic correct?
          // No Terminator, so last result is a field.
          log(Debug(this.toString() + " - Parsed: " + result))
          log(Debug(this.toString() + " - Ended at bit position " + endBitPos))
          val endCharPos = start.charPos + result.length()
          val currentElement = start.parent
          currentElement.addContent(new org.jdom.Text(result))
          start.withPos(endBitPos, endCharPos)
        } 
        case SearchResult.PartialMatch => start.failed(this.toString() + ": Partial match found!")
        case SearchResult.FullMatch => {
          log(Debug(this.toString() + " - Parsed: " + result))
          log(Debug(this.toString() + " - Ended at bit position " + endBitPos))
          val endCharPos = start.charPos + result.length()
          val currentElement = start.parent
          currentElement.addContent(new org.jdom.Text(result))
          start.withPos(endBitPos, endCharPos)
        }
        case SearchResult.EOD => {
          start.failed(this.toString() + " - Reached End Of Data.")
        }
      }

      postState
    }
  }
}

case class StringPatternMatched(e: ElementBase) extends Terminal(e, true) with Logging {
  val sequenceSeparator = e.nearestEnclosingSequence.get.separator

  def parser: Parser = new Parser(e) {
    override def toString = "StringPatternMatched"
    val decoder = e.knownEncodingDecoder
    var cbuf = CharBuffer.allocate(1024)
    val pattern = e.lengthPattern

    // TODO: Add parameter for changing CharBuffer size

    def parse(start: PState): PState = {
      log(Debug("Parsing starting at bit position: " + start.bitPos))
      val in = start.inStream.asInstanceOf[InStreamFromByteChannel]
      var bitOffset = 0L

      val (result, endBitPos, theState) = in.fillCharBufferWithPatternMatch(cbuf, start.bitPos, decoder, pattern)

      val postState = theState match {
        case SearchResult.NoMatch => start.failed(this.toString() + ": No match found!")
        case SearchResult.PartialMatch => start.failed(this.toString() + ": Partial match found!")
        case SearchResult.FullMatch => {
          log(Debug("Parsed: " + result))
          log(Debug("Ended at bit position " + endBitPos))
          val endCharPos = start.charPos + result.length()
          val currentElement = start.parent
          currentElement.addContent(new org.jdom.Text(result))
          start.withPos(endBitPos, endCharPos)
        }
      }

      postState
    }
  }
}

abstract class ConvertTextNumberPrim[S](e: ElementBase, guard: Boolean) extends Terminal(e, guard) with Logging {
  protected def getNum(s: Number): S
  protected val GramName = "number"
  protected val GramDescription = "Number"
  protected def isInvalidRange(n: S): Boolean = false
  def parser: Parser = new Parser(e) {
    def parse(start: PState): PState = {
      val node = start.parent
      var str = node.getText

      Assert.invariant(str != null) // worst case it should be empty string. But not null.
      val resultState = try {
        // Strip leading + (sign) since the DecimalFormat can't handle it
        if (str.length > 0 && str.charAt(0) == '+') {
          // TODO: There needs to be a way to restore '+' in the unparse, but that will be in the format field
          str = str.substring(1)
        }
        if (str == "") return PE(start, "Convert to %s (for xs:%s): Cannot parse number from empty string", GramDescription, GramName)

        // Need to use Decimal Format to parse even though this is an Integral number
        val df = new DecimalFormat()
        val pos = new ParsePosition(0)
        val num = df.parse(str, pos)

        // Verify that what was parsed was what was passed exactly in byte count.  Use pos to verify all characters consumed & check for errors!
        if (pos.getIndex != str.length) {
          // log(Debug("Error: Unable to parse all characters from " + GramDescription + ": " + str + "\n"))
          return PE(start, "Convert to %s (for xs:%s): Unable to parse all characters from: %s", GramDescription, GramName, str)
        }

        // Assume long as the most precision
        val asNumber = getNum(num)

        // Verify no digits lost (the number was correctly transcribed)
        if (asNumber.asInstanceOf[Number] != num || isInvalidRange(asNumber)) {
          // Transcription error
          // log(Debug("Error: Invalid " + GramDescription + ": " + str + "\n"))
          return PE(start, "Convert to %s (for xs:%s): Invalid data.", GramDescription, GramName, str)
        } else {
          node.setText(asNumber.toString)
        }

        start
      } // catch { case e: Exception => start.failed("Failed to convert %s to an xs:%s" + GramName) }

      resultState
    }
  }

}

case class ConvertTextIntegerPrim(e: ElementBase) extends ConvertTextNumberPrim[BigInteger](e, true) {
  protected override def getNum(num: Number) = new BigInteger(num.toString)
  protected override val GramName = "integer"
  protected override val GramDescription = "Unbounded Integer"
}

case class ConvertTextLongPrim(e: ElementBase) extends ConvertTextNumberPrim[Long](e, true) {
  protected override def getNum(num: Number) = num.longValue
  protected override val GramName = "long"
  protected override val GramDescription = "Long Integer"
}

case class ConvertTextIntPrim(e: ElementBase) extends ConvertTextNumberPrim[Int](e, true) {
  protected override def getNum(num: Number) = num.intValue
  protected override val GramName = "int"
  protected override val GramDescription = "Integer"
}

case class ConvertTextShortPrim(e: ElementBase) extends ConvertTextNumberPrim[Short](e, true) {
  protected override def getNum(num: Number) = num.shortValue
  protected override val GramName = "short"
  protected override val GramDescription = "Short Integer"
}

case class ConvertTextBytePrim(e: ElementBase) extends ConvertTextNumberPrim[Byte](e, true) {
  protected override def getNum(num: Number) = num.byteValue
  protected override val GramName = "byte"
  protected override val GramDescription = "Byte"
}

case class ConvertTextUnsignedLongPrim(e: ElementBase) extends ConvertTextNumberPrim[BigInteger](e, true) {
  protected override def getNum(num: Number) = new BigInteger(num.toString)
  protected override val GramName = "unsignedLong"
  protected override val GramDescription = "Unsigned Long"
  protected override def isInvalidRange(n: BigInteger) = n.compareTo(BigInteger.ZERO) < 0 || n.compareTo(BigInteger.ONE.shiftLeft(64)) >= 0
}

case class ConvertTextUnsignedIntPrim(e: ElementBase) extends ConvertTextNumberPrim[Long](e, true) {
  protected override def getNum(num: Number) = num.longValue
  protected override val GramName = "unsignedInt"
  protected override val GramDescription = "Unsigned Int"
  protected override def isInvalidRange(n: Long) = n < 0 || n >= (1L << 32)
}

case class ConvertTextUnsignedShortPrim(e: ElementBase) extends ConvertTextNumberPrim[Int](e, true) {
  protected override def getNum(num: Number) = num.intValue
  protected override val GramName = "unsignedShort"
  protected override val GramDescription = "Unsigned Short"
  protected override def isInvalidRange(n: Int) = n < 0 || n >= (1 << 16)
}

case class ConvertTextUnsignedBytePrim(e: ElementBase) extends ConvertTextNumberPrim[Short](e, true) {
  protected override def getNum(num: Number) = num.shortValue
  protected override val GramName = "unsignedByte"
  protected override val GramDescription = "Unsigned Byte"
  protected override def isInvalidRange(n: Short) = n < 0 || n >= (1 << 8)
}

case class ConvertTextDoublePrim(e: ElementBase) extends Terminal(e, true) {
  def parser: Parser = new Parser(e) {

    override def toString = "to(xs:double)"

    def parse(start : PState) : PState = {
      val node = start.parent
      val str = node.getText

      val resultState =
        // try 
        {
          //convert to NumberFormat to handle format punctuation such as , . $ & etc
          //then get the value as a double and convert to string
          val df = new DecimalFormat()
          val pos = new ParsePosition(0)
          val num = df.parse(str, pos)
          node.setText(num.doubleValue.toString)

          start
        } // catch { case e: Exception => start.failed("Failed to convert to an xs:double") }

      resultState
    }
  }
}

case class ConvertTextFloatPrim(e: ElementBase) extends Terminal(e, true) {
  def parser: Parser = new Parser(e) {

    override def toString = "to(xs:float)"

    def parse(start: PState): PState = {
      val node = start.parent
      val str = node.getText

      val resultState = 
        // Note: don't wrap in try-catch. The framework has everything surrounded
        // by that already.
        // try 
        {
        //convert to NumberFormat to handle format punctuation such as , . $ & etc
        //then get the value as a float and convert to string
        val df = new DecimalFormat()
        val pos = new ParsePosition(0)
        val num = df.parse(str, pos)
        node.setText(num.floatValue.toString)
        start
        } // catch { case e: Exception => start.failed("Failed to convert to an xs:float") }

      resultState
    }
  }
}

abstract class Primitive(e: Term, guard: Boolean = false)
  extends Terminal(e, guard) {
  override def toString = "Prim[" + name + "]"
  def parser: Parser = DummyParser(e)
}

abstract class ZonedTextNumberPrim(e: ElementBase, guard: Boolean) extends Terminal(e, guard) {
  def parser: Parser = new Parser(e) {
    def parse(start: PState): PState = {
      // TODO: Compute the Zoned Number generically
      start
    }
  }
}
case class ZonedTextBytePrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextShortPrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextIntPrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextLongPrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)

class Regular32bitIntPrim(context: Term, byteOrder: java.nio.ByteOrder) extends Parser(context) {
  override def toString = "binary(xs:int, " + byteOrder + ")"
  def parse(start: PState): PState = {
    if (start.bitLimit != -1L && (start.bitLimit - start.bitPos < 32)) start.failed("Not enough bits to create an xs:int")
    else {
      val value = start.inStream.getInt(start.bitPos, byteOrder)
      start.parent.addContent(new org.jdom.Text(value.toString))
      val postState = start.withPos(start.bitPos + 32, -1)
      postState
    }
  }

}

case class Regular32bitBigEndianIntPrim(e: ElementBase) extends Terminal(e, true) {
  def parser = new Regular32bitIntPrim(e, java.nio.ByteOrder.BIG_ENDIAN)
}
case class Regular32bitLittleEndianIntPrim(e: ElementBase) extends Terminal(e, true) {
  def parser = new Regular32bitIntPrim(e, java.nio.ByteOrder.LITTLE_ENDIAN)
}
case class PackedIntPrim(e: ElementBase) extends Primitive(e, false)
case class BCDIntPrim(e: ElementBase) extends Primitive(e, false)

case class DoublePrim(ctx: Term, byteOrder: java.nio.ByteOrder) extends Parser(ctx) with Logging {
  override def toString = "binary(xs:double, " + byteOrder + ")"
  def parse(start: PState): PState = {
    if (start.bitLimit != -1L && (start.bitLimit - start.bitPos < 64)) start.failed("Not enough bits to create an xs:double")
    else {
      val value = start.inStream.getDouble(start.bitPos, byteOrder)
      start.parent.addContent(new org.jdom.Text(value.toString))
      log(Debug("Found binary double " + value))
      log(Debug("Ended at bit position " + (start.bitPos + 64)))
      //val postState = start.withPos(start.bitPos + 64, -1)
      val postState = start.withPos(start.bitPos + 64, start.charPos + 1)
      postState
    }
  }
}

case class BigEndianDoublePrim(e: ElementBase) extends Terminal(e, true) {
  def parser = new DoublePrim(e, java.nio.ByteOrder.BIG_ENDIAN)
}

case class LittleEndianDoublePrim(e: ElementBase) extends Terminal(e, true) {
  def parser = new DoublePrim(e, java.nio.ByteOrder.LITTLE_ENDIAN)
}

case class FloatPrim(ctx: Term, byteOrder: java.nio.ByteOrder) extends Parser(ctx) {
  override def toString = "binary(xs:float, " + byteOrder + ")"
  def parse(start: PState): PState = {
    if (start.bitLimit != -1L && (start.bitLimit - start.bitPos < 32)) start.failed("Not enough bits to create an xs:float")
    else {
      val value = start.inStream.getFloat(start.bitPos, byteOrder)
      start.parent.addContent(new org.jdom.Text(value.toString))
      val postState = start.withPos(start.bitPos + 32, -1)
      postState
    }
  }
}

case class BigEndianFloatPrim(e: ElementBase) extends Terminal(e, true) {
  def parser = new FloatPrim(e, java.nio.ByteOrder.BIG_ENDIAN)
}

case class LittleEndianFloatPrim(e: ElementBase) extends Terminal(e, true) {
  def parser = new FloatPrim(e, java.nio.ByteOrder.LITTLE_ENDIAN)
}

class StaticDelimiter(delim: String, e: Term, guard: Boolean = true)
  extends StaticText(delim, e, guard)


abstract class StaticText(delim: String, e: Term, guard: Boolean = true) extends Terminal(e, guard) with Logging {
 
  lazy val es = e.escapeScheme
  lazy val esObj = EscapeScheme.getEscapeScheme(es, e)
  
  //e.asInstanceOf[Term].terminatingMarkup
  
  def parser: Parser = new Parser(e) {
    
    val t = e.asInstanceOf[Term]

    // TODO: Fix Cheezy matcher. Doesn't implement ignore case. Doesn't fail at first character that doesn't match. It grabs
    // the whole length (if it can), and then compares.
    // Also handles only one delimiter string. They can actually be whitespace-separated lists of alternative
    // delimiters
    // 
    Assert.notYetImplemented(e.ignoreCase == YesNo.Yes)

    Assert.invariant(delim != "") // shouldn't be here at all in this case.
    override def toString = "StaticText('" + delim + "' with terminating markup: " +  t.prettyTerminatingMarkup + ")"
    val decoder = e.knownEncodingDecoder
    val cbuf = CharBuffer.allocate(1024)

    def parse(start: PState): PState = {
    
      log(Debug("Parsing delimiter at byte position: " + (start.bitPos >> 3)))
      log(Debug("Parsing delimiter at bit position: " + start.bitPos))
      //val tm = t.terminatingMarkup.map(x => x.evaluate(start.parent, start.variableMap)).asInstanceOf[String].split("\\s").toList
      val tm = t.terminatingMarkup.map(x => x.evaluate(start.parent, start.variableMap).asInstanceOf[String].split("\\s").toList).flatten
     
      val delims = delim.split("\\s").toList ++ tm
      
      val separators = delim.split("\\s").toList
      val terminators = t.terminatingMarkup.map(x => x.evaluate(start.parent, start.variableMap).asInstanceOf[String].split("\\s").toList).flatten
      
      val separatorsCooked: Queue[String] = new Queue
      val terminatorsCooked: Queue[String] = new Queue
     
      separators.foreach(x => separatorsCooked.enqueue(EntityReplacer.replaceAll(x)))
      terminators.foreach(x => terminatorsCooked.enqueue(EntityReplacer.replaceAll(x)))
      
      //log(Debug("StaticText - Looking for: " + separators + " AND " + terminators))
      log(Debug("StaticText - Looking for: " + separatorsCooked + " AND " + terminatorsCooked))

      val in = start.inStream.asInstanceOf[InStreamFromByteChannel]
      //
      // Lots of things could go wrong in here. We might be looking at garbage, so decoding will get errors, etc.
      // Those should all count as "did not find the delimiter"
      //
      // No matter what goes wrong, we're counting on an orderly return here.
      //

      //var (resultStr, endBitPos, endBitPosDelim, theState, theMatchedDelim) = in.getDelimiter(cbuf, start.bitPos, decoder, separators.toSet, delims.toSet, esObj)
      var (resultStr, endBitPos, endBitPosDelim, theState, theMatchedDelim) = in.getDelimiter(cbuf, start.bitPos, decoder, separatorsCooked.toSet, terminatorsCooked.toSet, esObj)

      if (theMatchedDelim == null) {
        val postState = start.failed(this.toString() + ": Delimiter not found!")
        return postState
      }

      val delimRegex = theMatchedDelim.asInstanceOf[Delimiter].buildDelimRegEx()
      
      val p = Pattern.compile(delimRegex, Pattern.MULTILINE)

      val result =
        if (endBitPos == -1) "" // causes failure down below this
        else cbuf.toString

      // TODO: Is the below find even needed?  
      val m = p.matcher(result)
      if (m.find()) {
        // TODO: For numBytes, is length correct?!
        val numBytes = result.substring(m.start(), m.end()).getBytes(decoder.charset()).length
        val endCharPos = start.charPos + (m.end() - m.start())
        System.err.println("numBytes: " + numBytes)
        System.err.println("endCharPos: " + endCharPos)
        endBitPosDelim = (8 * numBytes) + start.bitPos // TODO: Is this correct?

        log(Debug("Found " + theMatchedDelim.toString()))
        log(Debug("Ended at byte position " + (endBitPosDelim >> 3)))
        log(Debug("Ended at bit position " + endBitPosDelim))

        val postState = start.withPos(endBitPosDelim, endCharPos)
        postState
      } else {
        val postState = start.failed(this.toString() + ": Delimiter not found!")
        postState
      }
    }
  }
}

class DynamicDelimiter(delimExpr: CompiledExpression, e: Term, guard: Boolean = true) extends Primitive(e, guard)

//case class StaticInitiator(e: Term) extends StaticDelimiter(e.initiator.constantAsString, e)
case class StaticInitiator(e: Term) extends StaticDelimiter(e.initiator.constantAsString, e)
//case class StaticTerminator(e : Term) extends StaticDelimiter(e.terminator.constantAsString, e)
case class StaticTerminator(e: Term) extends StaticDelimiter(e.terminator.constantAsString, e)
case class DynamicInitiator(e: Term) extends DynamicDelimiter(e.initiator, e)
case class DynamicTerminator(e: Term) extends DynamicDelimiter(e.terminator, e)

//case class StaticSeparator(e : Sequence) extends StaticDelimiter(e.separatorExpr.constantAsString, e)
//case class DynamicSeparator(e : Sequence) extends DynamicDelimiter(e.separatorExpr, e)

case class StartChildren(ct: ComplexTypeBase, guard: Boolean = true) extends Terminal(ct.element, guard) {
  def parser: Parser = new Parser(ct.element) {

    override def toString = "StartChildren"

    def parse(start: PState): PState = {
      val postState = start.withChildIndexStack(1L :: start.childIndexStack)
      postState
    }
  }
}

case class StartSequence(sq: Sequence, guard: Boolean = true) extends Terminal(sq, guard) {
  def parser: Parser = new Parser(sq) {

    override def toString = "StartSequence"

    def parse(start: PState): PState = {
      val postState = start.withGroupIndexStack(1L :: start.groupIndexStack)
      postState
    }
  }
}

case class Nada(sc: Term) extends Terminal(sc, true) {
  override def isEmpty = false 
  // cannot optimize this out! It is used as an alternative to things
  // with the intention of "find this and this, or find nothing"
  def parser: Parser = new Parser(sc) {

    override def toString = "Nothing"

    def parse(start: PState): PState = start
  }
}

case class GroupPosGreaterThan(n: Long, term: Term, guard: Boolean = true) extends Terminal(term, guard) {
  def parser: Parser = new Parser(term) {

    override def toString = "GroupPosGreaterThan(" + n + ")"

    def parse(start: PState): PState = {
      val res = if (start.groupPos > 1) {
        start
      } else {
        start.failed("Group position not greater than n (" + n + ")")
      }
      res
    }
  }
}

case class EndChildren(ct: ComplexTypeBase, guard: Boolean = true) extends Terminal(ct.element, guard) {
  def parser: Parser = new Parser(ct.element) {

    override def toString = "EndChildren"

    def parse(start: PState): PState = {
      val postState = start.withChildIndexStack(start.childIndexStack.tail)
      postState
    }
  }
}

case class EndSequence(sq: Sequence, guard: Boolean = true) extends Terminal(sq, guard) {
  def parser: Parser = new Parser(sq) {

    override def toString = "EndSequence"

    def parse(start: PState): PState = {
      val postState = start.withGroupIndexStack(start.groupIndexStack.tail)
      postState
    }
  }
}

case class StartArray(e: ElementBase, guard: Boolean = true) extends Terminal(e, guard) {
  def parser: Parser = new Parser(e) {

    override def toString = "StartArray"

    def parse(start: PState): PState = {
      val postState = start.withArrayIndexStack(1L :: start.arrayIndexStack)
      postState
    }
  }
}

case class EndArray(e: ElementBase, guard: Boolean = true) extends Terminal(e, guard) {
  def parser: Parser = new Parser(e) {

    override def toString = "EndArray"

    def parse(start: PState): PState = {
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

case class LiteralNilValue(e: ElementBase)
  extends StaticText(e.nilValue, e, e.isNillable) {
  val stParser = super.parser
  override def parser = new Parser(e) {
    def parse(start: PState): PState = {
      val afterNilLit = stParser.parse(start)

      if (afterNilLit.status != Success) start.failed("Doesn't match nil literal.")
      else {
        val xsiNS = afterNilLit.parent.getNamespace(XMLUtils.XSI_NAMESPACE)
        afterNilLit.parent.setAttribute("nil", "true", xsiNS)
        afterNilLit
      }
    }
  }
}

case class LogicalNilValue(e: ElementBase) extends Primitive(e, e.isNillable)

// As soon as you turn these on (by removing the false and putting the real guard), then schemas all need to have
// these properties in them, which is inconvenient until we have multi-file schema support and format references.
case class LeadingSkipRegion(e: Term) extends Primitive(e, false) // e.leadingSkip > 0)

case class AlignmentFill(e: Term) extends Primitive(e, false) // e.alignment != AlignmentType.Implicit)

case class TrailingSkipRegion(e: Term) extends Primitive(e, false) // e.trailingSkip > 0)

case class PrefixLength(e: ElementBase) extends Primitive(e, e.lengthKind == LengthKind.Prefixed)

case class UnicodeByteOrderMark(e: GlobalElementDecl) extends Primitive(e, false)

case class FinalUnusedRegion(e: ElementBase) extends Primitive(e, false)

case class InputValueCalc(e: ElementBase with ElementDeclMixin) extends Terminal(e, false) {

  def parser: Parser = new Parser(e) {
    override def toString = "InputValueCalc"
    val Some(ivcExprText) = e.inputValueCalcOption
    // Only for strings for now
    lazy val isString = {
      e.namedTypeQName match {
        case None => false
        case Some((ns, local)) => {
          val res = (local == "string" && ns == XMLUtils.XSD_NAMESPACE)
          res
        }
      }
    }
    Assert.notYetImplemented(!isString)
    val ivcExpr = e.expressionCompiler.compile('String, ivcExprText)

    def parse(start: PState): PState = {
      val currentElement = start.parent
      val result = ivcExpr.evaluate(currentElement, start.variableMap)
      val res = result.asInstanceOf[String] // only strings for now.
      currentElement.addContent(new org.jdom.Text(res))
      val postState = start // inputValueCalc consumes nothing. Just creates a value.
      postState
    }
  }
}
