package daffodil.grammar

import daffodil.dsom._
import daffodil.exceptions.Assert
import junit.framework.Assert.assertTrue
import daffodil.schema.annotation.props._
import daffodil.schema.annotation.props.gen._
import daffodil.xml._
import daffodil.processors._
import java.nio.CharBuffer
import java.nio.ByteBuffer
import java.nio.charset.CharsetEncoder
import java.nio.charset.CharsetDecoder
import com.ibm.icu.text._
import java.util.regex._
import java.text.{ ParseException, ParsePosition }
import java.math.BigInteger
import stringsearch.constructs._
import stringsearch.delimiter._

import daffodil.util._

case class ElementBegin(e: ElementBase) extends Terminal(e, true) {
  def parser: Parser = new Parser {
    override def toString = "<" + e.name + ">"

    /**
     * ElementBegin just adds the element we are constructing to the infoset and changes
     * the state to be referring to this new element as what we're parsing data into.
     */
    def parse(start: PState): PState = {
      val currentElement = new org.jdom.Element(e.name, e.namespace)
      val priorElement = start.parent
      priorElement.addContent(currentElement)
      val postState = start.withParent(currentElement)
      postState
    }
  }

  def unparser: Unparser = new Unparser {
    override def toString = "<" + e.name + ">"

    /**
     * Changes the state to refer to the next element in the infoset as the element to unparse.
     */
    def unparse(start: UState): UState = {
      val nextElement = {
        try { //if content contains elements
          if (!start.childIndexStack.isEmpty) {
            //TODO
            start.currentElement.getContent().get(start.childPos.asInstanceOf[Int] - 1).asInstanceOf[org.jdom.Element]
          } else {
            start.currentElement.getContent().get(0).asInstanceOf[org.jdom.Element]
          }
        } catch {
          case e: Exception => start.currentElement //if content is text
        }
      }
      //      System.err.println("Infoset root: " + start.root)
      //      System.err.println("nextElement: " + nextElement)

      val postState = start.withCurrent(nextElement).moveOverByOne
      postState
    }
  }
}

case class ElementEnd(e: ElementBase) extends Terminal(e, true) {
  def parser: Parser = new Parser {
    override def toString = "</" + e.name + ">"

    /**
     * ElementEnd just moves back to the parent element of the current one.
     */
    def parse(start: PState): PState = {
      val currentElement = start.parent
      val priorElement = currentElement.getParent.asInstanceOf[org.jdom.Element]
      val postState = start.withParent(priorElement).moveOverByOne
      postState
    }
  }

  def unparser: Unparser = new Unparser {
    override def toString = e.name

    /**
     * Changes state to refer to parent element of the current one.
     */
    def unparse(start: UState): UState = {
      val priorElement = {
        if (start.currentElement.getName != start.rootName)
          start.currentElement.getParentElement()
        else
          new org.jdom.Element("_document_") //dummy element to be parent of the root
      }
      val postState = start.withCurrent(priorElement)
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
  def parser: Parser = new Parser {
    override def toString = "StringFixedLengthInBytesParser(" + nBytes + ")"
    val decoder = e.knownEncodingDecoder
    val cbuf = CharBuffer.allocate(nBytes.toInt) // TODO: Performance: get a char buffer from a pool. 

    def parse(start: PState): PState = {
      log(Debug("Parsing starting at bit position: " + start.bitPos))
      val in = start.inStream
      val endBitPos = in.fillCharBuffer(cbuf, start.bitPos, decoder)
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

  def unparser: Unparser = new Unparser {
    override def toString = "StringFixedLengthInBytesUnparser(" + nBytes + ")"
    val encoder = e.knownEncodingEncoder
    var bbuf = ByteBuffer.allocate(nBytes.toInt) // TODO: Performance: get buffer from a pool. 

    def unparse(start: UState): UState = {
      log(Debug("Unparsing starting at bit position: " + start.bitPos))
      val data = start.currentElement.getText
      val endBitPos = start.outStream.fillOutStream(bbuf, start.bitPos, data, encoder)
      val result = bbuf.toString
      Assert.invariant(start.currentElement.getName != "_document_")
      log(Debug("Unparsed: " + result))
      log(Debug("Ended at bit position " + endBitPos))
      val endCharPos = start.charPos + result.length

      val postState = start.withPos(endBitPos, endCharPos)
      postState
    }
  }
}

case class StringFixedLengthInBytesVariableWidthCharacters(e: ElementBase, nBytes: Long) extends Terminal(e, true) {
  Assert.notYetImplemented()
  def parser: Parser = new Parser {
    def parse(start: PState): PState = {
      Assert.notYetImplemented()
    }
  }

  def unparser: Unparser = new Unparser {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

case class StringFixedLengthInVariableWidthCharacters(e: ElementBase, nChars: Long) extends Terminal(e, true) {
  Assert.notYetImplemented()
  def parser: Parser = new Parser {
    def parse(start: PState): PState = {
      Assert.notYetImplemented()
    }
  }

  def unparser: Unparser = new Unparser {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

case class StringDelimitedEndOfData(e: ElementBase) extends Terminal(e, true) with Logging {

  lazy val es = e.escapeScheme
  lazy val esObj = EscapeScheme.getEscapeScheme(es, e)
  lazy val tm = e.terminatingMarkup
  lazy val cname = toString

  def parser: Parser = new Parser {
    override def toString = cname + "(" + tm.map { _.prettyExpr } + ")"
    val decoder = e.knownEncodingDecoder
    var cbuf = CharBuffer.allocate(1024) // TODO: Performance: get a char buffer from a pool.

    def parse(start: PState): PState = {

      log(Debug(this.toString() + " - Parsing starting at bit position: " + start.bitPos))
      val in = start.inStream.asInstanceOf[InStreamFromByteChannel]
      var bitOffset = 0L

      val delimiters2 = e.terminatingMarkup.map(x => x.evaluate(start.parent, start.variableMap).asInstanceOf[String].split("\\s").toList)
      val delimiters = delimiters2.flatten(x => x)

      log(Debug("StringDelimitedEndOfData - Looking for: " + delimiters + " Count: " + delimiters.length))

      val (result, endBitPos, theState, theDelimiter) = in.fillCharBufferUntilDelimiterOrEnd(cbuf, start.bitPos, decoder, Set.empty, delimiters.toSet, esObj)
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

  def unparser: Unparser = new Unparser {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

case class StringPatternMatched(e: ElementBase) extends Terminal(e, true) with Logging {
  val sequenceSeparator = e.nearestEnclosingSequence.get.separator
  lazy val es = e.escapeScheme
  lazy val esObj = EscapeScheme.getEscapeScheme(es, e)

  def parser: Parser = new Parser {
    override def toString = "StringPatternMatched"
    val decoder = e.knownEncodingDecoder
    var cbuf = CharBuffer.allocate(1024)
    val pattern = e.lengthPattern

    // TODO: Add parameter for changing CharBuffer size

    def parse(start: PState): PState = {
      log(Debug("Parsing starting at bit position: " + start.bitPos))
      val in = start.inStream.asInstanceOf[InStreamFromByteChannel]
      var bitOffset = 0L

      val (result, endBitPos, theState) = in.fillCharBufferWithPatternMatch(cbuf, start.bitPos, decoder, pattern, esObj)

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

  def unparser: Unparser = new Unparser {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

abstract class ConvertTextNumberPrim[S](e: ElementBase, guard: Boolean) extends Terminal(e, guard) with Logging {
  protected def getNum(s: Number): S
  protected val GramName = "number"
  protected val GramDescription = "Number"
  protected def isInvalidRange(n: S): Boolean = false
  def parser: Parser = new Parser {
    override def toString = "ConvertTextNumberPrimUnparser"

    def parse(start: PState): PState = {
      val node = start.parent
      var str = node.getText

      val resultState = try {
        // Strip leading + (sign) since the DecimalFormat can't handle it
        if (str.charAt(0) == '+') {
          // TODO: There needs to be a way to restore '+' in the unparse, but that will be in the format field
          str = str.substring(1)
        }

        // Need to use Decimal Format to parse even though this is an Integral number
        val df = new DecimalFormat()
        val pos = new ParsePosition(0)
        val num = df.parse(str, pos)

        // Verify that what was parsed was what was passed exactly in byte count.  Use pos to verify all characters consumed & check for errors!
        if (pos.getIndex != str.length) {
          log(Debug("Error: Unable to parse all characters from " + GramDescription + ": " + str + "\n"))
          throw new ParseException("Error: Unable to parse all characters from " + GramDescription + ": " + str + "\n", 0)
        }

        // Assume long as the most precision
        val asNumber = getNum(num)

        // Verify no digits lost (the number was correctly transcribed)
        if (asNumber.asInstanceOf[Number] != num || isInvalidRange(asNumber)) {
          // Transcription error
          log(Debug("Error: Invalid " + GramDescription + ": " + str + "\n"))
          throw new ParseException("Error: Invalid " + GramDescription + ": " + str, 0)
        } else {
          node.setText(asNumber.toString)
        }

        start
      } catch { case e: Exception => start.failed("Failed to convert to an xs:" + GramName) }

      resultState
    }
  }

  def unparser: Unparser = new Unparser {
    override def toString = "ConvertTextNumberPrimUnparser"

    def unparse(start: UState): UState = {
      val node = start.currentElement
      var str = node.getText

      val resultState = try {
        // TODO: There needs to be a way to restore '+' in the unparse

        // Need to use Decimal Format to parse even though this is an Integral number
        val df = new DecimalFormat()
        val pos = new ParsePosition(0)
        val num = df.parse(str, pos)

        // Verify that what was unparsed was what was passed exactly in byte count.  Use pos to verify all characters consumed & check for errors!
        if (pos.getIndex != str.length) {
          System.err.print("Error: Unable to unparse all characters from " + GramDescription + ": " + str + "\n")
          throw new ParseException("Error: Unable to unparse all characters from " + GramDescription + ": " + str + "\n", 0)
        }

        // Assume long as the most precision
        val asNumber = getNum(num)

        // Verify no digits lost (the number was correctly transcribed)
        if (asNumber.asInstanceOf[Number] != num || isInvalidRange(asNumber)) {
          // Transcription error
          System.err.print("Error: Invalid " + GramDescription + ": " + str + "\n")
          throw new ParseException("Error: Invalid " + GramDescription + ": " + str, 0)
        }

        start
      } catch { case e: Exception => start.failed("Failed to convert to an xs:" + GramName) }

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
  def parser: Parser = new Parser {
    override def toString = "to(xs:double)"

    def parse(start: PState): PState = {
      val node = start.parent
      val str = node.getText

      val resultState = try {
        //convert to NumberFormat to handle format punctuation such as , . $ & etc
        //then get the value as a double and convert to string
        val df = new DecimalFormat()
        val pos = new ParsePosition(0)
        val num = df.parse(str, pos)
        node.setText(num.doubleValue.toString)

        start
      } catch { case e: Exception => start.failed("Failed to convert to an xs:double") }

      resultState
    }
  }

  def unparser: Unparser = new Unparser {
    override def toString = "to(double)"

    def unparse(start: UState): UState = {
      val node = start.currentElement
      val str = node.getText
      val resultState = try {
        //convert to NumberFormat to handle format punctuation such as , . $ & etc
        //then get the value as a double and convert to string
        val df = new DecimalFormat()
        val pos = new ParsePosition(0)
        val num = df.parse(str, pos)
        node.setText(num.doubleValue.toString)

        start
      } catch { case e: Exception => start.failed("Failed to convert to a double") }

      resultState
    }
  }
}

case class ConvertTextFloatPrim(e: ElementBase) extends Terminal(e, true) {
  def parser: Parser = new Parser {

    override def toString = "to(xs:float)"

    def parse(start: PState): PState = {
      val node = start.parent
      val str = node.getText

      val resultState = try {
        //convert to NumberFormat to handle format punctuation such as , . $ & etc
        //then get the value as a float and convert to string
        val df = new DecimalFormat()
        val pos = new ParsePosition(0)
        val num = df.parse(str, pos)
        node.setText(num.floatValue.toString)

        start
      } catch { case e: Exception => start.failed("Failed to convert to an xs:float") }

      resultState
    }
  }

  def unparser: Unparser = new Unparser {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

abstract class Primitive(e: PropertyMixin, guard: Boolean = false)
  extends Terminal(e, guard) {
  override def toString = "Prim[" + name + "]"
  def parser: Parser = DummyParser(e)
  def unparser: Unparser = DummyUnparser(e)
}

abstract class ZonedTextNumberPrim(e: ElementBase, guard: Boolean) extends Terminal(e, guard) {
  def parser: Parser = new Parser {
    def parse(start: PState): PState = {
      // TODO: Compute the Zoned Number generically
      start
    }
  }

  def unparser: Unparser = new Unparser {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}
case class ZonedTextBytePrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextShortPrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextIntPrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)
case class ZonedTextLongPrim(el: ElementBase) extends ZonedTextNumberPrim(el, false)

class Regular32bitIntPrim(byteOrder: java.nio.ByteOrder) extends Parser with Unparser {
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

  def unparse(start: UState): UState = {
    Assert.notYetImplemented()
  }
}

case class Regular32bitBigEndianIntPrim(e: ElementBase) extends Terminal(e, true) {
  def parser = new Regular32bitIntPrim(java.nio.ByteOrder.BIG_ENDIAN)
  def unparser = new Regular32bitIntPrim(java.nio.ByteOrder.BIG_ENDIAN)
}
case class Regular32bitLittleEndianIntPrim(e: ElementBase) extends Terminal(e, true) {
  def parser = new Regular32bitIntPrim(java.nio.ByteOrder.LITTLE_ENDIAN)
  def unparser = new Regular32bitIntPrim(java.nio.ByteOrder.LITTLE_ENDIAN)
}
case class PackedIntPrim(e: ElementBase) extends Primitive(e, false)
case class BCDIntPrim(e: ElementBase) extends Primitive(e, false)

case class DoublePrim(byteOrder: java.nio.ByteOrder) extends Parser with Unparser with Logging {
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

  def unparse(start: UState): UState = {
    Assert.notYetImplemented()
  }
}

case class BigEndianDoublePrim(e: ElementBase) extends Terminal(e, true) {
  def parser = new DoublePrim(java.nio.ByteOrder.BIG_ENDIAN)
  def unparser = new DoublePrim(java.nio.ByteOrder.BIG_ENDIAN)
}

case class LittleEndianDoublePrim(e: ElementBase) extends Terminal(e, true) {
  def parser = new DoublePrim(java.nio.ByteOrder.LITTLE_ENDIAN)
  def unparser = new DoublePrim(java.nio.ByteOrder.LITTLE_ENDIAN)
}

case class FloatPrim(byteOrder: java.nio.ByteOrder) extends Parser with Unparser {
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

  def unparse(start: UState): UState = {
    Assert.notYetImplemented()
  }
}

case class BigEndianFloatPrim(e: ElementBase) extends Terminal(e, true) {
  def parser = new FloatPrim(java.nio.ByteOrder.BIG_ENDIAN)
  def unparser = new FloatPrim(java.nio.ByteOrder.BIG_ENDIAN)
}

case class LittleEndianFloatPrim(e: ElementBase) extends Terminal(e, true) {
  def parser = new FloatPrim(java.nio.ByteOrder.LITTLE_ENDIAN)
  def unparser = new FloatPrim(java.nio.ByteOrder.LITTLE_ENDIAN)
}

class StaticDelimiter(delim: String, e: InitiatedTerminatedMixin, guard: Boolean = true)
  extends StaticText(delim, e, guard)

abstract class StaticText(delim: String, e: InitiatedTerminatedMixin, guard: Boolean = true) extends Terminal(e, guard) with Logging {

  lazy val es = e.escapeScheme
  lazy val esObj = EscapeScheme.getEscapeScheme(es, e)
  e.asInstanceOf[Term].terminatingMarkup

  def parser: Parser = new Parser {

    val t = e.asInstanceOf[Term]

    // TODO: Fix Cheezy matcher. Doesn't implement ignore case. Doesn't fail at first character that doesn't match. It grabs
    // the whole length (if it can), and then compares.
    // Also handles only one delimiter string. They can actually be whitespace-separated lists of alternative
    // delimiters
    // 
    Assert.notYetImplemented(e.ignoreCase == YesNo.Yes)

    Assert.invariant(delim != "") // shouldn't be here at all in this case.
    override def toString = "StaticText('" + delim + "' with terminating markup: " + t.prettyTerminatingMarkup + ")"
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

      log(Debug("StaticText - Looking for: " + separators + " AND " + terminators))

      val in = start.inStream.asInstanceOf[InStreamFromByteChannel]
      //
      // Lots of things could go wrong in here. We might be looking at garbage, so decoding will get errors, etc.
      // Those should all count as "did not find the delimiter"
      //
      // No matter what goes wrong, we're counting on an orderly return here.
      //

      var (resultStr, endBitPos, endBitPosDelim, theState, theMatchedDelim) = in.getDelimiter(cbuf, start.bitPos, decoder, separators.toSet, delims.toSet, esObj)

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
        val numBytes = result.substring(m.start(), m.end()).getBytes().length
        val endCharPos = start.charPos + (m.end() - m.start())
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

  def unparser: Unparser = new Unparser {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

class DynamicDelimiter(delimExpr: CompiledExpression, e: InitiatedTerminatedMixin, guard: Boolean = true) extends Primitive(e, guard)

//case class StaticInitiator(e: InitiatedTerminatedMixin) extends StaticDelimiter(e.initiator.constantAsString, e)
case class StaticInitiator(e: InitiatedTerminatedMixin) extends StaticDelimiter(e.initiator.constantAsString, e)
//case class StaticTerminator(e : InitiatedTerminatedMixin) extends StaticDelimiter(e.terminator.constantAsString, e)
case class StaticTerminator(e: InitiatedTerminatedMixin) extends StaticDelimiter(e.terminator.constantAsString, e)
case class DynamicInitiator(e: InitiatedTerminatedMixin) extends DynamicDelimiter(e.initiator, e)
case class DynamicTerminator(e: InitiatedTerminatedMixin) extends DynamicDelimiter(e.terminator, e)

//case class StaticSeparator(e : Sequence) extends StaticDelimiter(e.separatorExpr.constantAsString, e)
//case class DynamicSeparator(e : Sequence) extends DynamicDelimiter(e.separatorExpr, e)

case class StartChildren(ct: ComplexTypeBase, guard: Boolean = true) extends Terminal(ct, guard) {
  def parser: Parser = new Parser {
    override def toString = "StartChildren"

    def parse(start: PState): PState = {
      val postState = start.withChildIndexStack(1L :: start.childIndexStack)
      postState
    }
  }

  def unparser: Unparser = new Unparser {
    override def toString = "StartChildren"

    def unparse(start: UState): UState = {
      val postState = start.withChildIndexStack(1L :: start.childIndexStack)
      postState
    }
  }
}

case class StartSequence(sq: Sequence, guard: Boolean = true) extends Terminal(sq, guard) {
  def parser: Parser = new Parser {
    override def toString = "StartSequence"

    def parse(start: PState): PState = {
      val postState = start.withGroupIndexStack(1L :: start.groupIndexStack)
      postState
    }
  }

  def unparser: Unparser = new Unparser {
    override def toString = "StartSequence"

    def unparse(start: UState): UState = {
      val postState = start.withGroupIndexStack(1L :: start.groupIndexStack)
      postState
    }
  }
}

/**
 * Scala has a standard type Nothing, so we use Spanish. Nada means nothing in Spanish.
 */
case class Nada(sc: SchemaComponent) extends Terminal(sc, true) {
  override def isEmpty = false 
  // cannot optimize this out! It is used as an alternative to things
  // with the intention of "find this and this, or find nothing"
  def parser: Parser = new Parser {
    override def toString = "Nada"

    def parse(start: PState): PState = start
  }

  def unparser: Unparser = new Unparser {
    override def toString = "Nada"

    def unparse(start: UState): UState = start
  }
}

case class GroupPosGreaterThan(n: Long, term: Term, guard: Boolean = true) extends Terminal(term, guard) {
  def parser: Parser = new Parser {
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

  def unparser: Unparser = new Unparser {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

case class EndChildren(ct: ComplexTypeBase, guard: Boolean = true) extends Terminal(ct, guard) {
  def parser: Parser = new Parser {
    override def toString = "EndChildren"

    def parse(start: PState): PState = {
      val postState = start.withChildIndexStack(start.childIndexStack.tail)
      postState
    }
  }

  def unparser: Unparser = new Unparser {
    override def toString = "EndChildren"

    def unparse(start: UState): UState = {
      val postState = start.withChildIndexStack(start.childIndexStack.tail)
      postState
    }
  }
}

case class EndSequence(sq: Sequence, guard: Boolean = true) extends Terminal(sq, guard) {
  def parser: Parser = new Parser {
    override def toString = "EndSequence"

    def parse(start: PState): PState = {
      val postState = start.withGroupIndexStack(start.groupIndexStack.tail)
      postState
    }
  }

  def unparser: Unparser = new Unparser {
    override def toString = "EndSequence"

    def unparse(start: UState): UState = {
      val postState = start.withGroupIndexStack(start.groupIndexStack.tail)
      postState
    }
  }
}

case class StartArray(e: ElementBase, guard: Boolean = true) extends Terminal(e, guard) {
  def parser: Parser = new Parser {

    override def toString = "StartArray"

    def parse(start: PState): PState = {
      val postState = start.withArrayIndexStack(1L :: start.arrayIndexStack)
      postState
    }
  }

  def unparser: Unparser = new Unparser {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

case class EndArray(e: ElementBase, guard: Boolean = true) extends Terminal(e, guard) {
  def parser: Parser = new Parser {

    override def toString = "EndArray"

    def parse(start: PState): PState = {
      val postState = start.withArrayIndexStack(start.arrayIndexStack.tail)
      postState
    }
  }

  def unparser: Unparser = new Unparser {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
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
  override def parser = new Parser {
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

  override def unparser: Unparser = new Unparser {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

case class LogicalNilValue(e: ElementBase) extends Primitive(e, e.isNillable)

// As soon as you turn these on (by removing the false and putting the real guard), then schemas all need to have
// these properties in them, which is inconvenient until we have multi-file schema support and format references.
case class LeadingSkipRegion(e: AnnotatedMixin) extends Primitive(e, false) // e.leadingSkip > 0) 

case class AlignmentFill(e: AnnotatedMixin) extends Primitive(e, false) // e.alignment != AlignmentType.Implicit)

case class TrailingSkipRegion(e: AnnotatedMixin) extends Primitive(e, false) // e.trailingSkip > 0)

case class PrefixLength(e: ElementBase) extends Primitive(e, e.lengthKind == LengthKind.Prefixed)

case class UnicodeByteOrderMark(e: GlobalElementDecl) extends Primitive(e, false)

case class FinalUnusedRegion(e: ElementBase) extends Primitive(e, false)

case class InputValueCalc(e: ElementBase with ElementDeclMixin) extends Terminal(e, false) {

  def parser: Parser = new Parser {
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

  def unparser: Unparser = new Unparser {
    def unparse(start: UState): UState = {
      Assert.notYetImplemented()
    }
  }
}

