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
import java.text.{ParseException, ParsePosition}
import java.math.BigInteger
import stringsearch.DelimSearcherV3.SearchResult._

case class ElementBegin(e : ElementBaseMixin) extends Terminal(e, true) {
     def parser: Parser = new Parser {
     
      override def toString = "<" + e.name + ">"
      
      /**
       * ElementBegin just adds the element we are constructing to the infoset and changes
       * the state to be referring to this new element as what we're parsing data into.
       */
      def parse(start : PState) : PState = {
        val currentElement = new org.jdom.Element(e.name, e.namespace)
        val priorElement = start.parent
        priorElement.addContent(currentElement)
    	val postState = start.withParent(currentElement)
    	postState
      }
     }
}

case class ElementEnd(e : ElementBaseMixin) extends Terminal(e, true) {
   def parser: Parser = new Parser {
     
      override def toString = "</" + e.name + ">"
      
      /**
       * ElementEnd just moves back to the parent element of the current one.
       */
      def parse(start : PState) : PState = {
        val currentElement = start.parent
        val priorElement = currentElement.getParent.asInstanceOf[org.jdom.Element]
    	val postState = start.withParent(priorElement).moveOverByOne
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


case class StringFixedLengthInBytes(e : ElementBaseMixin, nBytes : Long) extends Terminal(e, true) {
   def parser: Parser = new Parser {
     
      override def toString = "StringFixedLengthInBytesParser"
      val decoder = e.knownEncodingDecoder
      val cbuf = CharBuffer.allocate(nBytes.toInt) // TODO: Performance: get a char buffer from a pool. 
      
      def parse(start : PState) : PState = {
        System.err.println("Parsing starting at bit position: " + start.bitPos)
    	val in = start.inStream
    	val endBitPos = in.fillCharBuffer(cbuf, start.bitPos, decoder) 
    	val result = cbuf.toString
    	System.err.println("Parsed: " + result)
    	System.err.println("Ended at bit position " + endBitPos)
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

case class StringFixedLengthInBytesVariableWidthCharacters(e : ElementBaseMixin, nBytes : Long) extends Terminal(e, true) {
   def parser: Parser = new Parser {
      def parse(start : PState) : PState = {
         Assert.notYetImplemented()
      }
     
   }
}

case class StringFixedLengthInVariableWidthCharacters(e : ElementBaseMixin, nChars : Long) extends Terminal(e, true) {
   def parser: Parser = new Parser {
     def parse(start : PState) : PState = {
        Assert.notYetImplemented()
     }
   }
}

case class StringDelimited(e : LocalElementBase) extends Terminal(e, true) {
  lazy val tm = e.terminatingMarkup
  // TODO: Guard should insist all delimiters are constants if that is what is requried.
  lazy val tmStrings = tm.map {_.constantAsString} // TODO: allow these to be evaluated, not constant.
  lazy val tmStringLengths = tmStrings.map{_.length}
  lazy val maxLen = tmStringLengths.foldLeft(0) {(a,b) => math.max(a,b)}
  lazy val minLen = tmStringLengths.foldLeft(maxLen) {(a,b) => math.min(a,b)}
  Assert.invariant(maxLen > 0) // TODO: SDE
  
  val orderedStrings = tmStrings.sortWith((a, b) => a.length < b.length)
  
  val quotedStrings = orderedStrings.map {quoteForRegEx(_)}
  val regEx = quotedStrings.foldLeft("") {_+"|"+_}
  val pattern = java.util.regex.Pattern.compile(regEx)
  
  def quoteForRegEx(string : String) = {
    "\\Q" + string + "\\E"
  }
  
  //TODO: take DFDL EscapeSchemes into account - "escape" type and "block" type

  def parser: Parser = new Parser {
    override def toString = "StringDelimited"
    val decoder = e.knownEncodingDecoder
    val cbuf = CharBuffer.allocate(maxLen) // TODO: Performance: get a char buffer from a pool.

    def parse(start: PState): PState = {
      Assert.notYetImplemented()
//      System.err.println("Parsing starting at bit position: " + start.bitPos)
//      val in = start.inStream
//      val endBitPos = in.fillCharBuffer(cbuf, start.bitPos, decoder)
//      System.err.println("Ended at bit position " + endBitPos)
//      cbuf.flip()
//      if (cbuf.length < minLen) {
//        start.failed("Delimiter not found")
//      } else {
//        val result = cbuf.toString
//        System.err.println("Parsed: " + result)
//        val matcher = pattern.matcher(result)
//        if (!matcher.matches) start.failed("Delimiter not found")
//        else {
//          val matchLength = matcher.end() - matcher.start()
//          if (matchLength == maxLen) {
//            Assert.invariant(endBitPos != -1L)
//            val postState = start.withPos(endBitPos, start.charPos + matchLength)
//            postState
//          } else {
//            val shortCBuf = CharBuffer.allocate(matchLength)
//            val endBitPos = in.fillCharBuffer(shortCBuf, start.bitPos, decoder)
//            System.err.println("Ended at bit position " + endBitPos)
//            Assert.invariant(endBitPos != -1L)
//            val postState = start.withPos(endBitPos, start.charPos + matchLength)
//            postState
//          }
//        }
//      }
    }
  }
}

//case class StringDelimitedNoEscapeSchemeNoTerminator(e : LocalElementBase) extends Terminal(e, true) {
//	val sequenceSeparator = e.nearestEnclosingSequence.get.separator
//	
//  def parser: Parser = new Parser {
//    override def toString = "StringDelimitedNoEscapeSchemeNoTerminator"
//    val decoder = e.knownEncodingDecoder
//    var cbuf = CharBuffer.allocate(1024) // TODO: Performance: get a char buffer from a pool.
//
//    def parse(start: PState): PState = {
//     
//      val in = start.inStream.asInstanceOf[InStreamFromByteChannel]
//      var bitOffset = 0L
//      
//      val (result, endBitPos) = in.fillCharBufferUntilDelimiterOrEnd(cbuf, start.bitPos, decoder, Set(sequenceSeparator.constantAsString))
//
//      val endCharPos = start.charPos + result.length()
//      val currentElement = start.parent
//      currentElement.addContent(new org.jdom.Text(result))
//      val postState = start.withPos(endBitPos, endCharPos)
//
//      postState
//    }
//  }
//}

case class StringDelimitedNoEscapeSchemeNoTerminator(e : LocalElementBase) extends Terminal(e, true) {
	val sequenceSeparator = e.nearestEnclosingSequence.get.separator
	
  def parser: Parser = new Parser {
    override def toString = "StringDelimitedNoEscapeSchemeNoTerminator"
    val decoder = e.knownEncodingDecoder
    var cbuf = CharBuffer.allocate(1024) // TODO: Performance: get a char buffer from a pool.
    //var cbuf = CharBuffer.allocate(4)

    def parse(start: PState): PState = {
      System.err.println("Parsing starting at bit position: " + start.bitPos)
      val in = start.inStream.asInstanceOf[InStreamFromByteChannel]
      var bitOffset = 0L
      
      println("sequenceSeparator: " + sequenceSeparator.constantAsString)
      
      val (result, endBitPos, theState) = in.fillCharBufferUntilDelimiterOrEnd(cbuf, start.bitPos, decoder, Set(sequenceSeparator.constantAsString))
      System.err.println("Parsed: " + result)
      System.err.println("Ended at bit position " + endBitPos)
      val endCharPos = start.charPos + result.length()
      val currentElement = start.parent
      currentElement.addContent(new org.jdom.Text(result))
      val postState = start.withPos(endBitPos, endCharPos)

      postState
    }
  }
}

case class StringDelimitedNoEscapeSchemeWithTerminator(e : LocalElementBase) extends Terminal(e, true) {
  val sequenceSeparator = e.nearestEnclosingSequence.get.separator
  val terminator = e.terminatingMarkup.map(x => x.constantAsString)

  def parser: Parser = new Parser {
    override def toString = "StringDelimitedNoEscapeSchemeWithTerminator"
    val decoder = e.knownEncodingDecoder
    var cbuf = CharBuffer.allocate(1024)
    //var cbuf = CharBuffer.allocate(4)
    
    // TODO: Add parameter for changing CharBuffer size
    // TODO: Change EOF to NoMatch

    def parse(start: PState): PState = {
      System.err.println("Parsing starting at bit position: " + start.bitPos)
      val in = start.inStream.asInstanceOf[InStreamFromByteChannel]
      var bitOffset = 0L

      val (result, endBitPos, theState) = in.fillCharBufferUntilDelimiterOrEnd(cbuf, start.bitPos, decoder, terminator.toSet)

      val postState =  theState match {
        case EOF  => start.failed(this.toString() + ": No match found!")
        case PartialMatch => start.failed(this.toString() + ": Partial match found!")
        case FullMatch => {
          System.err.println("Parsed: " + result)
          System.err.println("Ended at bit position " + endBitPos)
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

abstract class ConvertTextNumberPrim[S](e: ElementBaseMixin, guard: Boolean) extends Terminal(e, guard) {
  protected def getNum(s: Number) : S
  protected val GramName = "number"
  protected val GramDescription = "Number"
  protected def isInvalidRange(n: S) : Boolean = false
  def parser : Parser = new Parser {
    def parse(start: PState) : PState = {
      val node = start.parent
      var str = node.getText

      val resultState =  try {
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
          System.err.print("Error: Unable to parse all characters from " + GramDescription + ": " + str + "\n")
          throw new ParseException("Error: Unable to parse all characters from " + GramDescription + ": " + str + "\n", 0)
        }

        // Assume long as the most precision
        val asNumber = getNum(num)

        // Verify no digits lost (the number was correctly transcribed)
        if (asNumber.asInstanceOf[Number] != num || isInvalidRange(asNumber)) {
          // Transcription error
          System.err.print("Error: Invalid " + GramDescription + ": " + str + "\n")
          throw new ParseException("Error: Invalid " + GramDescription + ": " + str, 0)
        }
        else {
          node.setText(asNumber.toString)
        }

        start
      } catch {case e:Exception => start.failed("Failed to convert to an xs:" + GramName) }

      resultState
    }
  }

}

case class ConvertTextIntegerPrim(e : ElementBaseMixin) extends ConvertTextNumberPrim[BigInteger](e, true) {
  protected override def getNum(num: Number) = new BigInteger(num.toString)
  protected override val GramName = "integer"
  protected override val GramDescription = "Unbounded Integer"
}

case class ConvertTextLongPrim(e : ElementBaseMixin) extends ConvertTextNumberPrim[Long](e, true) {
  protected override def getNum(num: Number) = num.longValue
  protected override val GramName = "long"
  protected override val GramDescription = "Long Integer"
}

case class ConvertTextIntPrim(e : ElementBaseMixin) extends ConvertTextNumberPrim[Int](e, true) {
  protected override def getNum(num: Number) = num.intValue
  protected override val GramName = "int"
  protected override val GramDescription = "Integer"
}

case class ConvertTextShortPrim(e : ElementBaseMixin) extends ConvertTextNumberPrim[Short](e, true) {
  protected override def getNum(num: Number) = num.shortValue
  protected override val GramName = "short"
  protected override val GramDescription = "Short Integer"
}

case class ConvertTextBytePrim(e : ElementBaseMixin) extends ConvertTextNumberPrim[Byte](e, true) {
  protected override def getNum(num: Number) = num.byteValue
  protected override val GramName = "byte"
  protected override val GramDescription = "Byte"
}

case class ConvertTextUnsignedLongPrim(e: ElementBaseMixin) extends ConvertTextNumberPrim[BigInteger](e, true) {
  protected override def getNum(num: Number) = new BigInteger(num.toString)
  protected override val GramName = "unsignedLong"
  protected override val GramDescription = "Unsigned Long"
  protected override def isInvalidRange(n : BigInteger) = n.compareTo(BigInteger.ZERO) < 0 || n.compareTo(BigInteger.ONE.shiftLeft(64)) >= 0
}

case class ConvertTextUnsignedIntPrim(e: ElementBaseMixin) extends ConvertTextNumberPrim[Long](e, true) {
  protected override def getNum(num: Number) = num.longValue
  protected override val GramName = "unsignedInt"
  protected override val GramDescription = "Unsigned Int"
  protected override def isInvalidRange(n : Long) = n < 0 || n >= (1L<<32)
}

case class ConvertTextUnsignedShortPrim(e: ElementBaseMixin) extends ConvertTextNumberPrim[Int](e, true) {
  protected override def getNum(num: Number) = num.intValue
  protected override val GramName = "unsignedShort"
  protected override val GramDescription = "Unsigned Short"
  protected override def isInvalidRange(n : Int) = n < 0 || n >= (1<<16)
}

case class ConvertTextUnsignedBytePrim(e: ElementBaseMixin) extends ConvertTextNumberPrim[Short](e, true) {
  protected override def getNum(num: Number) = num.shortValue
  protected override val GramName = "unsignedByte"
  protected override val GramDescription = "Unsigned Byte"
  protected override def isInvalidRange(n : Short) = n < 0 || n >= (1<<8)
}

case class ConvertTextDoublePrim(e: ElementBaseMixin) extends Terminal(e, true) {
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
}

case class ConvertTextFloatPrim(e: ElementBaseMixin) extends Terminal(e, true) {
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
}


abstract class Primitive(e: PropertyMixin, guard: Boolean = false) 
extends Terminal(e, guard) {
    override def toString = "Prim[" + name + "]"
    def parser: Parser = DummyParser(e)
  }

abstract class ZonedTextNumberPrim(e: ElementBaseMixin, guard: Boolean) extends Terminal(e, guard) {
  def parser : Parser = new Parser {
    def parse(start: PState) : PState = {
      // TODO: Compute the Zoned Number generically
      start
    }
  }
}
case class ZonedTextBytePrim(el : ElementBaseMixin) extends ZonedTextNumberPrim(el, false)
case class ZonedTextShortPrim(el : ElementBaseMixin) extends ZonedTextNumberPrim(el, false)
case class ZonedTextIntPrim(el : ElementBaseMixin) extends ZonedTextNumberPrim(el, false)
case class ZonedTextLongPrim(el : ElementBaseMixin) extends ZonedTextNumberPrim(el, false)

class Regular32bitIntPrim(byteOrder: java.nio.ByteOrder) extends Parser {
	override def toString = "binary(xs:int, " + byteOrder + ")"
    def parse(start : PState) : PState = {
      if (start.bitLimit != -1L && (start.bitLimit - start.bitPos < 32)) start.failed("Not enough bits to create an xs:int")
      else {
        val value = start.inStream.getInt(start.bitPos, byteOrder)
        start.parent.addContent(new org.jdom.Text(value.toString))
    	val postState = start.withPos(start.bitPos + 32, -1)
    	postState
      }
    }
  
}

case class Regular32bitBigEndianIntPrim(e : ElementBaseMixin) extends Terminal(e, true) {
  def parser = new Regular32bitIntPrim(java.nio.ByteOrder.BIG_ENDIAN)
}
case class Regular32bitLittleEndianIntPrim(e : ElementBaseMixin) extends Terminal(e, true) {
   def parser = new Regular32bitIntPrim(java.nio.ByteOrder.LITTLE_ENDIAN)
}
case class PackedIntPrim(e : ElementBaseMixin) extends Primitive(e, false)
case class BCDIntPrim(e : ElementBaseMixin) extends Primitive(e, false)


case class DoublePrim(byteOrder: java.nio.ByteOrder) extends Parser {
  override def toString = "binary(xs:double, " + byteOrder + ")"
  def parse(start : PState) : PState = {
    if (start.bitLimit != -1L && (start.bitLimit - start.bitPos < 64)) start.failed("Not enough bits to create an xs:double")
    else {
      val value = start.inStream.getDouble(start.bitPos, byteOrder)
      start.parent.addContent(new org.jdom.Text(value.toString))
      val postState = start.withPos(start.bitPos + 64, -1)
      postState
    }
  }
}

case class BigEndianDoublePrim(e : ElementBaseMixin) extends Terminal(e, true) {
  def parser = new DoublePrim(java.nio.ByteOrder.BIG_ENDIAN)
}

case class LittleEndianDoublePrim(e : ElementBaseMixin) extends Terminal(e, true) {
  def parser = new DoublePrim(java.nio.ByteOrder.LITTLE_ENDIAN)
}


case class FloatPrim(byteOrder: java.nio.ByteOrder) extends Parser {
  override def toString = "binary(xs:float, " + byteOrder + ")"
  def parse(start : PState) : PState = {
    if (start.bitLimit != -1L && (start.bitLimit - start.bitPos < 32)) start.failed("Not enough bits to create an xs:float")
    else {
      val value = start.inStream.getFloat(start.bitPos, byteOrder)
      start.parent.addContent(new org.jdom.Text(value.toString))
      val postState = start.withPos(start.bitPos + 32, -1)
      postState
    }
  }
}

case class BigEndianFloatPrim(e : ElementBaseMixin) extends Terminal(e, true) {
  def parser = new FloatPrim(java.nio.ByteOrder.BIG_ENDIAN)
}

case class LittleEndianFloatPrim(e : ElementBaseMixin) extends Terminal(e, true) {
  def parser = new FloatPrim(java.nio.ByteOrder.LITTLE_ENDIAN)
}


class StaticDelimiter(delim: String, e: AnnotatedMixin, guard: Boolean = true) extends Terminal(e, guard) {
  def parser: Parser = new Parser {

    // TODO: Fix Cheezy matcher. Doesn't implement ignore case. Doesn't fail at first character that doesn't match. It grabs
    // the whole length (if it can), and then compares.
    // Also handles only one delimiter string. They can actually be whitespace-separated lists of alternative
    // delimiters
    // 
    Assert.notYetImplemented(e.ignoreCase == YesNo.Yes)

    Assert.invariant(delim != "") // shouldn't be here at all in this case.
    override def toString = "StaticDelimiter(" + delim + ")"
    val decoder = e.knownEncodingDecoder
    //val cbuf = CharBuffer.allocate(delim.length) // TODO: Performance: get a char buffer from a pool. 
    val cbuf = CharBuffer.allocate(1024)

    def parse(start: PState): PState = {
      //System.err.println("Parsing delimiter at bit position: " + start.bitPos)
      //val in = start.inStream
      val in = start.inStream.asInstanceOf[InStreamFromByteChannel]
      //
      // Lots of things could go wrong in here. We might be looking at garbage, so decoding will get errors, etc.
      // Those should all count as "did not find the delimiter"
      //
      // No matter what goes wrong, we're counting on an orderly return here.
      //
      //val endBitPos = in.fillCharBuffer(cbuf, start.bitPos, decoder)
      println("startBitPos: " + start.bitPos)
      //var (resultStr, endBitPos) = in.fillCharBufferUntilDelimiterOrEnd(cbuf, start.bitPos, decoder, Set(delim))
      var (resultStr, endBitPos, endBitPosDelim, theState) = in.getDelimiter(cbuf, start.bitPos, decoder, Set(delim))
      
      //println("resultSTR:" + resultStr)
     
      println("BUF: " + cbuf.toString + " ENDBITPOS: " + endBitPos + " ENDBITPOSDELIM: " + endBitPosDelim)
      
      println("CBUF LENGTH: " + cbuf.toString().length())
      
     
      val d = new stringsearch.delimiter.Delimiter
      
      d(delim)
      val delimRegex = d.buildDelimRegEx()
      println("delimRegex: " + delimRegex)
      val p = Pattern.compile(delimRegex, Pattern.MULTILINE)
      
      
      val result = 
        if (endBitPos == -1) "" // causes failure down below this
        else cbuf.toString
        
      val m = p.matcher(result)
      if (m.find()){ 
      //if (result == delim) { // TODO: use string.compare and string.compareIgnoreCase so we can implement ignoreCase property.
        System.err.println("Found " + delim)
        System.err.println("Ended at bit position " + endBitPosDelim)
        //val endCharPos = start.charPos + result.length
        println("charPos: " + start.charPos + " length: " + (m.end() - m.start()))
        val endCharPos = start.charPos + (m.end() - m.start())
        endBitPosDelim = 8 * endCharPos // TODO: Is this correct?
        val postState = start.withPos(endBitPosDelim, endCharPos)
        println("endCharPos: " + endCharPos)
        postState
      } else {
        //val postState = start.withPos(start.bitPos, start.charPos, new Failure("Delimiter not found"))
        //postState
        val postState = start.failed(this.toString() + ": Delimiter not found!")
        postState
      }
    }
  }
}

class DynamicDelimiter(delimExpr : CompiledExpression, e: AnnotatedMixin, guard: Boolean = true) extends Primitive(e, guard)

case class StaticInitiator(e : InitiatedTerminatedMixin) extends StaticDelimiter(e.initiator.constantAsString, e)
case class StaticTerminator(e : InitiatedTerminatedMixin) extends StaticDelimiter(e.terminator.constantAsString, e)
case class DynamicInitiator(e : InitiatedTerminatedMixin) extends DynamicDelimiter(e.initiator, e)
case class DynamicTerminator(e : InitiatedTerminatedMixin) extends DynamicDelimiter(e.terminator, e)

//case class StaticSeparator(e : Sequence) extends StaticDelimiter(e.separatorExpr.constantAsString, e)
//case class DynamicSeparator(e : Sequence) extends DynamicDelimiter(e.separatorExpr, e)

case class StartChildren(ct: ComplexTypeBase, guard: Boolean = true) extends Terminal(ct, guard) {
    def parser: Parser = new Parser {
     
      override def toString = "StartChildren"

      def parse(start : PState) : PState = {
    	val postState = start.withChildIndexStack(1L :: start.childIndexStack)
    	postState
      }
   }
}

case class StartSequence(sq : Sequence, guard: Boolean = true) extends Terminal(sq, guard) {
    def parser: Parser = new Parser {
     
      override def toString = "StartSequence"

      def parse(start : PState) : PState = {
    	val postState = start.withGroupIndexStack(1L :: start.groupIndexStack)
    	postState
      }
   }
}

case class Nothing(sc : SchemaComponent) extends Terminal(sc, true) {
      def parser: Parser = new Parser {
     
      override def toString = "Nothing"

      def parse(start : PState) : PState = start
   } 
}

case class GroupPosGreaterThan(n: Long, term: Term, guard: Boolean = true) extends Terminal(term, guard) {
     def parser: Parser = new Parser {
     
      override def toString = "GroupPosGreaterThan(" + n + ")"

      def parse(start : PState) : PState = {
    	val res = if (start.groupPos > 1) {
    	  start
    	}
    	else {
    	  start.failed("Group position not greater than n (" + n + ")")
    	}
    	res
     }
   }
}

case class EndChildren(ct: ComplexTypeBase, guard: Boolean = true)  extends Terminal(ct, guard) {
    def parser: Parser = new Parser {
     
      override def toString = "EndChildren"

      def parse(start : PState) : PState = {
    	val postState = start.withChildIndexStack(start.childIndexStack.tail)
    	postState
      }
   }
}

case class EndSequence(sq : Sequence, guard: Boolean = true)  extends Terminal(sq, guard) {
    def parser: Parser = new Parser {
     
      override def toString = "EndSequence"

      def parse(start : PState) : PState = {
    	val postState = start.withGroupIndexStack(start.groupIndexStack.tail)
    	postState
      }
   }
}
  
case class StartArray(e: LocalElementBase, guard: Boolean = true) extends Terminal(e, guard) {
    def parser: Parser = new Parser {
     
      override def toString = "StartArray"

      def parse(start : PState) : PState = {
    	val postState = start.withArrayIndexStack(1L :: start.arrayIndexStack)
    	postState
      }
   }
}
  
case class EndArray(e: LocalElementBase, guard: Boolean = true)   extends Terminal(e, guard) {
    def parser: Parser = new Parser {
     
      override def toString = "EndArray"

      def parse(start : PState) : PState = {
    	val postState = start.withArrayIndexStack(start.arrayIndexStack.tail)
    	postState
      }
   }
}

case class NoValue(e: GlobalElementDecl, guard: Boolean = true) extends Primitive(e, guard) 
 
case class SaveInputStream(e: ElementBaseMixin, guard: Boolean = true) extends Primitive(e, guard) 
 
case class SetEmptyInputStream(e: ElementBaseMixin, guard: Boolean = true) extends Primitive(e, guard) 

case class RestoreInputStream(e: ElementBaseMixin, guard: Boolean = true) extends Primitive(e, guard) 

//case class Value(e: SchemaComponent, guard: Boolean = true) extends Primitive(e, guard) 

case class NotStopValue(e: LocalElementBase) extends Primitive(e, e.hasStopValue) 

case class StopValue(e: LocalElementBase) extends Primitive(e, e.hasStopValue) 

case class TheDefaultValue(e: ElementBaseMixin) extends Primitive(e, e.isDefaultable) 

case class LiteralNilValue(e: ElementBaseMixin) extends Primitive(e, e.isNillable) 

case class LogicalNilValue(e: ElementBaseMixin) extends Primitive(e, e.isNillable) 

// As soon as you turn these on (by removing the false and putting the real guard), then schemas all need to have
// these properties in them, which is inconvenient until we have multi-file schema support and format references.
case class LeadingSkipRegion(e: AnnotatedMixin) extends Primitive(e, false) // e.leadingSkip > 0) 

case class AlignmentFill(e: AnnotatedMixin) extends Primitive(e, false) // e.alignment != AlignmentType.Implicit)

case class TrailingSkipRegion(e: AnnotatedMixin) extends Primitive(e, false) // e.trailingSkip > 0)

case class PrefixLength(e:ElementBaseMixin) extends Primitive(e, e.lengthKind == LengthKind.Prefixed)

case class UnicodeByteOrderMark(e: GlobalElementDecl) extends Primitive(e, false)

case class FinalUnusedRegion(e: LocalElementBase) extends Primitive(e, false)

case class InputValueCalc(e: ElementDeclBase) extends Terminal(e, false) {

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
      
      def parse(start : PState) : PState = {
    	val currentElement = start.parent
    	val result = ivcExpr.evaluate(currentElement, start.variableMap)
    	val res = result.asInstanceOf[String] // only strings for now.
    	currentElement.addContent(new org.jdom.Text(res))
    	val postState = start // inputValueCalc consumes nothing. Just creates a value.
    	postState
      }
  }
}