package daffodil.grammar

import daffodil.dsom._
import daffodil.exceptions.Assert
import daffodil.schema.annotation.props._
import daffodil.schema.annotation.props.gen._
import daffodil.xml._
import daffodil.processors._
import java.nio.CharBuffer


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
        val priorElement = currentElement.getParent().asInstanceOf[org.jdom.Element]
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
    	Assert.invariant(currentElement.getName() != "_document_")
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

case class ConvertTextIntPrim(e : ElementBaseMixin) extends Terminal(e, true) {
   def parser : Parser = new Parser {
     def parse(start: PState) : PState = {
       val node = start.parent
       val str = node.getText()
       
      val resultState =  try {
         val i = str.toInt
       // Node remains a string because of jdom
         start
       } catch {case e:Exception => start.failed("Failed to convert to an xs:int") }
       
      resultState
     }
   }
}

abstract class Primitive(e: PropertyMixin, guard: Boolean = false) 
extends Terminal(e, guard) {
    override def toString = "Prim[" + name + "]"
    def parser: Parser = DummyParser(e)
  }

case class ZonedTextIntPrim(e : ElementBaseMixin) extends Primitive(e, false)
case class RegularBinaryIntPrim(e : ElementBaseMixin) extends Primitive(e, false)
case class PackedIntPrim(e : ElementBaseMixin) extends Primitive(e, false)
case class BCDIntPrim(e : ElementBaseMixin) extends Primitive(e, false)

class StaticDelimiter(delim: String, e: AnnotatedMixin, guard: Boolean = true) extends Terminal(e, guard) {
  def parser: Parser = new Parser {

    // TODO: Fix Cheezy matcher. Doesn't implement ignore case. Doesn't fail at first character that doesn't match. It grabs
    // the whole length (if it can), and then compares.
    // Also handles only one delimiter string. They can actually be whitespace-separated lists of alternative
    // delimiters
    // 
    Assert.notYetImplemented(e.ignoreCase == YesNo.Yes)

    Assert.invariant(delim != "") // shouldn't be here at all in this case.
    override def toString = "'" + delim + "'"
    val decoder = e.knownEncodingDecoder
    val cbuf = CharBuffer.allocate(delim.length) // TODO: Performance: get a char buffer from a pool. 

    def parse(start: PState): PState = {
      System.err.println("Parsing delimiter at bit position: " + start.bitPos)
      val in = start.inStream
      //
      // Lots of things could go wrong in here. We might be looking at garbage, so decoding will get errors, etc.
      // Those should all count as "did not find the delimiter"
      //
      // No matter what goes wrong, we're counting on an orderly return here.
      //
      val endBitPos = in.fillCharBuffer(cbuf, start.bitPos, decoder)
      val result = 
        if (endBitPos == -1) "" // causes failure down below this
        else cbuf.toString
        		 
      if (result == delim) { // TODO: use string.compare and string.compareIgnoreCase so we can implement ignoreCase property.
        System.err.println("Found " + delim)
        System.err.println("Ended at bit position " + endBitPos)
        val endCharPos = start.charPos + result.length
        val postState = start.withPos(endBitPos, endCharPos)
        postState
      } else {
        val postState = start.withPos(start.bitPos, start.charPos, new Failure("Delimiter not found"))
        postState
      }
    }
  }
}

class DynamicDelimiter(delimExpr : CompiledExpression, e: AnnotatedMixin, guard: Boolean = true) extends Primitive(e, guard)

case class StaticInitiator(e : InitiatedTerminatedMixin) extends StaticDelimiter(e.initiatorExpr.constant.asInstanceOf[String], e)
case class StaticTerminator(e : InitiatedTerminatedMixin) extends StaticDelimiter(e.terminatorExpr.constant.asInstanceOf[String], e)
case class DynamicInitiator(e : InitiatedTerminatedMixin) extends DynamicDelimiter(e.initiatorExpr, e)
case class DynamicTerminator(e : InitiatedTerminatedMixin) extends DynamicDelimiter(e.terminatorExpr, e)

//case class StaticSeparator(e : Sequence) extends StaticDelimiter(e.separatorExpr.constant.asInstanceOf[String], e)
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
     
      override def toString = "GroupPosGreaterThan"

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
            val res = (local == "string" && ns == XMLUtil.XSD_NAMESPACE) 
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