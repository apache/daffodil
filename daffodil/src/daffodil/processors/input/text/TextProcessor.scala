package daffodil.processors.input.text

/**
 * Copyright (c) 2010 NCSA.  All rights reserved.
 * Developed by: NCSA Cyberenvironments and Technologies
 *               University of Illinois at Urbana-Champaign
 *               http://cet.ncsa.uiuc.edu/
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal with the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the names of NCSA, University of Illinois, nor the names of its
 *     contributors may be used to endorse or promote products derived from this
 *     Software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * WITH THE SOFTWARE.
 *
 */

/*
 * Created By: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu >
 * Date: 2010
 */
import java.nio.charset.Charset

import org.jdom.Parent
import org.jdom.Element

import daffodil.processors.xpath.XPathUtil
import daffodil.processors.xpath.StringResult
import daffodil.processors.xpath.NodeResult
import daffodil.schema.annotation.enumerations.LeftJustification
import daffodil.schema.annotation.enumerations.CenterJustification
import daffodil.schema.annotation.enumerations.RightJustification
import daffodil.schema.annotation.enumerations.TextStringJustification
import daffodil.schema.annotation.enumerations.SeparatorPosition
import daffodil.schema.annotation.enumerations.SeparatorPolicy
import daffodil.schema.annotation.enumerations.Infix
import daffodil.schema.annotation.enumerations.Prefix
import daffodil.schema.annotation.enumerations.Postfix
import daffodil.schema.annotation.enumerations.LengthKind
import daffodil.schema.annotation.enumerations.EndOfParent
import daffodil.schema.annotation.enumerations.Explicit
import daffodil.schema.annotation.enumerations.Implicit
import daffodil.schema.annotation.enumerations.Pattern
import daffodil.schema.annotation.enumerations.Prefixed
import daffodil.schema.annotation.enumerations.Delimited
import daffodil.schema.annotation.enumerations.EscapeKind
import daffodil.schema.annotation.enumerations.EscapeCharacter
import daffodil.schema.annotation.enumerations.EscapeBlock
import daffodil.xml.Namespaces
import daffodil.xml.XMLUtil.addNewChild
import daffodil.xml.XMLUtil.removeChild
import daffodil.xml.XMLUtil.XSD_NAMESPACE
import daffodil.xml.XMLUtil.getListFromExpression
import daffodil.exceptions._
import daffodil.parser.{RollbackStream, VariableEncodingStreamReader}
import daffodil.processors._
import input._
import daffodil.parser.regex.{RegexCompiled, Regex}
import daffodil.schema.annotation._

/**
 * The basic scanner for text input. This processor scans for tokens, initiators, separators and terminators on text.
 *
 * @version 1
 * @author Alejandro Rodriguez
 */
class TextProcessor(val charset:Charset,val acceptEOF:Boolean)
        extends BasicProcessor {

  private var lengthKind:LengthKind= _
  private var length:String = _
  private var lengthPattern:java.util.regex.Pattern = _

  private var separator:AttributeValue = EmptyValue
  private var separatorPolicy:SeparatorPolicy = _
  private var separatorPosition:SeparatorPosition = _

  private var initiator:AttributeValue = EmptyValue
  private var terminator:AttributeValue = EmptyValue

  protected var ignoreCase:Boolean = AnnotationDefaults defaultIgnoreCase

  private var justification:TextStringJustification = _
  private var padCharacter:Regex = _

  private var documentFinalTerminatorCanBeMissing:Boolean = _

  private var escapeKind:EscapeKind = _

  private var escapeCharacter:String = _

  private var escapeEscapeCharacter:String = _

  private var escapeBlockStart:String = _
  private var escapeBlockEnd:String = _

  protected var typeName:String = _

  def setLengthKind(lengthKind:LengthKind) = this.lengthKind = lengthKind
  def setLength(length:String) = this.length = length
  def setLengthPattern(pattern:String) = {
    if (ignoreCase)
      this.lengthPattern = java.util.regex.Pattern compile (pattern , java.util.regex.Pattern.CASE_INSENSITIVE)
    else
      this.lengthPattern = java.util.regex.Pattern compile (pattern)
  }


  def setSeparator(separator:AttributeValue) = this.separator = separator
  def setSeparatorPolicy(separatorPolicy:SeparatorPolicy) = this.separatorPolicy = separatorPolicy
  def setSeparatorPosition(separatorPosition:SeparatorPosition) = this.separatorPosition = separatorPosition

  def setInitiator(initiator:AttributeValue) = this.initiator = initiator
  def setTerminator(terminator:AttributeValue) = this.terminator = terminator

  def setIgnoreCase(ignoreCase:Boolean) = {
    this.ignoreCase = ignoreCase
    if (lengthPattern != null)
      setLengthPattern(lengthPattern.pattern)
  }

  def setTextStringJustification(justification:TextStringJustification) = this.justification = justification
  def setPadCharacter(padCharacter:Regex) = this.padCharacter = padCharacter

  def setFinalTerminatorCanBeMissing(b:Boolean) = this.documentFinalTerminatorCanBeMissing = b

  def getEscapeKind = escapeKind
  def setEscapeKind(escapeKind:EscapeKind) = this.escapeKind = escapeKind
  def setEscapeCharacter(escapeChar:String) = this.escapeCharacter = escapeChar
  def setEscapeEscapeCharacter(escapeEscapeChar:String) = this.escapeEscapeCharacter = escapeEscapeChar
  def setEscapeBlockStart(escapeBlock:String) = this.escapeBlockStart = escapeBlock
  def setEscapeBlockEnd(escapeBlock:String) = this.escapeBlockEnd = escapeBlock

  def setTypeName(typeName:String) = this.typeName = typeName

  override def apply(input:RollbackStream,element:Element,variables:VariableMap,
                     namespaces:Namespaces,terminators:List[Regex]):ProcessorResult = {
    val result = lengthKind match {
      case Prefixed | Implicit =>
        throw new DFDLSchemaDefinitionException("Prefixed or Implicit lengths not allowed in text representation",
          null,null,element,Some(input getPosition))
      case Delimited | EndOfParent => getVariableLength(input,element,variables,namespaces,terminators)
      case Explicit => getFixedLength(input,element,variables,namespaces)
      case Pattern => getPatterned(input,element,namespaces)
    }
    //TODO FIXME EndOfParent could also mean fixed length if parent is patterned or explicit


//    //TODO only other alternative is an extended basic type, find the base type and use it
//    if (typeName!=null){
//      result match {
//        case Success | Last => setType(typeName.substring(XSD_NAMESPACE.length),element,namespaces)
//        case _ =>
//      }
//    }

    result
  }

  private def getVariableLength(input:RollbackStream,element:Element,
                                variables:VariableMap,namespaces:Namespaces,
                                parentTerminators:List[Regex]):ProcessorResult = {
    val reader = new VariableEncodingStreamReader(input,charset)
   // val stringBuilder = new StringBuilder()

    val separators = separator match {
      case EmptyValue => List()
      case ListLiteralValue(l) => l
      case e:ExpressionValue => getListFromExpression(e,variables,element,namespaces)
    }
    val terminators = terminator match {
      case EmptyValue => List()
      case ListLiteralValue(l) => l
      case e:ExpressionValue => getListFromExpression(e,variables,element,namespaces)
    }

    input checkpoint()

    try {
      val processorResult = separatorPosition match {
        case Prefix => 
          getPrefixSeparated(input,reader,separators,terminators, parentTerminators,element,variables,namespaces)
        case Infix => 
          getInfixSeparated(input,reader,separators,terminators, parentTerminators,element,variables,namespaces)
        case Postfix => 
          getPostfixSeparated(input,reader,separators,terminators, parentTerminators,element,variables,namespaces)
      }

      input.uncheck;
      processorResult
    }catch{
      case e:ElementNotFoundException => input.rollback; throw e;
    }
  }

  private def getInfixSeparated(
    input: RollbackStream,
    reader: VariableEncodingStreamReader,
    separators: List[Regex],
    terminators: List[Regex],
    parentTerminators: List[Regex],
    element: Element,
    variables: VariableMap,
    namespaces: Namespaces): ProcessorResult = {

    val foundSep = findPostfixSeparator(input, reader, element, variables, namespaces, separators, terminators, parentTerminators)
    foundSep match {
      case SeparatorFound(token, separator) => { element setText (token); Success }
      case TerminatorFound(token, terminator) => { element setText (token); Last } // FIXME: being treated as an array terminator, not an element of the array terminator
      //TODO should include parentTerminators in this condition?
      case EndOfStreamFound(token) =>
        if (documentFinalTerminatorCanBeMissing || (terminators.size + parentTerminators.size == 0))
          if (token.length > 0) {
            element setText (token); Last
          } else
            LastEmpty
        else
          throw new ElementNotFoundException("End of stream reached looking for separator/terminator",
            documentContext = element, position = Some(input getPosition))
      case NothingFound =>
        throw new ElementNotFoundException("End of stream reached looking for separator/terminator",
          documentContext = element, position = Some(input getPosition))
    }
  }

  private def getPrefixSeparated(input:RollbackStream,reader:VariableEncodingStreamReader,
                                 separators:List[Regex],terminators:List[Regex],
                                 parentTerminators:List[Regex],element:Element,variables:VariableMap,
                                 namespaces:Namespaces):ProcessorResult = {

    val fps = findPrefixSeparator(input,reader,element,variables,namespaces,separators,terminators,parentTerminators) 
    fps match {
      case SeparatorFound(_,separator) =>
      case TerminatorFound(_,terminator) =>
        throw new ElementNotFoundException("Found terminator when looking for prefix separator",
          documentContext = element,position = Some(input getPosition))
      case EndOfStreamFound(_) =>
        throw new ElementNotFoundException("End of stream reached when looking for prefix separator",
          documentContext = element,position = Some(input getPosition))
      case NothingFound =>
        throw new ElementNotFoundException("Nothing found when looking for prefix separator",
          documentContext = element,position = Some(input getPosition))
    }

    findPostfixSeparator(input,reader,element,variables,namespaces,separators,terminators,parentTerminators) match {
      case SeparatorFound(token,separator) => { input.pushBack(reader size(separator)); element setText(token); Success }
      case TerminatorFound(token,terminator) => { element setText(token); Last }
      case EndOfStreamFound(token) => { element setText(token); Last }
    }
  }

  private def getPostfixSeparated(input:RollbackStream,reader:VariableEncodingStreamReader,
                                  separators:List[Regex],terminators:List[Regex],
                                  parentTerminators:List[Regex],element:Element,variables:VariableMap,
                                  namespaces:Namespaces):ProcessorResult = {

    val pfs = findPostfixSeparator(input,reader,element,variables,namespaces,separators,terminators,parentTerminators) 
    pfs match {
      case SeparatorFound(token,separator) => { 
        element.setText(token)
        Success 
      }
      case TerminatorFound(token,terminator) => { 
        element.setText(token)
        Last 
      }
      case EndOfStreamFound(token) =>  { 
        element.setText(token)
        Last
      }
      case NothingFound =>
        throw new ElementNotFoundException("End of stream reached when looking for postfix separator",
          documentContext = element,position = Some(input getPosition))
    }
  }


  override def findPrefixSeparator(input:RollbackStream,parent:Parent,variables:VariableMap,
                                   namespaces:Namespaces,parentTerminators:List[Regex]):ScanResult = {
    val terminators = terminator match {
      case EmptyValue => List()
      case ListLiteralValue(l) => l
      case e:ExpressionValue => getListFromExpression(e,variables,parent,namespaces)
    }
    val separators = separator match {
      case EmptyValue => List()
      case ListLiteralValue(l) => l
      case e:ExpressionValue => getListFromExpression(e,variables,parent,namespaces)
    }
    val reader = new VariableEncodingStreamReader(input,charset)

    findSeparatorOnly(input,reader,parent,variables,namespaces,separators,terminators,parentTerminators)
  }

  override def findPostfixSeparator(input:RollbackStream,parent:Parent,variables:VariableMap,
                                    namespaces:Namespaces,parentTerminators:List[Regex] = Nil):ScanResult = {

    val terminators = terminator match {
      case EmptyValue => List()
      case ListLiteralValue(l) => l
      case e:ExpressionValue => getListFromExpression(e,variables,parent,namespaces)
    }
    val separators = separator match {
      case EmptyValue => List()
      case ListLiteralValue(l) => l
      case e:ExpressionValue => getListFromExpression(e,variables,parent,namespaces)
    }
    val reader = new VariableEncodingStreamReader(input,charset)

    findSeparatorOnly(input,reader,parent,variables,namespaces,separators,terminators,parentTerminators)
  }

  /**
   * Scans for the next longest separator or terminator. If a separator is found it will be consumed.
   * If a terminator if found is pushed back to the stream
   *
   * The separator/terminator does not need to start at the current input position
   *
   */
  private def findSeparatorOnly(input:RollbackStream,reader:VariableEncodingStreamReader,
                                element:Parent,variables:VariableMap,namespaces:Namespaces,
                                separators:List[Regex],
                                terminators:List[Regex],parentTerminators:List[Regex]):ScanResult = {

    val evaluatedEscapeChar:Char = getEscapeChar(element,variables,namespaces)
    val evaluatedEscapeEscapeChar:Char = getEscapeEscapeChar(element,variables,namespaces)
    val evaluatedEscapeStart:String = getEscapeStart(element,variables,namespaces)
    val evaluatedEscapeEnd:String = getEscapeEnd(element,variables,namespaces)

    val compiledSeparators = separators map { _ compile (ignoreCase) }
    val compiledTerminators = terminators map { _ compile (ignoreCase) }
    val compiledParentTerminators = if (endOfParent) parentTerminators map { _ compile (ignoreCase) } else List()

    val charScanner = nextChar(input,reader,evaluatedEscapeChar,evaluatedEscapeEscapeChar,
        evaluatedEscapeStart,evaluatedEscapeEnd,Integer.MAX_VALUE) _

    input checkpoint

    val result = readDelimiter(input,charScanner,List(compiledSeparators,compiledTerminators,compiledParentTerminators),
      Set(0),true)

    result._1 match {
      case SeparatorFound(x,y) =>
        input uncheck;
        if (result._2 == 0)
          result._1
        else
          TerminatorFound(x,y)
      case EndOfStreamFound(_) =>
        input uncheck;
        result._1
      case NothingFound =>
        input rollback;
        result._1
    }
  }

  /**
   * Consumes a separator. Assumes the separator starts at the current position of input.
   *
   * If a terminator is found instead, it's pushed back to the stream
   *
   * @returns A ScanResult containing: the separator/terminator if found,
   *          EndOfStreamFound if EOF before scanning a whole separator
   *          NothingFound otherwise
   */
  private def findPrefixSeparator(input:RollbackStream,reader:VariableEncodingStreamReader,
                                  element:Parent,variables:VariableMap,namespaces:Namespaces,
                                  separators:List[Regex],
                                  terminators:List[Regex],
                                  parentTerminators:List[Regex]):ScanResult = {

    val evaluatedEscapeChar:Char = getEscapeChar(element,variables,namespaces)
    val evaluatedEscapeEscapeChar:Char = getEscapeEscapeChar(element,variables,namespaces)
    val evaluatedEscapeStart:String = getEscapeStart(element,variables,namespaces)
    val evaluatedEscapeEnd:String = getEscapeEnd(element,variables,namespaces)

    val compiledSeparators = separators map { _ compile(ignoreCase) }
    val compiledTerminators = terminators map { _ compile(ignoreCase) }
    val compiledParentTerminators = if (endOfParent) parentTerminators map { _ compile(ignoreCase) } else List()

    val charScanner = nextChar(input,reader,evaluatedEscapeChar,evaluatedEscapeEscapeChar,
      evaluatedEscapeStart,evaluatedEscapeEnd,Integer.MAX_VALUE) _

    input checkpoint;

    val result = readDelimiter(input,charScanner,List(compiledSeparators,compiledTerminators,compiledParentTerminators),
      Set(0),false)

    result._1 match {
      case SeparatorFound(x,y) =>
        input uncheck;
        if (result._2 == 0)
          result._1
        else
          TerminatorFound(x,y)
      case EndOfStreamFound(_) =>
        input uncheck;
        result._1
      case NothingFound =>
        input rollback;
        result._1
    }
  }

  private def maxS(s:Seq[String]) =
    s.foldLeft(0) { (x:Int,y:String)=> if (y.length>x) y.length else x}

  private def maxI(s:Seq[Int]) =
    s.foldLeft(0) { (x:Int,y:Int)=> if (y>x) y else x}

  private def first(s:Seq[(Int,String)]) =
    s.foldLeft((Integer.MAX_VALUE,"")) { (x:(Int,String),y:(Int,String))=> if (y._1<x._1 && y._1>=0) y else x}


  /**
   * Scans for the next separator/terminator and the token prefixing it.
   *
   * It will consume the prefix, but the separator/terminator are pushed back to the stream.
   *
   * @returns a ScanResult containing TerminatorFound or SeparatorFound with the token if found
   *           EndOfStreamFound with the token if EOF reached before separator/terminator found but some prefix was read
   *           NothingFound otherwise
   */
  private def findPostfixSeparator(input:RollbackStream,reader:VariableEncodingStreamReader,
                                   element:Element,variables:VariableMap,namespaces:Namespaces,separators:List[Regex],
                                   terminators:List[Regex],parentTerminators:List[Regex]):ScanResult = {

    val evaluatedEscapeChar:Char = getEscapeChar(element,variables,namespaces)
    val evaluatedEscapeEscapeChar:Char = getEscapeEscapeChar(element,variables,namespaces)
    val evaluatedEscapeStart:String = getEscapeStart(element,variables,namespaces)
    val evaluatedEscapeEnd:String = getEscapeEnd(element,variables,namespaces)

    input checkpoint

    val charScanner = nextChar(input,reader,evaluatedEscapeChar,evaluatedEscapeEscapeChar,
      evaluatedEscapeStart,evaluatedEscapeEnd,Integer.MAX_VALUE) _

    val compiledTerminators = terminators map { _ compile(ignoreCase) }
    val compiledSeparators = separators map { _ compile(ignoreCase) }
    val compiledParentTerminators = if (endOfParent) parentTerminators map { _ compile(ignoreCase) } else List()


    val result = readDelimiter(input,charScanner,List(compiledSeparators,compiledTerminators,
      compiledParentTerminators),Set(),true)

    result._1 match {
      case SeparatorFound(x,y) =>
        input uncheck;
        if (result._2 == 0)
          result._1
        else
          new TerminatorFound(x,y)
      case EndOfStreamFound(_) => input uncheck; result._1
      case NothingFound => input rollback; result._1
    } 
  }


  /**
   * Consumes the left padding and initiator
   */
  override def init(input:RollbackStream,element:Element,variables:VariableMap,
                    namespaces:Namespaces):Unit = {
    val reader = new VariableEncodingStreamReader(input,charset)
    
    input checkpoint //checkpoint before padding and initiator

    consumeLeftPadding(input,reader)

    val initiators = initiator match {
      case EmptyValue => List()
      case ListLiteralValue(l) => l
      case e:ExpressionValue => getListFromExpression(e,variables,element,namespaces)
    }

    if (initiators.length > 0){
      val compiledInitiators = initiators map { _ compile (ignoreCase) }

      val evaluatedEscapeChar:Char = getEscapeChar(element,variables,namespaces)
      val evaluatedEscapeEscapeChar:Char = getEscapeEscapeChar(element,variables,namespaces)
      val evaluatedEscapeStart:String = getEscapeStart(element,variables,namespaces)
      val evaluatedEscapeEnd:String = getEscapeEnd(element,variables,namespaces)

      val charScanner = nextChar(input,reader,evaluatedEscapeChar,evaluatedEscapeEscapeChar,
                       evaluatedEscapeStart,evaluatedEscapeEnd,Integer.MAX_VALUE) _

      if (readDelimiter(input,charScanner,List(compiledInitiators),Set(0),false)._1 !=NothingFound)
        input uncheck //deletes checkpoint before padding
      else{
        input rollback; //backtracks padding
        throw new InitiatorMissingException("Initiator not found",
          documentContext = element,position = Some(input getPosition))
      }
    }
  }

  /**
   * Consumes the terminator and right padding
   */
  override def terminate(input:RollbackStream,element:org.jdom.Element,variables:VariableMap,
                         namespaces:Namespaces,parentTerminators:List[Regex]) = {
    val reader = new VariableEncodingStreamReader(input,charset)
    val stringBuilder = new StringBuilder()

    input checkpoint //checkpoint before terminator and padding

    val terminators = terminator match {
      case EmptyValue => List()
      case ListLiteralValue(l) => l
      case e:ExpressionValue => getListFromExpression(e,variables,element,namespaces)
    }

    if (terminators.length > 0){
      val compiledTerminators = terminators map { _ compile(ignoreCase) }
      val compiledParentTerminators = if (endOfParent) parentTerminators map { _ compile(ignoreCase) } else List()
      val evaluatedEscapeChar:Char = getEscapeChar(element,variables,namespaces)
      val evaluatedEscapeEscapeChar:Char = getEscapeEscapeChar(element,variables,namespaces)
      val evaluatedEscapeStart:String = getEscapeStart(element,variables,namespaces)
      val evaluatedEscapeEnd:String = getEscapeEnd(element,variables,namespaces)

      val charScanner = nextChar(input,reader,evaluatedEscapeChar,evaluatedEscapeEscapeChar,
                       evaluatedEscapeStart,evaluatedEscapeEnd,Integer.MAX_VALUE) _

      val scanResult = readDelimiter(input,charScanner,List(compiledTerminators,compiledParentTerminators),Set(0),false)

      if (scanResult._1 == NothingFound){
        if (!documentFinalTerminatorCanBeMissing){
          val currentPos = input.getPosition
          input rollback()
          throw new TerminatorMissingException("Terminator not found",
            documentContext = element,
            position = Some(currentPos))
        }
      }
      //TODO FIXME Can't have right padding after parent terminator
    }
    consumeRightPadding(input,reader)
    input uncheck
  }

  private def consumeLeftPadding(input:RollbackStream,
                                 reader:VariableEncodingStreamReader) = {
    if (justification == RightJustification || justification == CenterJustification){
      var char = reader read()
      val regexCompiled = padCharacter compile(ignoreCase)
      while(regexCompiled matches (char.toChar.toString))
        char = reader read()
      if (char != -1)
        input pushBack(reader getBytesRead)
    }
  }

  private def consumeRightPadding(input:RollbackStream,reader:VariableEncodingStreamReader) = {
    if (justification == LeftJustification || justification == CenterJustification){
      var char = reader read()
      val regexCompiled = padCharacter compile(ignoreCase)
      while(regexCompiled matches (char.toChar.toString))
        char = reader read()
      if (char != -1)
        input pushBack(reader getBytesRead)
    }
  }

  /**
   * Scans a token with fixed length
   *
   * @param input the input stream to scan from
   * @param element the XML context element
   * @param variables variables in scope
   * @param namespaces namespaces in scope
   *
   */
  private def getFixedLength(input:RollbackStream,element:Element,
                             variables:VariableMap,namespaces:Namespaces):ProcessorResult = {
    val reader = new VariableEncodingStreamReader(input,charset)
    val string = new StringBuilder()
    input checkpoint()

    val evaluatedLength =
      if (XPathUtil isExpression(length))
        XPathUtil.evalExpression(length,variables,element,namespaces) match {
          case StringResult(s) => s toInt
          case NodeResult(n) => n.toString.toInt
        }
      else
        length toInt

    val evaluatedEscapeChar:Char = getEscapeChar(element,variables,namespaces)
    val evaluatedEscapeEscapeChar:Char = getEscapeEscapeChar(element,variables,namespaces)
    val evaluatedEscapeStart:String = getEscapeStart(element,variables,namespaces)
    val evaluatedEscapeEnd:String = getEscapeEnd(element,variables,namespaces)


    val c= nextChar(input,reader,evaluatedEscapeChar,evaluatedEscapeEscapeChar,
                    evaluatedEscapeStart,evaluatedEscapeEnd,evaluatedLength)(0)
    var char = c char
    var readChars = c charsRead

    while(char != -1) {
      string append (char.toChar)

      val p = nextChar(input,reader,evaluatedEscapeChar,evaluatedEscapeEscapeChar,
                       evaluatedEscapeStart,evaluatedEscapeEnd,evaluatedLength)(readChars)
      readChars = p.charsRead
      char = p.char
    }

    if (string.toString.length>0){
      input uncheck()
      element setText(string toString)
      if (readChars > evaluatedLength)
        Last
      else
        Success
    }else{
      input rollback()
      throw new ElementNotFoundException("End of stream reached when scanning fixed length element",
        documentContext = element,position = Some(input getPosition))
    }
  }

  /**
   * Scans a token with patterned length
   *
   * @param input the input stream to scan
   * @param element the XML context element
   * @param namespaces the namespaces in scope
   */
  private def getPatterned(input:RollbackStream,element:Element,
                           namespaces:Namespaces):ProcessorResult = {
    val reader = new VariableEncodingStreamReader(input,charset)
    val string = new StringBuilder()

    input checkpoint()

    var char = reader read()

    while(char > -1){
      string append(char.toChar)
      val notYetMatcher = lengthPattern matcher(string toString)
      if (notYetMatcher matches){
        char = reader read()
        while(char > -1){
          string append(char toChar)
          val stillMatcher = lengthPattern matcher(string toString)
          if (! stillMatcher.matches){
            input pushBack (reader getBytesRead)
            input uncheck()
            val s = string toString;
            element setText(s substring(0,s.length-1))
            return Success
          }
          char = reader read()
        }

        if (acceptEOF){
          input uncheck()
          element setText(string toString)
          return Last
        }else{
          input rollback()
          throw new ElementNotFoundException("End of stream reached when scanning patterned element",
            documentContext = element,position = Some(input getPosition))
        }
      }
      char = reader read()
    }

    input rollback()
    throw new ElementNotFoundException("End of stream reached when scanning patterned element",
      documentContext = element,position = Some(input getPosition))
  }

  private def getEscapeChar(element:Parent,variables:VariableMap,namespaces:Namespaces):Char =
    if (escapeKind == EscapeCharacter){
      if (XPathUtil isExpression(escapeCharacter))
        XPathUtil.evalExpression(escapeCharacter,variables,element,namespaces) match {
          case StringResult(s) => if (s.size==1) s(0) else
            throw new ElementProcessingException("The expression does not return a character",
              documentContext = element)
          case NodeResult(n) => if (n.toString.size==1) n.toString.apply(0) else
            throw new ElementProcessingException("The expression does not return a character",
              documentContext = element)
        }
      else
      if (escapeCharacter.size==1) escapeCharacter(0) else
        throw new DFDLSchemaDefinitionException("The expression is not a character",null,null,element,None)
    }else
      0

  private def getEscapeEscapeChar(element:Parent,variables:VariableMap,namespaces:Namespaces):Char =
    if (escapeEscapeCharacter != null){
      if (XPathUtil isExpression(escapeEscapeCharacter))
        XPathUtil.evalExpression(escapeEscapeCharacter,variables,element,namespaces) match {
          case StringResult(s) => if (s.size==1) s(0) else
            throw new ElementProcessingException("The expression does not return a character",
              documentContext = element)
          case NodeResult(n) => if (n.toString.size==1) n.toString.apply(0) else
            throw new ElementProcessingException("The expression does not return a character",
              documentContext = element)
        }
      else
        if (escapeEscapeCharacter.size==1) escapeEscapeCharacter(0) else
          throw new DFDLSchemaDefinitionException("The expression is not a character",
            documentContext = element)
    }else
      0


  private def getEscapeStart(element:Parent,variables:VariableMap,namespaces:Namespaces):String =
    if (escapeKind == EscapeBlock){
      if (XPathUtil isExpression(escapeBlockStart))
        XPathUtil.evalExpression(escapeBlockStart,variables,element,namespaces) match {
          case StringResult(s) => s
          case NodeResult(n) => n.toString
        }
      else
        escapeBlockStart
    }else
      null

  private def getEscapeEnd(element:Parent,variables:VariableMap,namespaces:Namespaces):String =
    if (escapeKind == EscapeBlock){
      if (XPathUtil isExpression(escapeBlockEnd))
        XPathUtil.evalExpression(escapeBlockEnd,variables,element,namespaces) match {
          case StringResult(s) => s
          case NodeResult(n) => n.toString
        }
      else
        escapeBlockEnd
    }else
      null


  /**
   * Reads the next no-escaped char. It might read multiple chars from the input in order to find a no-escaped char.
   *
   * The scanner will not read more than (maxLengh-charsRead) characters from the input.
   *
   * @returns the character read (-1 if EOF or nothing unescaped in the allowed number of characters),
   * and the actual number of bytes and characters (unescaped or otherwise) read from the input
   */
  private def nextChar(input:RollbackStream,reader:VariableEncodingStreamReader,evaluatedEscapeChar:Char,
                       evaluatedEscapeEscapeChar:Char,evaluatedEscapeStart:String,evaluatedEscapeEnd:String,
                      maxLength:Int)(charsRead:Int):CharScan = {
    if (charsRead>=maxLength)
      new CharScan(0,0,-1)
    else
      escapeKind match {
        case EscapeCharacter => nextCharWithEscapeChar(input,reader,evaluatedEscapeChar,evaluatedEscapeEscapeChar,
                                                       charsRead,maxLength)
        case EscapeBlock => nextCharWithEscapeBlock(input,reader,evaluatedEscapeEscapeChar,evaluatedEscapeStart,
                                                    evaluatedEscapeEnd,charsRead,maxLength)
        case _ => nextCharWithNoEscape(reader,charsRead,maxLength)
      }
  }

  private def nextCharWithEscapeChar(input:RollbackStream,reader:VariableEncodingStreamReader,
                                     evaluatedEscapeChar:Char,
                                     evaluatedEscapeEscapeChar:Char,
                                     charsRead:Int,maxLength:Int):CharScan = {
    var char = reader read
    var bytesRead = reader.getBytesRead
    var newCharsRead = charsRead
    if (char == -1)
      new CharScan(bytesRead,maxLength+1,-1)
    else{
      newCharsRead += 1
      if (char == evaluatedEscapeEscapeChar){
        input checkpoint;
        var char2 = reader read()
        bytesRead += reader.getBytesRead;
        if (char2!= -1){
          newCharsRead += 1
          if (char2 == evaluatedEscapeChar){
            input uncheck;
            return new CharScan(bytesRead,newCharsRead,char2)
          }else{
            newCharsRead -= 1
            input rollback;
          }
        }else{
          input uncheck;
           new CharScan(bytesRead,newCharsRead,evaluatedEscapeEscapeChar)
        }
      }

      if (char == evaluatedEscapeChar){
        //read and discard next char
        if (newCharsRead<maxLength){
          char = reader read()
          bytesRead += reader.getBytesRead;
          if (char!= -1){
            newCharsRead += 1
            if (newCharsRead<maxLength){
              //read and keep next char
              nextCharWithEscapeChar(input,reader,evaluatedEscapeChar,evaluatedEscapeEscapeChar,newCharsRead,maxLength)
            }else
               new CharScan(bytesRead,newCharsRead,-1)
          }else
             new CharScan(bytesRead,maxLength+1,-1)
        }else
          new CharScan(bytesRead,newCharsRead,-1)
      }else
        new CharScan(bytesRead,newCharsRead,char)
    }
  }


  private def nextCharWithEscapeBlock(input:RollbackStream,reader:VariableEncodingStreamReader,
                       evaluatedEscapeEscapeChar:Char,evaluatedEscapeStart:String,evaluatedEscapeEnd:String,
                       charsRead:Int,maxLength:Int):CharScan = {

    var char = reader read
    var bytesRead = reader getBytesRead
    var newCharsRead = charsRead

    def readUntilEnd:Unit = {
      if (newCharsRead<maxLength){
        var char = reader read;
        bytesRead += reader.getBytesRead
        val sb = new StringBuilder
        newCharsRead += 1

        while (char != -1 && newCharsRead<maxLength){
          sb.append(char.toChar)
          if (sb.toString.endsWith(escapeBlockEnd))
            return
          else{
            char = reader read;
            bytesRead += reader.getBytesRead
            newCharsRead += 1
          }
        }
      }
    }

    def readStart(i:Int):Boolean = {
      if (i==escapeBlockStart.length)
        true
      else{
        if (newCharsRead == maxLength)
          false
        else{
          val char = reader read;
          bytesRead += reader.getBytesRead;
          if (char != -1 && char.toChar == escapeBlockStart(i)){
            newCharsRead += 1
            readStart(i+1)
          }
          else
            false
        }
      }
    }


    if (char == -1)
      new CharScan(bytesRead,newCharsRead,-1)
    else{
      newCharsRead += 1

      if (char.toChar == evaluatedEscapeEscapeChar){
        input checkpoint;
        var char2 = reader read;
        bytesRead += reader.getBytesRead;
        if (char2 != -1){
          if (char2.toChar == escapeBlockStart(0)){
            input uncheck;
            return new CharScan(bytesRead,newCharsRead+1,char2)
          }else{
            input rollback
          }
        }else{
          return new CharScan(bytesRead,newCharsRead,char)
        }
      }

      if (char.toChar == escapeBlockStart(0)){
        input checkpoint

        if (readStart(1)){
          readUntilEnd

          if (newCharsRead<maxLength){
            char = reader read;
            if (char != -1){
              input uncheck;
              new CharScan(bytesRead,newCharsRead+1,char)
            }else{
              input uncheck;
              new CharScan(bytesRead,newCharsRead+1,-1)
            }
          }else{
            input uncheck;
            new CharScan(bytesRead,newCharsRead,-1)
          }
        }else{
          input rollback;
          new CharScan(bytesRead,charsRead+1,char)
        }
      }else
        new CharScan(bytesRead,newCharsRead,char)
    }
  }

  private def nextCharWithNoEscape(reader:VariableEncodingStreamReader,
                       charsRead:Int,maxLength:Int):CharScan = {
    var char = reader read
    var bytesRead = reader getBytesRead
    var newCharsRead = charsRead

    if (char == -1)
      new CharScan(bytesRead,maxLength+1,-1)
    else
      new CharScan(bytesRead,newCharsRead+1,char)
  }


  override def toString = "TextProcessor("+parametersToString+")"

  protected def parametersToString = "encoding:"+charset+ ",EOF:"+acceptEOF+
          ",length:"+length+
          ",separator:"+separator+",terminator:"+terminator+
          ",initiator:"+initiator

  def canEqual(o:Any):Boolean = o.isInstanceOf[TextProcessor]

  override def equals(o:Any) =
    o match {
      case that:TextProcessor => {
        that.canEqual(this) && this.length == that.length &&
                this.charset == that.charset && this.acceptEOF == that.acceptEOF &&
                this.initiator == that.initiator &&
                this.separator == that.separator &&
                this.terminator == that.terminator
      }
      case _ => false
    }


  /**
   * Scans a delimiter from current position in input
   *
   * @param input the input stream
   * @param charScanner function that takes how many characters have been already read in this operation and returns
   * the next char
   * @param delimitersByPrecedence the delimiters, group by precedence
   * @param consumeDelimiters if the delimiter found is of any of these precedences,
   *  the input should be positioned after the delimiter, otherwise right before it
   * @param havePrefix whether to expect a token of text before the delimiter
   *
   * @returns the token found if any in a ScanResult and the number of precedence group to which the delimiter found
   * corresponds
   */
  private def readDelimiter(input:RollbackStream,charScanner:(Int)=>CharScan,
                            delimitersByPrecedence:List[List[RegexCompiled]],
                            consumeDelimiters:Set[Int],havePrefix:Boolean):(ScanResult,Int) = {

    val allTokens = delimitersByPrecedence.flatMap { x:List[RegexCompiled] => x }
    val prefix = new StringBuilder

    //tries to read the longest matching delimiter at current position
    //does not backtrack, but on success input is at position right after
    //the matching delimiter. Result is (ScanResult, precedence of delimiter)
    def readTerminator(bytesRead:List[Int],nextChar:CharScan,delimiter:StringBuilder):Option[(ScanResult,Int)] = {
      delimiter.append(nextChar.char.toChar)
      val currentDelimiter = delimiter toString;
      if (! allTokens.exists { st:RegexCompiled => st startsWith(currentDelimiter) }){
        //current token is not prefix of any delimiter, try dropping chars from the end of token
        var l = currentDelimiter.length - 1
        while(l>0){
          //drop last character from delimiter
          input pushBack (bytesRead(currentDelimiter.length-1-l))
          for(i <- 0 until delimitersByPrecedence.length)
            if (delimitersByPrecedence(i).exists { _ matches delimiter.substring(0,l) }){
              return Some((SeparatorFound(prefix toString,delimiter.substring(0,l)),i))
          }          
          l -= 1
        }
        None
      }else{
        //current token is prefix of some delimiter, try appending more chars
        val newChar = charScanner(nextChar.charsRead)
        if (newChar.char != -1)
          readTerminator(newChar.bytesRead :: bytesRead,newChar,delimiter)
        else{
          for(i <- 0 until delimitersByPrecedence.length)
            if (delimitersByPrecedence(i).exists { _ matches delimiter.toString})
              return Some((SeparatorFound(prefix toString,delimiter.toString),i))
          None
        }
      }
    }

    def readPrefix(previousChar:CharScan):(ScanResult,Int) = {
      input checkpoint; //checkpoint before beginning of delimiter
      var c = charScanner(previousChar.charsRead)

      if (c.char != -1){
        val delimiter = readTerminator(List(c.bytesRead),c,new StringBuilder)

        delimiter match {
          case None =>
            input rollback; //backtracks last readTerminator attempt
            c = charScanner(previousChar.charsRead)
            prefix append(c.char.toChar)
            readPrefix(c)
          case Some(x) =>
            if (consumeDelimiters contains(x _2))
              input uncheck
            else
              input rollback; //go back to beginning of delimiter

            input uncheck; //this deletes the checkpoint at the beginning of the prefix
            return x
        }
      }else
        if (prefix.length > 0){
          input uncheck;//this deletes the checkpoint at the beginning of the prefix
          (EndOfStreamFound(prefix toString),0)
        }else{
          input rollback;//backtracks to before trying to read prefix
          (NothingFound,0)
        }
    }

    input checkpoint //checkpoint right before beginning of prefix or stand-alone delimiter

    if (havePrefix){
      readPrefix(new CharScan(0,0,-1))      
    }else{
      val c = charScanner(0)
      if (c.char != -1)
        readTerminator(List(c.bytesRead),c,new StringBuilder) match {
          case Some(x) =>
            if (consumeDelimiters contains(x _2))
              input uncheck; //deletes checks point at beginning of stand-alone delimiter
            else
              input rollback; //backtracks to before beginning of stand-alone delimiter
            x
          case None => input.rollback; (NothingFound,0)
        }
      else{
        input rollback; //backtracks to before beginning of stand-alone delimiter
        (NothingFound,0)
      }
    }
  }

  private class CharScan(val bytesRead:Int,val charsRead:Int,val char:Int) {
    override def toString = "CharScan(" + bytesRead + ", " + charsRead + ", '" + char + "')"
  }

}