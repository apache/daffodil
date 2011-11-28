package daffodil.schema

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
import annotation._
import annotation.enumerations._
import daffodil.processors.xpath.NodeResult
import daffodil.processors.xpath.StringResult
import daffodil.exceptions._
import daffodil.processors.xpath.XPathUtil
import org.jdom.Parent
import daffodil.processors._
import daffodil.xml.{XMLUtil, Namespaces}
import input._
import daffodil.parser.RollbackStream
import daffodil.parser.regex.{RegexCompiled, Regex}
import scala.collection.mutable.LinkedList

abstract class ChildResult

case class ChildSuccess(l:LinkedList[org.jdom.Element]) extends ChildResult
case class ChildLast(l:LinkedList[org.jdom.Element]) extends ChildResult

abstract class BasicNodeImpl(target:String,namespaces:Namespaces,ann:Annotation)
        extends BasicNode(target,namespaces,ann){

  protected var children:List[BasicNode] = Nil

  private var min = 1
  private var max = 1

  private var separatorPrefix:Boolean = false
  private var separatorInfix:Boolean = false
  private var separatorPostfix:Boolean = false


  private var stopValue:List[RegexCompiled] = null
  
  def setMinOccurs(m:Int) = min = m

  def setMaxOccurs(m:Int) = max = m

  def getMinOccurs = min

  def getMaxOccurs = max

  def addChild(child:BasicNode) =
    children = children ::: List(child)


  def addAll(child:List[BasicNode]) =
    children = children ::: child

  def getChildren = children

  /**
   * Returns the name of the DOM element to be returned by this node 
   */
  protected def getName(parent:Parent):String

  protected def setCardinality(element:org.jdom.Element,annotation:Annotation,
                               variables:VariableMap,namespaces:Namespaces) = {

    annotation.format.minOccurs match {
      case Some(i) => setMinOccurs(i)
      case None => 1
    }

    annotation.format.maxOccurs match {
      case Some(i) => setMaxOccurs(i)
      case None => 1
    }

    val ignoreCase = annotation.format.ignoreCase match {
      case Some(b) => b
      case None => AnnotationDefaults defaultIgnoreCase
    }

    annotation.format.occursCountKind match {
      case Some(Expression) => setCardinalityByExpression(element,annotation,variables,namespaces)
      case Some(Fixed) => if (getMinOccurs != getMaxOccurs)
        throw new DFDLSchemaDefinitionException("minOccurs!=maxOccurs and occursKind=fixed",
          schemaContext = annotation element,documentContext = element)
      case Some(Parsed) => setMaxOccurs(-1)
      case Some(StopValue) => annotation.format.stopValue match {
        case x:ExpressionValue => setMaxOccurs(-1)
          stopValue = XMLUtil.getListFromExpression(x,variables,element,namespaces).map { _ compile(ignoreCase) }
        case ListLiteralValue(x) => setMaxOccurs(-1); stopValue = x.map { _ compile(ignoreCase) }
        case EmptyValue =>  throw new DFDLSchemaDefinitionException("occursCountKind=stopValue but no stopValue defined",
          schemaContext = annotation element,documentContext = element)
      }
      case None =>
    }
  }

  private def getStopValueByExpression(x:String,element:org.jdom.Element,annotation:Annotation,
                                         variables:VariableMap,namespaces:Namespaces) =
    XPathUtil evalExpression(XPathUtil getExpression(x),variables,element,namespaces) match {
      case StringResult(s) => s
      case NodeResult(n) =>
        if (n!=null)
          n.getText
        else
          throw new ElementProcessingException("Empty result from expression '"+XPathUtil.getExpression(x)+"'",
            schemaContext = annotation.element,documentContext = element)
      }

  private def setCardinalityByExpression(element:org.jdom.Element,annotation:Annotation,
                                         variables:VariableMap,namespaces:Namespaces):Unit = {
    val expression = annotation.format.occursCount match {
      case Some(s) => s
      case None => throw new DFDLSchemaDefinitionException("Missing occursCount and occursCountKind='expression'",
        schemaContext = annotation element,documentContext = element)
    }
    if (!XPathUtil.isExpression(expression))
      throw new ExpressionDefinitionException("not an expression "+expression,
        schemaContext = annotation element,documentContext = element)

    val value = XPathUtil evalExpression(XPathUtil getExpression(expression),variables,element,namespaces) match {
      case StringResult(s) => s
      case NodeResult(n) =>
        if (n!=null)
          n.getText
        else
          throw new ElementProcessingException("Empty result from expression '"+XPathUtil.getExpression(expression)+"'",
            schemaContext = annotation.element,documentContext = element)
      }
    try{
      setMinOccurs(value.toInt)
      setMaxOccurs(value.toInt)
    }catch{
      case e:NumberFormatException =>
        throw new ElementProcessingException("Expression did not return a number '"+value+"'",
          schemaContext = annotation.element,documentContext = element)
    } 
  }

  override def apply(input:RollbackStream,parentAnnotations:Annotation,
                     variables:VariableMap,parent:Parent,
                     maxLength:Int,terminators:List[Regex]):LinkedList[org.jdom.Element] = {

    val name = getName(parent)

    var thisVariables = variables
    input checkpoint;
    thisVariables save

    //TODO FIXME DONT CREATE ELEMENT HERE IF IT IS SEQUENCE OR CHOICE, USE PARENT
    val thisTemporalElement = XMLUtil addNewChild(parent,name,target,namespaces)

    val processor = ProcessorFactory getInputProcessor(annotation)

    try{
      for (beforeProcessor <- ProcessorFactory getBeforeProcessor(annotation))
        thisVariables = beforeProcessor(thisTemporalElement,thisVariables,target,namespaces)

      setCardinality(thisTemporalElement,annotation,thisVariables,namespaces)

      processor init(input,thisTemporalElement,thisVariables,namespaces)
    }catch{
      case e:ElementProcessingException => {
        thisVariables restore;
        input rollback;
        throw e }
    }finally {
      XMLUtil removeChild(parent,thisTemporalElement)
    }

    separatorPrefix = false
    separatorInfix = false
    separatorPostfix = false
    if (annotation.format.separator != EmptyValue)
      annotation.format.separatorPosition match {
        case Some(Prefix) => separatorPrefix = true
        case Some(Infix) => separatorInfix = true
        case Some(Postfix) => separatorPostfix = true
        case None =>  separatorInfix = true // FIXME: should error. Properties don't have defaults in DFDL.
      }


    val terminatorsToChild:List[Regex] = annotation.format.terminator match {
      case EmptyValue => terminators
      case ListLiteralValue(l) =>  l ::: terminators
      case e:ExpressionValue =>
          XMLUtil.getListFromExpression(e,thisVariables,parent,namespaces) ::: terminators
    }

    var results:LinkedList[org.jdom.Element] = emptyLinkedList

    try{
      thisVariables save;
      results = applyHelper(input,thisVariables,max,maxLength,parent,terminatorsToChild,processor)

      val sz = results.size
      if (sz < min) {
        throw new ElementNotFoundException("Occurrences less than xs:minOccurs",
          schemaContext = annotation element,documentContext = parent,position = Some(input getPosition))
      }

      processor terminate(input,results.last,thisVariables,namespaces,terminatorsToChild)

      //TODO FIXME after processors should be executed right after its child element, in applyHelper
      for(afterProcessor <- ProcessorFactory getAfterProcessor(annotation))
        for(child <- results)
          thisVariables = afterProcessor(child,thisVariables,target,namespaces)

      if (ProcessorFactory isHidden(annotation))
        for(e <- results)
          variables hideElement(e)

      input uncheck;
      results

    }catch{
      case e:ElementNotFoundException => input rollback; if (getMinOccurs ==0) emptyLinkedList else throw e
      case e:ElementProcessingException => {
        XMLUtil removeChildren(parent,results);
        thisVariables restore;
        input rollback;
        if (e.position == None)
          e.position = Some(input getPosition)
        if (e.documentContext == null)
          e.documentContext = parent
        if (e.schemaContext == null)
          e.schemaContext = annotation.element
        throw e }
    }
  }

  val emptyLinkedList = new LinkedList[org.jdom.Element]
  /**
   * Return the list of elements produced by this BasicNode from the current point in the input stream.
   * This would all the elements in the array if the element occurs multiple times
   */
  private def applyHelper(input: RollbackStream,
    variables: VariableMap,
    occurs: Int,
    maxLength: Int,
    parent: Parent,
    terminators: List[Regex],
    processor: BasicProcessor): LinkedList[org.jdom.Element] = {
    if (occurs == 0)
      emptyLinkedList
    else {
      val (newOccurs, results) = nextToken(input, variables, parent, maxLength, terminators, occurs, processor)
      // assert(newOccurs >= 0)
      // TODO set maxLength to correct value
      results match {
        case ChildLast(l) => l
        case ChildSuccess(l) =>
          try {
            if (stopValue == null || !stopValue.exists { _ matches (l.last.getText) }) {
              val tail = applyHelper(input, variables, newOccurs, maxLength, parent, terminators, processor)
              l append tail
            }
            l
          } catch {
            // speculation failed. We might have found something, but something required after it was not found
            // so we end up back here due to the throw.
            case e: ElementNotFoundException => {
               System.err.println("Backtrack! " + e.getMessage())
               input rollback;
               emptyLinkedList
            }
          }
      }
    }
  }

  /**
   * Reads the separator (if needed, prefix, infix or postfix) and the next element in the array of DOM elements
   * produced by this BasicNode. Delegates to findChildren
   */
  private def nextToken(input:RollbackStream,variables:VariableMap,parent:Parent,
          maxLength:Int,terminators:List[Regex],occurs:Int,processor:BasicProcessor):(Int,ChildResult) = {

      input checkpoint;
      try {
        var newOccurs = occurs - 1
        if (separatorPrefix)
        processor.findPrefixSeparator(input,parent,variables,namespaces,terminators) match {
          case _:TerminatorFound | _:EndOfStreamFound | NothingFound =>
            throw new ElementNotFoundException("Could not find prefix separator",
              schemaContext = annotation element,documentContext = parent,position = Some(input getPosition))
          case _:SeparatorFound => //OK
        }

        val results = findChildren(input,variables,parent,maxLength,terminators,processor)

      //TODO FIXME parent is wrong (if this is ComplexElement or SimpleElement), should be a temporal element or results.last
      if (separatorInfix && newOccurs!=0)
        processor.findPostfixSeparator(input,parent,variables,namespaces,terminators) match {
          case _:TerminatorFound | _:EndOfStreamFound | NothingFound => newOccurs = 0//that was the last one
          case _:SeparatorFound => //OK
        }
      else if (separatorPostfix) {
        val fps = processor.findPostfixSeparator(input,parent,variables,namespaces,terminators)
        fps match {
          case _:SeparatorFound => //OK
          case _:TerminatorFound | _:EndOfStreamFound | NothingFound => {
            newOccurs = 0
            if (annotation.format.separatorPolicy == Some(Required)){
               removeChild(results,parent)
               throw new ElementNotFoundException("Could not find postfix separator",
                 schemaContext = annotation element,documentContext = parent,position = Some(input getPosition))
            }
          }
          }
      }
        input uncheck;
        (newOccurs,results)
      }catch{
        case e:ElementProcessingException => input rollback; throw e
      }
  }

  private def removeChild(child:ChildResult,parent:Parent) =
    child match {
      case ChildSuccess(l) => XMLUtil.removeChildren(parent,l)
      case ChildLast(l) => XMLUtil.removeChildren(parent,l)
      case _ =>
    }

  /**
   * Reads the next element (single) of the array of elements produced by this BasicNode
   */
  protected def findChildren(input:RollbackStream,variables:VariableMap,
                             parent:Parent,maxLength:Int,terminators:List[Regex],
                             processor:BasicProcessor) : ChildResult

 
  override def diff(o:Any) : Similarity = o match {
    case that:BasicNodeImpl => {
      val superDiff = super.diff(that) 
      if (superDiff != Same) return superDiff
      val childDiff = Diffable.diffLists(this.children, that.children)
      if (childDiff != Same) return childDiff
      Same
    }
    case	 _ => DifferentType
  }
  
}
