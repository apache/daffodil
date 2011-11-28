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

import scala.collection.mutable.LinkedList

import org.jdom.Parent

import annotation.enumerations.Fixed
import annotation.enumerations.OrderedSequence
import annotation.enumerations.Parsed
import annotation.enumerations.UnorderedSequence
import annotation.Annotation
import annotation.AnnotationDefaults
import daffodil.exceptions.ElementNotFoundException
import daffodil.exceptions.ElementProcessingException
import daffodil.exceptions.UnimplementedException
import daffodil.parser.regex.Regex
import daffodil.parser.RollbackStream
import daffodil.processors.input.BasicProcessor
import daffodil.processors.VariableMap
//import daffodil.schema.BasicNode
//import daffodil.schema.ChildResult
//import daffodil.schema.ComplexType
import daffodil.xml.Namespaces
import daffodil.xml.XMLUtil

/**
 * A BasicNode representing the xsd:sequence component.
 *
 * This node parses a sequence of children BasicNode and succeeds if all children are parsed correctly. All
 * the children will be siblings and are returned by the findChildren method.
 *
 * @param ann annotation associated with this node
 * @param target the target namespace
 * @param namespaces the namespaces in scope
 * @param c children nodes contained by this sequence
 *
 * @version 1
 * @author Alejandro Rodriguez
 *
 */
@SerialVersionUID(1)
class Sequence(ann:Annotation,target:String,namespaces:Namespaces,c:List[BasicNode])
        extends BasicNodeImpl(target,namespaces,ann) with ComplexType {

  private val ordered : Boolean = (ann.format.sequenceKind match {
    case Some(x) => x
    case None => AnnotationDefaults.defaultSequenceKind
  }) match {
    case OrderedSequence => true
    case UnorderedSequence => false
  }

  
  private var originalChildrenMin:Array[Int] = _
  private var originalChildrenMax:Array[Int] = _
  private var originalFixedOccurs:Boolean = _

  addAll(c)

  if (!ordered){
    setChildrenOccurrences
  }

  override def getName(parent:Parent) :String = 
    parent match{
      case e:org.jdom.Element => e.getName
      case _ => throw new IllegalStateException("Parent of sequence is not an element")
    }

  override protected def findChildren(input:RollbackStream,variables:VariableMap,
                             parent:Parent,maxLength:Int,terminators:List[Regex],
                             processor:BasicProcessor) : ChildResult = {
    if (ordered)
      findChildrenOrdered(input,variables,parent,maxLength,terminators,processor)
    else
      findChildrenUnordered(input,variables,parent,maxLength,terminators,processor)
  }

  private def findChildrenOrdered(input:RollbackStream,variables:VariableMap,
                             parent:Parent,maxLength:Int,terminators:List[Regex],
                             processor:BasicProcessor) : ChildResult = {

    var results:LinkedList[org.jdom.Element] = null

    for(child <- children)
      if (results==null)
        results =  child(input,annotation,variables,parent,maxLength,terminators)
      else {
        val childElt = child(input,annotation,variables,parent,maxLength,terminators)
        val res = results append childElt
        res
      }
    new ChildSuccess(results)
  }

  private def findChildrenUnordered(input:RollbackStream,variables:VariableMap,
                             parent:Parent,maxLength:Int,terminators:List[Regex],
                             processor:BasicProcessor) : ChildResult = {
    var results:Array[LinkedList[org.jdom.Element]] = new Array(children size)


    //find a child in any order
    def findAnyChild:Boolean = {
      for(i <- 0 until children.size){
        try{
          val r : LinkedList[org.jdom.Element] = children(i)(input,annotation,variables,parent,maxLength,terminators)
          if(r.size>0){
            if (results(i)==null)
              results(i) = r
            else
              results(i) append r

            return true
          }
        }catch {
          case e:ElementProcessingException => //ignore
        }
      }

      return false
    }

    var notDone = true

    var maxCount : Int = if (originalChildrenMax exists { _ < 0 }) -1 else originalChildrenMax.reduceLeft { _+_ }

    while(notDone && maxCount != 0){
      notDone = findAnyChild
      maxCount -= 1;
    }


    def sizeList(l:LinkedList[_]) : Int =
      if (l==null) 0 else l.size


    //remove all the elements we just added from parent
    for(l <- results)
        XMLUtil removeChildren (parent,l)

    //check mins and maxs
    if((0 until children.size).forall { i:Int => sizeList(results(i)) >= originalChildrenMin(i) &&
            (originalChildrenMax(i) == -1 || sizeList(results(i)) <= originalChildrenMax(i))}){
      //re-add them in proper order
      var orderedResults:LinkedList[org.jdom.Element] = null

      for(l <- results)
        if (orderedResults!=null)
          orderedResults append (l)
        else
          orderedResults = l

      XMLUtil addChildren(parent,orderedResults)

      new ChildSuccess(orderedResults)
    }else{      
      throw new ElementNotFoundException("Occurrences of inner elements do not match",
        schemaContext = annotation element,documentContext = parent,position = Some(input getPosition))
    }
  }

  /**
   * overrides the min and max occurrences in children nodes
   * remembers the values in originalChildrenMin/Max
   */
  private def setChildrenOccurrences {
    var allParsed = true
    var allFixed = true
    for(child <- children){
      child.annotation.format.occursCountKind.getOrElse(AnnotationDefaults defaultOccursCountKind) match {
        case Parsed => allFixed = false
        case Fixed => allParsed = false
        case _ => allParsed = false; allFixed = false
      }
    }

    // TODO other occurs count are allowed?
    if (!allParsed && !allFixed)
      throw new UnimplementedException("For unordered sequences all contained elements should " +
              "be occursCountKind={fixed | parsed }",schemaContext = annotation element)

    originalFixedOccurs = allFixed

    originalChildrenMin = new Array(children size)
    originalChildrenMax = new Array(children size)

    for(i <- 0 until children.size){
      originalChildrenMin(i) = children(i).annotation.format.minOccurs.getOrElse(1)
      originalChildrenMax(i) = children(i).annotation.format.maxOccurs.getOrElse(1)

      children(i).annotation.format.setMinOccurs(1)
      children(i).annotation.format.setMaxOccurs(1)

//      if (originalFixedOccurs){
//        children(i).annotation.format.setMaxOccurs(1)
//      }else
//        children(i).annotation.format.setMaxOccurs("unbounded")
    }



  }

  override def canEqual(o:Any):Boolean = o.isInstanceOf[Sequence]

  override def equals(o:Any) : Boolean = o match {
    case that:Sequence => {
      that.canEqual(this) && super.equals(that)
    }
    case	 _ => false
  }
  
  override def diff(o:Any) : Similarity = o match {
    case that:Sequence => {
      val superDiff = super.diff(that) 
      return superDiff
    }
    case	 _ => DifferentType
  }

}
