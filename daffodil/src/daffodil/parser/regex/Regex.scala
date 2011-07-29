package daffodil.parser.regex

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


import java.util.regex.Pattern
import java.io.Serializable

/**
 * A regular expression with the property that it accepts A iff any prefix of A is accepted by some prefix
 * of the regular expression.
 *
 * These expressions are especially useful for DFDL strings (a regular string with DFDL class characters).
 *
 * @version 1
 * @author Alejandro Rodriguez
 */
@SerialVersionUID(1)
class Regex extends Serializable{


  //TODO reimplement whole class using scala.util.matching utilities for building custom regular expression matchers



  private var charMap:Map[Char,Int] = Map()

  private var start = new Node(0)
  private var lastNode:Set[Node] = Set(start)
  private var minLength = 0
  private var regexComiled:RegexCompiled = _
  private var lastIgnoreCase = false


  //debugging
  private val sb = new StringBuilder

  /**
   * Appends a character group to the expression.
   *
   * E.G. to build /ab[cd]*(e|fg)+ use:
   *
   * addAlternatives(List("a"),false,false)
   * addAlternatives(List("b"),false,false)
   * addAlternatives(List("c","d"),true,true)
   * addAlternatives(List("e","fg"),false,true)
   *
   */
  def addAlternatives(chars:List[String],optional:Boolean,repeating:Boolean) = {

    sb append("(" + chars.reduceLeft { _ + "|" + _ } + ")")
    if (optional && repeating)
      sb.append("*")
    else if (repeating)
      sb append("+")
    else if (optional)
      sb append("?")
    
    var newLastNode:Set[Node] = Set()
    var middleLayer:Set[Node] = Set()

    for(s <- chars){
      var node = new Node(s(0))

     middleLayer += node

      if (!charMap.contains(s(0)))
        charMap += ((s(0),charMap.size+1))

      for(n <- lastNode)
        n addChild(node)

      for(i <- 1 until s.length){
        if (!charMap.contains(s(i)))
          charMap += ((s(i),charMap.size+1))

        val tNode = new Node(s(i))
        node addChild(tNode)
        node = tNode
      }

      newLastNode += node
    }

    if (repeating)
      for(n1 <- newLastNode;
          n2 <- middleLayer)
        n1 addChild(n2)

    if (optional)
      newLastNode ++= lastNode

    lastNode = newLastNode

    minLength += chars.foldLeft(0) { (x:Int,y:String) => if (x<y.length) x else y.length }
  }

  /**
   * Compiles the regular expression and provides a matcher in the form of a RegexCompiled object
   */
  def compile(ignoreCase:Boolean):RegexCompiled = {
    if (regexComiled != null && ignoreCase == lastIgnoreCase)
      regexComiled
    else{    
      start.number = 0

      val N = setStateNumbers(Set(start),Set(),1)
      val M = charMap.size + 1

      val transitionMap:Array[Array[Int]] = Array.ofDim(N,M)
      var finalStates:Set[Int] = Set()

      for(n <- lastNode)
        finalStates += n.number

      for(i <- 0 until N;
          j <- 0 until M)
        transitionMap(i)(j) = -1

      transitionMap(0)(0) = 0;

      val compiledCharMap =
        if (ignoreCase)
          charMap map { x:(Char,Int) => (x._1.toLower,x._2)}
        else
          charMap

      setTransitions(transitionMap,compiledCharMap,Set(start),Set(),ignoreCase)

      regexComiled = new InnerRegexCompiled(minLength,compiledCharMap,transitionMap,finalStates,ignoreCase)
      lastIgnoreCase = ignoreCase
      regexComiled
    }
  }


  private class Node(val char:Char) {
    private var children:List[Node] = Nil
    var number:Int = -1
    def addChild(node:Node) = children = node :: children
    def getChildren = children
    override def toString = "Node["+char+","+number+"]"
  }

  private def setStateNumbers(visiting:Set[Node],visited:Set[Node],nextNumber:Int):Int = {
    if (visiting.size>0){
      var toCount:Set[Node] = Set()

      for(n <- visiting)
        if (toCount.forall { _.getChildren != n.getChildren })
          toCount += n

      var newNextNumber = nextNumber

      for(n <- toCount if n.number == -1){
        n.number = newNextNumber
        newNextNumber += 1
      }

      for(n <- visiting if n.number == -1)
        toCount.find { _.getChildren == n.getChildren } match {
          case Some(n2) => n.number = n2.number
          case None => throw new IllegalStateException("Could not assing number to state")
        }

      var toVisit:Set[Node] = Set()

      for(n <- toCount)
        toVisit ++= n.getChildren
      
      toCount.size + setStateNumbers((toVisit--visited)--visiting,visited++visiting,newNextNumber)
    } else
      0
  }

  private def setTransitions(transitionMap:Array[Array[Int]],charMap:Map[Char,Int],
                             visiting:Set[Node],visited:Set[Node],ignoreCase:Boolean):Unit =
    if (visiting.size>0){
      var toVisit:Set[Node] = Set()

      for(source <- visiting;
          destination <- source.getChildren){
        if (ignoreCase)
          transitionMap(source.number)(charMap(destination.char.toLower)) = destination.number
        else
          transitionMap(source.number)(charMap(destination.char)) = destination.number
        if (!visited.contains(destination))
          toVisit += destination
      }

      setTransitions(transitionMap,charMap,(toVisit--visited)--visiting,visited++visiting,ignoreCase)
    }

  private class InnerRegexCompiled(length:Int,charMap:Map[Char,Int],transitionMap:Array[Array[Int]],
                                   finalStates:Set[Int],ignoreCase:Boolean) extends RegexCompiled {

    override def minLength:Int = length

    override def startsWith(string:String):Boolean = {
      var currentState = 0

      val toMatch =
        if (ignoreCase)
          string toLowerCase
        else
          string

      for(c <- toMatch){
        if (!charMap.contains(c))
          return false
        currentState = transitionMap(currentState)(charMap(c))
        if (currentState == -1)
          return false
      }

      true
    }

    override def matches(string:String):Boolean = {
      var currentState = 0

      val toMatch =
        if (ignoreCase)
          string toLowerCase
        else
          string

      for(c <- toMatch){
        if (!charMap.contains(c))
          return false
        currentState = transitionMap(currentState)(charMap(c))
        if (currentState == -1)
          return false
      }

      finalStates.contains(currentState)
    }
  }

  override def toString = sb toString

}