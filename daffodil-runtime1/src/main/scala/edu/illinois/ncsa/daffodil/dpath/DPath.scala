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

package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.exceptions._
import javax.xml.xpath._
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.util.Logging
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.processors._
import scala.xml.Node
import scala.collection.JavaConversions._
import scala.collection.immutable.Queue
import edu.illinois.ncsa.daffodil.dsom._
import com.ibm.icu.util.Calendar
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.calendar.DFDLCalendar
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError

class RuntimeExpressionDPath(tt: NodeInfo.Kind, recipe: CompiledDPath,
  dpathText: String,
  ci: DPathCompileInfo,
  isEvaluatedAbove: Boolean)
  extends CompiledExpression(dpathText) {

  override def targetType = tt

  // TODO: fix this check below. There is a unierse of target types which is 
  // muuch smaller than the set of all types, so some check is useful to be sure
  // we stay within the subset of types that are actually used as target types.
  //  Assert.usage(targetType == NodeInfo.AnyType // used by debugger eval stmt
  //    || targetType == NodeInfo.NonEmptyString // string-valued properties
  //    || targetType == NodeInfo.Long // length and occurs expressions
  //    || targetType == NodeInfo.Boolean // assert/discriminator test expressions
  //    || targetType.isInstanceOf[NodeInfo.AnyAtomic.Kind] // inputValueCalc, outputValueCalc, 
  //    // setVariable, defineVariable (default expressions), newVariableInstance
  //    , "not an accepted targetType")

  override lazy val prettyExpr = dpathText

  def isConstant = false
  def isKnownNonEmpty = true // expressions are not allowed to return empty string
  def constant: Any = Assert.usageError("Boolean isConstant is false. Cannot request a constant value.")

  private def doPE(e: Exception, state: ParseOrUnparseState) = {
    val msg = "Expression evaluation failed due to: %s.".format(DiagnosticUtils.getSomeMessage(e).get)
    state match {
      case pstate: PState => {
        val pe = new ParseError(One(ci.schemaFileLocation), One(pstate), msg)
        pstate.setFailed(pe)
        (null, null)
      }
      case ustate: UState => {
        UnparseError(One(ci.schemaFileLocation), One(ustate), msg)
      }
    }
  }

  private def doSDE(e: Exception, state: ParseOrUnparseState) = {
    val pe = new RuntimeSchemaDefinitionError(ci.schemaFileLocation, state, "Expression evaluation failed due to: %s.", DiagnosticUtils.getSomeMessage(e).get)
    state match {
      case pstate: PState => pstate.setFailed(pe)
      case _ => //ok
    }
    (null, null)
  }

  private def evaluateForParserOrUnparser(pstate: ParseOrUnparseState): Any = {
    val (value, newVMap) =
      try {
        recipe.runExpression(pstate)

        val dstate = pstate.dstate
        val newVMap = dstate.vmap
        val pair = {
          dstate.currentNode match {
            case null => {
              // there is no element. Can happen if one evaluates say, 5 + 6 in the debugger
              (dstate.currentValue, newVMap)
            }
            case n: DIElement if n.isNilled => (n, newVMap)
            case c: DIComplex => {
              Assert.invariant(!targetType.isInstanceOf[NodeInfo.AnyAtomic.Kind])
              (c, newVMap)
            }
            case s: DISimple => (s.dataValue, newVMap)
            case _ => Assert.invariantFailed("must be an element, simple or complex.")
          }
        }
        pair
      } catch {
        //
        // Here we catch exceptions that indicate something went wrong with the
        // expression, but by that we mean legal evaluation of a compiled expression
        // produced an error such as divide by zero or string having wrong format for 
        // conversion to another type. I.e., things te DFDL schema author could get
        // wrong that some data inputs would exacerbate.
        //
        // This should not catch things that indicate a Daffodil code problem e.g.,
        // class cast exceptions. 
        //
        // Of course some things that are daffodil code bugs can hide - if for example
        // there is an arithmetic error in daffodil code, this catch can't distinguish 
        // that error (which should be an abort, from an arithmetic exception 
        // due to an expression dividing by zero say. 
        case e: InfosetNoSuchChildElementException => doSDE(e, pstate)
        case e: InfosetArrayIndexOutOfBoundsException => doSDE(e, pstate)
        case e: IllegalArgumentException => doPE(e, pstate)
        case e: IllegalStateException => doPE(e, pstate)
        case e: NumberFormatException => doPE(e, pstate)
        case e: ArithmeticException => doPE(e, pstate)
      }
    (value, newVMap) match {
      case (null, null) => {
        Assert.invariant(pstate.status != Success)
        return (null, null)
      }
      case _ => {
        pstate.variableMap = newVMap
        // then fall through
      }
    }
    val value1 =
      if (!value.isInstanceOf[DIElement]) {
        targetType match {
          case NodeInfo.AnyType => value // ok
          case NodeInfo.Long => {
            value match {
              case bi: BigInt => bi.toLong
              case bd: BigDecimal => bd.toLong
              case d: Double => d.toLong
              case i: Int => i.toLong
              case l: Long => l
              case _ => Assert.invariantFailed("wasn't a number. Was %s.".format(Misc.getNameFromClass(value)))
            }
          }
          case NodeInfo.NonEmptyString => {
            Assert.invariant(value.isInstanceOf[String])
            ci.schemaDefinitionUnless(value.asInstanceOf[String].length > 0,
              "Non-empty string required.")
            value
          }
          case NodeInfo.DateTime | NodeInfo.Date | NodeInfo.Time => {
            Assert.invariant(value.isInstanceOf[DFDLCalendar])
            value
          }
          case _: NodeInfo.String.Kind => {
            Assert.invariant(value.isInstanceOf[String])
            value
          }
          case NodeInfo.Boolean => {
            Assert.invariant(value.isInstanceOf[Boolean])
            value
          }
          case NodeInfo.HexBinary => {
            Assert.invariant(value.isInstanceOf[Array[Byte]])
            value
          }
          case _ => // TODO: add more checks. E.g., that proper type matching occurred for all the number 
            // and date types as well.
            value
        }
      } else {
        value
      }
    value1
  }

  /**
   * Evaluate for unparser. The value is returned. The ustate is modified
   * to contain any updated variable map.
   */
  final def evaluate(ustate: UState): Any = {
    evaluateForParserOrUnparser(ustate)
  }

  final def evaluate(pstate: PState): (Any, VariableMap) = {
    val v = evaluateForParserOrUnparser(pstate)
    val vmap = pstate.variableMap
    (v, vmap)
  }

  //  final def evaluate(pstate: PState): (Any, VariableMap) = {
  //    val (value, vmap) =
  //      try {
  //        recipe.runExpression(pstate)
  //
  //        val dstate = pstate.dstate
  //        val vmap = dstate.vmap
  //        val value = {
  //          dstate.currentNode match {
  //            case null => {
  //              // there is no element. Can happen if one evaluates say, 5 + 6 in the debugger
  //              dstate.currentValue
  //            }
  //            case n: DIElement if n.isNilled => n
  //            case c: DIComplex => {
  //              Assert.invariant(!targetType.isInstanceOf[NodeInfo.AnyAtomic.Kind])
  //              c
  //            }
  //            case s: DISimple => s.dataValue
  //            case _ => Assert.invariantFailed("must be an element, simple or complex.")
  //          }
  //        }
  //        (value, vmap)
  //      } catch {
  //        //
  //        // Here we catch exceptions that indicate something went wrong with the
  //        // expression, but by that we mean legal evaluation of a compiled expression
  //        // produced an error such as divide by zero or string having wrong format for 
  //        // conversion to another type. I.e., things te DFDL schema author could get
  //        // wrong that some data inputs would exacerbate.
  //        //
  //        // This should not catch things that indicate a Daffodil code problem e.g.,
  //        // class cast exceptions. 
  //        //
  //        // Of course some things that are daffodil code bugs can hide - if for example
  //        // there is an arithmetic error in daffodil code, this catch can't distinguish 
  //        // that error (which should be an abort, from an arithmetic exception 
  //        // due to an expression dividing by zero say. 
  //        case e: InfosetNoSuchChildElementException => doSDE(e, pstate)
  //        case e: InfosetArrayIndexOutOfBoundsException => doSDE(e, pstate)
  //        case e: IllegalArgumentException => doPE(e, pstate)
  //        case e: IllegalStateException => doPE(e, pstate)
  //        case e: NumberFormatException => doPE(e, pstate)
  //        case e: ArithmeticException => doPE(e, pstate)
  //      }
  //    value match {
  //      case null => {
  //        Assert.invariant(pstate.status != Success)
  //        return (null, null)
  //      }
  //      case _ => // fall through
  //    }
  //    val value1 =
  //      if (!value.isInstanceOf[DIElement]) {
  //        targetType match {
  //          case NodeInfo.AnyType => value // ok
  //          case NodeInfo.Long => {
  //            value match {
  //              case bi: BigInt => bi.toLong
  //              case bd: BigDecimal => bd.toLong
  //              case d: Double => d.toLong
  //              case i: Int => i.toLong
  //              case l: Long => l
  //              case _ => Assert.invariantFailed("wasn't a number. Was %s.".format(Misc.getNameFromClass(value)))
  //            }
  //          }
  //          case NodeInfo.NonEmptyString => {
  //            Assert.invariant(value.isInstanceOf[String])
  //            ci.schemaDefinitionUnless(value.asInstanceOf[String].length > 0,
  //              "Non-empty string required.")
  //            value
  //          }
  //          case NodeInfo.DateTime | NodeInfo.Date | NodeInfo.Time => {
  //            Assert.invariant(value.isInstanceOf[DFDLCalendar])
  //            value
  //          }
  //          case _: NodeInfo.String.Kind => {
  //            Assert.invariant(value.isInstanceOf[String])
  //            value
  //          }
  //          case NodeInfo.Boolean => {
  //            Assert.invariant(value.isInstanceOf[Boolean])
  //            value
  //          }
  //          case NodeInfo.HexBinary => {
  //            Assert.invariant(value.isInstanceOf[Array[Byte]])
  //            value
  //          }
  //          case _ => // TODO: add more checks. E.g., that proper type matching occurred for all the number 
  //            // and date types as well.
  //            value
  //        }
  //      } else {
  //        value
  //      }
  //    (value1, vmap)
  //  }

}
