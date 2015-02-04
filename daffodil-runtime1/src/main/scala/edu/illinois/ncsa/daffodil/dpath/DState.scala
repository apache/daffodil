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

import edu.illinois.ncsa.daffodil.processors._
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.xml.RefQName
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.OnStack
import edu.illinois.ncsa.daffodil.util.PreSerialization
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar
import scala.math.BigDecimal.RoundingMode
import edu.illinois.ncsa.daffodil.util.Bits
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import java.text.ParsePosition
import com.ibm.icu.util.SimpleTimeZone
import com.ibm.icu.util.TimeZone
import java.nio.ByteBuffer
import edu.illinois.ncsa.daffodil.calendar.DFDLCalendar

/**
 * expression evaluation side-effects this state block.
 */
case class DState() {
  import AsIntConverters._
  /**
   * The currentValue is used when we have a value that is not
   * associated with an element of simple type. E.g., If I have
   * the expression 5 + $x, then none of those literals, nor their
   * nor variable value, nor their sum, has an element associated with it.
   */
  private var _currentValue: Any = null

  def resetValue() {
    _currentValue = null
  }

  def currentValue: Any = {
    if (_currentValue == null) currentSimple.dataValue
    else _currentValue
  }

  def setCurrentValue(v: Any) {
    _currentValue = v
    _currentNode = null
  }

  def booleanValue: Boolean = currentValue.asInstanceOf[Boolean]

  def longValue: Long = asLong(currentValue)
  def intValue: Int = longValue.toInt
  def doubleValue: Double = asDouble(currentValue)

  def integerValue: BigInt = asBigInt(currentValue)
  def decimalValue: BigDecimal = asBigDecimal(currentValue)
  def stringValue: String = currentValue.asInstanceOf[String]

  def isNilled: Boolean = currentElement.isNilled

  def arrayLength: Long =
    if (currentNode.isInstanceOf[DIArray]) currentArray.length
    else {
      if (currentNode.isInstanceOf[DIElement]) {
    	  this.pstate.SDW("The specified path to element %s is not to an array. Suggest using fn:exists instead.", currentElement.name)        
      }
      else this.pstate.SDW("The specified path is not to an array. Suggest using fn:exists instead.")
      1
    }

  def exists: Boolean = true // we're at a node, so it must exist.

  def dateValue: DFDLCalendar = currentValue.asInstanceOf[DFDLCalendar]
  def timeValue: DFDLCalendar = currentValue.asInstanceOf[DFDLCalendar]
  def dateTimeValue: DFDLCalendar = currentValue.asInstanceOf[DFDLCalendar]

  /**
   * Array index calculations (that is [expr], what XPath
   * calls 'predicate')
   */
  def index: Int = longValue.toInt

  private var _currentNode: DINode = null

  def currentNode = _currentNode
  def setCurrentNode(n: DINode) {
    _currentNode = n
    _currentValue = null
  }

  def currentSimple = currentNode.asInstanceOf[DISimple]
  def currentElement = currentNode.asInstanceOf[DIElement]
  def currentArray = currentNode.asInstanceOf[DIArray]
  def currentComplex = currentNode.asInstanceOf[DIComplex]

  private var _vmap: VariableMap = null

  def vmap = _vmap
  def setVMap(m: VariableMap) {
    _vmap = m
  }

  private var _pstate: PState = null // for issuing processing errors

  def pstate = _pstate
  def setPState(ps: PState) {
    _pstate = ps
  }

  // These exists so we can override it in our fake DState we use when
  // checking expressions to see if they are constants. SelfMove 
  // for real is a no-op, but when we're evaluating an expression to see if 
  // it is a constant, the expression "." aka self, isn't constant. 
  // Similarly, if you call fn:exists(....) and the contents are not gong to 
  // exist at constnat fold time. But we don't want fn:exists to say the result
  // is always a constant (false) because at constant folding time, hey, nothing
  // exists... this hook lets us change behavior for constant folding to throw.
  def selfMove(): Unit = {}
  def fnExists(): Unit = {}
}

class DStateForConstantFolding extends DState {
  private def die = throw new java.lang.IllegalStateException("No infoset at compile time.")

  override def currentSimple = currentNode.asInstanceOf[DISimple]
  override def currentElement = die
  override def currentArray = die
  override def currentComplex = die
  override def pstate = die
  override def currentNode = new FakeDINode
  override def vmap = die
  override def selfMove() = die
  override def fnExists() = die
}