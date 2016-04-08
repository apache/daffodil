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

package edu.illinois.ncsa.daffodil.processors

import scala.Array.canBuildFrom
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.processors.dfa.CreateFieldDFA
import edu.illinois.ncsa.daffodil.util.Maybe.One

/**
 * What's going on here is that we have three factory classes:
 *
 * 1. EscapeScheme
 * 2. Delimiters
 * 3. Field
 *
 * Field depends on Delimiters and EscapeScheme. As such it needs to handle
 * dynamic/static cases. Delimiters can be static or dynamic and so need to
 * be generated prior to Field. EscapeScheme can also be static or
 * dynamic and so needs to also be generated prior to Field.
 *
 * We use information regarding whether or not we have Static vs. Dynamic
 * Delimiters/EscapeScheme to determine if we can statically generate the
 * Field. So there is some cascading going on here as a result of these
 * dependencies.
 */
sealed abstract class FieldFactoryBase(esev: Option[EscapeSchemeParseEv],
  context: ThrowsSDE) extends Serializable {

  def getFieldDFA(state: PState)
}

case class FieldFactoryStatic(esev: Option[EscapeSchemeParseEv], context: ThrowsSDE)
  extends FieldFactoryBase(esev, context) {

  lazy val fieldDFA = {
    val res = if (esev.isDefined) {
      val theScheme = esev.get.optConstant.get
      theScheme match {
        case s: EscapeSchemeBlockParserHelper => CreateFieldDFA()
        case s: EscapeSchemeCharParserHelper => CreateFieldDFA(s.ec, s.eec)
      }

    } else {
      CreateFieldDFA()
    }
    res
  }

  def getFieldDFA(start: PState) = {

    val delimDFAs = start.mpstate.getAllTerminatingMarkup
    val delimsCooked = delimDFAs.map(d => d.lookingFor).toList

    start.mpstate.currentFieldDFA = One(fieldDFA)
    start.mpstate.currentDelimsCooked = One(delimsCooked)
  }
}

case class FieldFactoryDynamic(esev: Option[EscapeSchemeParseEv],
  context: ThrowsSDE)
  extends FieldFactoryBase(esev, context) {

  def getFieldDFA(start: PState) = {

    val scheme = start.mpstate.currentEscapeScheme
    val delimDFAs = start.mpstate.getAllTerminatingMarkup
    val delimsCooked = delimDFAs.map(d => d.lookingFor).toList

    val fieldDFA =
      if (scheme.isDefined) {
        val theScheme = scheme.get
        val res = theScheme match {
          case s: EscapeSchemeBlockParserHelper => CreateFieldDFA()
          case s: EscapeSchemeCharParserHelper => CreateFieldDFA(s.ec, s.eec)
        }
        res
      } else {
        CreateFieldDFA()
      }

    start.mpstate.currentFieldDFA = One(fieldDFA)
    start.mpstate.currentDelimsCooked = One(delimsCooked)
  }
}
