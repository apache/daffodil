/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError
import edu.illinois.ncsa.daffodil.util.Maybe.Nope
import edu.illinois.ncsa.daffodil.util.Maybe.One
import edu.illinois.ncsa.daffodil.infoset.DISimple
import edu.illinois.ncsa.daffodil.infoset.XMLTextInfosetOutputter
import edu.illinois.ncsa.daffodil.infoset.InfosetCommon

case class DAFTrace(recipe: CompiledDPath, msg: String)
  extends RecipeOpWithSubRecipes(recipe) {

  private def asXMLString(ie: InfosetCommon) = {
    val sw = new java.io.StringWriter()
    val xml = new XMLTextInfosetOutputter(sw)
    ie.visit(xml, false)
    sw.toString()
  }

  override def run(dstate: DState): Unit = {
    recipe.run(dstate)
    val nodeString: String = dstate.currentNode match {
      case _: DISimple | null => {
        // if there is no current node (null case) then there must be a
        // current value.
        val v = dstate.currentValue
        dstate.setCurrentValue(v)
        v.toString()
      }
      case other: InfosetCommon => "\n" + asXMLString(other)
    }
    System.err.println("trace " + msg + ":" + nodeString)
  }

  // This is toXML for the case class object, not the infoset node it is
  // dealing with.
  override def toXML = toXML(recipe.toXML)

}

case object DAFError extends RecipeOp {

  override def run(dstate: DState) {
    val maybeSFL =
      if (dstate.runtimeData.isDefined) One(dstate.runtimeData.get.schemaFileLocation)
      else Nope
    dstate.mode match {
      case UnparserNonBlocking | UnparserBlocking =>
        UnparseError(maybeSFL, dstate.contextLocation, "The error function was called.")
      case _: ParserMode => {
        val fe = new FNErrorFunctionException(maybeSFL, dstate.contextLocation, "The error function was called.")
        throw fe
      }
    }
  }
}
