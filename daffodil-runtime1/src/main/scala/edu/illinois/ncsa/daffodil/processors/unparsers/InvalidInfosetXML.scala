/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.util.Maybe._
import javax.xml.stream.events.XMLEvent
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils
import javax.xml.stream.XMLStreamException

/**
 * various diagnostic situations associated with the incoming XML for creating and
 * infoset.
 *
 * Note: You cannot save the xmlEvent in a data structure, it is part of an accessor/cursor.
 */
object InvalidInfosetXML {

  private def err(info: ElementRuntimeData, str: String, args: Any*) = {
    UnparseError(One(info.schemaFileLocation), Nope, str.format(args: _*))
  }

  def errorWhenTagExpected(info: ElementRuntimeData, xse: XMLStreamException, xev: XMLEvent): Nothing =
    err(info, "An error occurred: %s.\n Last event occured on line %s", DiagnosticUtils.getSomeMessage(xse).get, xev.getLocation.getLineNumber)

  def missingInfosetContent(info: ElementRuntimeData): Nothing =
    err(info, "No infoset XML found. Expected %s.", info.prettyName)

  def illegalContentWhereTagExpected(info: ElementRuntimeData, xev: XMLEvent): Nothing =
    err(info, "Illegal content where element tag expected on line %s", xev.getLocation.getLineNumber)

  def nonTextFoundInSimpleContent(parent: ElementRuntimeData, event: XMLEvent): Nothing = {
    err(parent, "Illegal content for simple element %s on line %s.", parent.namedQName.toPrettyString, event.getLocation().getLineNumber())
  }
}
