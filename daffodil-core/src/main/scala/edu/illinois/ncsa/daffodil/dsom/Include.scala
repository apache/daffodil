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

package edu.illinois.ncsa.daffodil.dsom

import scala.xml.Node

import edu.illinois.ncsa.daffodil.util._
import IIUtils._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG

/**
 * enclosingGoalNS is None if this include
 * is being included (by one include hop, or several) into a schema having
 * 'no namespace'
 *
 * enclosingGoalNS is Some(str) if this include
 * is being included (by one include hop, or several) into a schema having
 * a targetNamespace.
 */
final class Include(xml: Node, xsd: XMLSchemaDocument, seenArg: IIMap)
  extends IIBase(xml, xsd, seenArg) {

  protected final def mapPair = LV('mapPair) {
    // for an include, the targetNamespace of the schema document that contained us is right.
    val mp = (targetNamespace, resolvedLocation)
    mp
  }.value

  private lazy val slText = schemaLocationProperty.get // include always has a schemaLocation property

  lazy val resolvedNamespaceURI = None // include doesn't have a namespace.

  // include always has a schemaLocation
  lazy val resolvedLocation = LV('resolvedLocation) {
    resolvedSchemaLocation match {
      case Some(rsl) => {
        val ns = OOLAG.keepGoing(
          schemaDefinitionError("Unable to determine target namespace.")) {
            xsd.targetNamespace
          }
        log(LogLevel.Debug, "Included schema from %s into namespace %s.", rsl, ns)
        rsl
      }
      case None => schemaDefinitionError("Included schema not found at location %s. %s", slText, whereSearched)
    }
  }.value

}
