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

package org.apache.daffodil.dsom

import scala.xml.Node

/**
 * Factory to create an instance of a global element declaration
 * either to be the root of the data, or when referenced from an
 * element reference, in which case a backpointer from the global element decl
 * instance will point back to the element reference.
 *
 * This backpointer is needed in order to determine some attributes that refer
 * outward to what something is contained within. E.g., nearestEnclosingSequence
 * is an attribute that might be the sequence containing the element reference
 * that is referencing this global element declaration.
 */
class GlobalElementDeclFactory(xmlArg: Node, schemaDocumentArg: SchemaDocument)
  extends SchemaComponentFactory(xmlArg, schemaDocumentArg)
  with GlobalElementComponentMixin {

  def forRoot() = asRoot // cache. Not a new one every time.

  private lazy val asRoot = {
    lazy val ged = new GlobalElementDecl(xml, parent, root)
    lazy val root: Root = new Root(xml, schemaDocument, namedQName, ged)
    root
  }

  def forElementRef(eRef: AbstractElementRef) = {
    new GlobalElementDecl(xml, schemaDocument, eRef)
  }

}
