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

/**
 * Base class for any DFDL annotation
 *
 * Note about SchemaComponent as a base class:
 * Many things are now derived from SchemaComponent that were not before.
 * Just turns out that there is a lot of desirable code sharing between
 * things that aren't strictly-speaking SchemaComponents and things that
 * previously were not. Accomplishing that sharing with mixins and
 * self-typing etc. was just too troublesome. So now many things
 * are schema components. E.g., all annotation objects, the Include
 * and Import objects which represent those statements in a schema,
 * the proxy DFDLSchemaFile object, etc.
 *
 * This change lets us share more easily, also hoist a base
 * SchemaComponentBase over into daffodil-lib, which lets some of this
 * shared code, specifcally stuff about errors and diagnostics,
 * migrate over to daffodil-lib as well where it can be tied a
 * bit more tightly to the OOLAG library there.
 */
abstract class DFDLAnnotation(xmlArg: Node, annotatedSCArg: AnnotatedSchemaComponent)
  extends SchemaComponent(xmlArg, annotatedSCArg) {

  final override val context: AnnotatedSchemaComponent = annotatedSCArg

  // delegate to the annotated component.
  override def findPropertyOption(pname: String): PropertyLookupResult = {
    val res = annotatedSC.findPropertyOption(pname)
    res
  }

  final lazy val annotatedSC = annotatedSCArg

  //  override def addDiagnostic(diag: Diagnostic) = annotatedSC.addDiagnostic(diag)

  //  match {
  //      case sc : SchemaComponent => sc
  //      case _ => Assert.invariantFailed("should be a SchemaComponent")
  //    }

  override def toString = path
}
