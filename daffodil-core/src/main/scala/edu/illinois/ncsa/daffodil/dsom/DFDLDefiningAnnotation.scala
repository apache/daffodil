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
import edu.illinois.ncsa.daffodil.xml.GlobalQName
import edu.illinois.ncsa.daffodil.xml.QName
import edu.illinois.ncsa.daffodil.xml.NamedQName

/**
 * Unlike say GlobalElementDecl, Defining annotations don't have a factory, because they
 * don't have any characteristics that depend
 * on context, i.e., that have to access the referring context to compute.
 */
abstract class DFDLDefiningAnnotation(xmlArg: Node, annotatedSCArg: AnnotatedSchemaComponent)
  extends DFDLAnnotation(xmlArg, annotatedSCArg)
  with NamedMixin { self: DFDLAnnotation =>

  // The point of this, is so we can match-case on type DFDLDefiningAnnotation
  // but then still conveniently use methods/members defined in the
  // DFDLAnnotation class
  final lazy val asAnnotation = self

  final def globalQName = namedQName.asInstanceOf[GlobalQName]

  final override lazy val namedQName: NamedQName = QName.createGlobal(name, namespace, xml.scope)
}

