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
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.api.WithDiagnostics
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.LV
import scala.util.matching.Regex
import edu.illinois.ncsa.daffodil.dsom.Facet._
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.grammar.GlobalElementDeclGrammarMixin

/**
 * A global element decl uses LocalElementBase because it behaves like a local
 * element when you consider that except for the root case, it has to be combined
 * with an ElementRef that references it. The ElementRef can carry the things
 * like min/maxOccurs and such that aren't allowed on a GlobalElementDecl. The
 * combination of an ElementRef plus its GlobalElementDecl behaves like a LocalElementDecl.
 */
class GlobalElementDecl(xmlArg: Node, schemaDocumentArg: SchemaDocument, val elementRef: Option[ElementRef])
  extends LocalElementBase(xmlArg, schemaDocumentArg, 0)
  with GlobalComponentMixin
  with ElementDeclMixin
  with GlobalElementDeclGrammarMixin {

  requiredEvaluations(document)

  override lazy val maxOccurs = elementRef match {
    case Some(er) => er.maxOccurs
    case None => 1
  }

  override lazy val minOccurs = elementRef match {
    case Some(er) => er.minOccurs
    case None => 1
  }

  lazy val isRoot = elementRef == None

  override lazy val isHidden = if (isRoot) false else elementRef.get.isHidden

  override lazy val enclosingComponent = elementRef.flatMap { _.enclosingComponent }

  override lazy val referringComponent: Option[SchemaComponent] = elementRef

  // GlobalElementDecls need to have access to elementRef's local properties.

  // We inherit the requirement for these attributes from Term. It all gets
  // too complicated in DSOM if you try to make GlobalElementDecl share with the other
  // element structures but not be a Term.
  //
  // But a GlobalElementDecl isn't really a Term except in a degenerate sense
  // that the root element is sort of a Term.
  //
  // In other words, we shouldn't be treating this as a term.
  //

  /**
   * global elements combined with element references referring to them can
   * be multiple occurring (aka arrays) hence, we have to have things
   * that take root and referenced situation into account.
   */
  override lazy val isScalar = minOccurs == 1 && maxOccurs == 1

}

