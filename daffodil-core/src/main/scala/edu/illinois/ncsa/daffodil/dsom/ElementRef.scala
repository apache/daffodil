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
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import scala.util.matching.Regex
import edu.illinois.ncsa.daffodil.dsom.Facet._
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.dpath.NodeInfo
import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType
import edu.illinois.ncsa.daffodil.grammar.ElementReferenceGrammarMixin

/**
 * Note ElementRef isn't a first class citizen with the other schema components.
 * It gets bypassed in that most things here just delegate to the GlobalElementDecl
 * that this references.
 *
 * Most of the framework expects to be handling elements via the ElementBase abstract
 * class. That class is responsible for testing and reaching back over to an ElementRef.
 *
 * So for example, to find out if an element has a property, an Element has to consider
 * that the property might be expressed on an element ref (if there is one), the element
 * itself, or a simpleType def or a base simple type def. Element does this. ElementRef
 * doesn't.
 */
final class ElementRef(xmlArg: Node, parent: ModelGroup, position: Int)
  extends LocalElementBase(xmlArg, parent, position)
  with ElementReferenceGrammarMixin
  with HasRefMixin
  with NamedMixin {

  requiredEvaluations(referencedElement)

  override def findPropertyOption(pname: String): PropertyLookupResult = {
    val res = referencedElement.findPropertyOption(pname)
    res
  }

  lazy val nonDefaultPropertySources = referencedElement.nonDefaultPropertySources
  lazy val defaultPropertySources = referencedElement.defaultPropertySources

  lazy val elementRef = None

  override lazy val referredToComponent = referencedElement

  override lazy val namedQName: NamedQName = LV('namedQName) {
    referencedElement.namedQName
  }.value

  // Need to go get the Element we are referencing
  final lazy val referencedElement: GlobalElementDecl = LV('referencedElement) {
    val ged = this.schemaSet.getGlobalElementDecl(refQName)
    val res = ged match {
      case None => SDE("Referenced element not found: %s.", this.ref)
      case Some(x) => x.forElementRef(this)
    }
    res
  }.value

  override lazy val runtimeData = referencedElement.runtimeData
  override lazy val termRuntimeData = referencedElement.termRuntimeData
  override def erd = referencedElement.erd
  override lazy val elementRuntimeData: ElementRuntimeData = LV('elementRuntimeData) {
    referencedElement.elementRuntimeData
  }.value

  // These will just delegate to the referenced element declaration
  def isNillable = referencedElement.isNillable
  def isSimpleType = referencedElement.isSimpleType
  def isComplexType = referencedElement.isComplexType
  def elementComplexType = referencedElement.elementComplexType
  def elementSimpleType = referencedElement.elementSimpleType
  def isDefaultable: Boolean = referencedElement.isDefaultable
  def defaultValueAsString = referencedElement.defaultValueAsString

  override lazy val namespace = namedQName.namespace

  override lazy val prettyName = "element." + name

  /**
   * valueOrElse....not just .value because when trying to get a diagnostic message out about
   * something, but then you get another failure just trying to get the
   * name of the thing that was causing the original diagnostic, so you
   * end up getting a completely inscrutable situation.
   *
   * So I made key things that are part of diagnostic messages have this
   * "always creates some value" behavior.
   *
   * Historic note:
   * I am hoping this problem will be less now. Some of it was because
   * we were failing validation, but then still running the rest of
   * the compiler which would then have errors it was not designed
   * to cope with like xs:element with no name or ref attribute.
   * Which would cause the above situation where just trying to get
   * the name was failing.
   */
  override lazy val name = nameFromRef
  private def nameFromRef = nameFromRef_.valueOrElse("?name?")
  private def nameFromRef_ = LV('nameFromRef) {
    namedQName.local
  }

  // TODO: perhaps many members of ElementRef are unused. 
  // Consider removing some. Although consider that
  // some have to be here because of abstract bases or traits requiring them
  // even if they aren't called.
  override def typeDef = referencedElement.typeDef

  // Element references can have minOccurs and maxOccurs, and annotations, but nothing else.
  override def inputValueCalcOption = referencedElement.inputValueCalcOption // can't have ivc on element reference
  override def outputValueCalcOption = referencedElement.outputValueCalcOption // can't have ivc on element reference

  //TODO: refactor and use shared code for creating resolved set of annotations for an annotation point.
  override lazy val statements = localStatements
  override lazy val newVariableInstanceStatements = localNewVariableInstanceStatements
  override lazy val assertStatements = localAssertStatements
  override lazy val discriminatorStatements = localDiscriminatorStatements
  override lazy val setVariableStatements = localSetVariableStatements

}
