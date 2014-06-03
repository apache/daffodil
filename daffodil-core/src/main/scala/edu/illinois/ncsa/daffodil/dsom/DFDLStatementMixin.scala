package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import scala.xml.Node

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

/**
 * The other kind of DFDL annotations are DFDL 'statements'.
 * This trait is everything shared by schema components that can have
 * statements.
 *
 * Factory for creating the corresponding DFDLAnnotation objects.
 */
trait DFDLStatementMixin extends ThrowsSDE { self: AnnotatedSchemaComponent =>

  requiredEvaluations(statements)

  def annotationFactoryForDFDLStatement(node: Node, self: AnnotatedSchemaComponent): DFDLAnnotation = {
    node match {
      case <dfdl:assert>{ content @ _* }</dfdl:assert> => new DFDLAssert(node, self)
      case <dfdl:discriminator>{ content @ _* }</dfdl:discriminator> => new DFDLDiscriminator(node, self)
      case <dfdl:setVariable>{ content @ _* }</dfdl:setVariable> => new DFDLSetVariable(node, self)
      case <dfdl:newVariableInstance>{ content @ _* }</dfdl:newVariableInstance> => new DFDLNewVariableInstance(node, self)
      case _ => SDE("Invalid DFDL annotation found: %s", node)
    }
  }

  /**
   * Validation won't check whether these are validly in place on a DFDL schema, so
   * we allow any annotated object to have them, and then we can do checking on this list
   * to enforce rules about which kinds of statements are allowed and where.
   *
   * Implement these abstract methods to do the right thing w.r.t. combining
   * statements from group refs and their referenced groups, element refs and their elements,
   * element decls and their simple types, simpleTypes and their base simpleTypes.
   *
   * The local ingredients are here for doing the needed combining and also for checking.
   * E.g., dfdl:newVariableInstance isn't allowed on simpleType, can only have one discriminator per
   * annotation point, and per combined annotation point, discriminators and assertions exclude each other, etc.
   */
  def statements: Seq[DFDLStatement]
  def newVariableInstanceStatements: Seq[DFDLNewVariableInstance]
  final lazy val notNewVariableInstanceStatements = setVariableStatements ++ discriminatorStatements ++ assertStatements
  def assertStatements: Seq[DFDLAssert]
  def discriminatorStatements: Seq[DFDLDiscriminator]
  def setVariableStatements: Seq[DFDLSetVariable]

  final lazy val localStatements = this.annotationObjs.collect { case st: DFDLStatement => st }
  final lazy val localNewVariableInstanceStatements = localStatements.collect { case nve: DFDLNewVariableInstance => nve }
  final lazy val localNotNewVariableInstanceStatements = localStatements.diff(localNewVariableInstanceStatements)
  final lazy val (localDiscriminatorStatements,
    localAssertStatements) = {
    val discrims = localStatements.collect { case disc: DFDLDiscriminator => disc }
    val asserts = localStatements.collect { case asrt: DFDLAssert => asrt }
    checkDiscriminatorsAssertsDisjoint(discrims, asserts)
  }

  final def checkDiscriminatorsAssertsDisjoint(discrims: Seq[DFDLDiscriminator], asserts: Seq[DFDLAssert]): (Seq[DFDLDiscriminator], Seq[DFDLAssert]) = {
    schemaDefinitionUnless(discrims.size <= 1, "At most one discriminator allowed at same location: %s", discrims)
    schemaDefinitionUnless(asserts == Nil || discrims == Nil,
      "Cannot have both dfdl:discriminator annotations and dfdl:assert annotations at the same location.")
    (discrims, asserts)
  }

  final def checkDistinctVariableNames(svs: Seq[DFDLSetVariable]) = {
    val names = svs.map { _.defv.extName }
    val areAllDistinct = names.distinct.size == names.size
    schemaDefinitionUnless(areAllDistinct, "Variable names must all be distinct at the same location: %s", names)
    svs
  }

  final lazy val localSetVariableStatements = {
    val svs = localStatements.collect { case sv: DFDLSetVariable => sv }
    checkDistinctVariableNames(svs)
  }

}
