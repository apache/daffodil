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

import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.TestKind
import edu.illinois.ncsa.daffodil.grammar.primitives.AssertBase

/**
 * The other kind of DFDL annotations are DFDL 'statements'.
 * This trait is everything shared by schema components that can have
 * statements.
 *
 * Factory for creating the corresponding DFDLAnnotation objects.
 */
trait DFDLStatementMixin extends ThrowsSDE { self: AnnotatedSchemaComponent =>

  requiredEvaluations(statements)

  protected final def annotationFactoryForDFDLStatement(node: Node, self: AnnotatedSchemaComponent): Option[DFDLAnnotation] = {
    node match {
      case <dfdl:assert>{ content @ _* }</dfdl:assert> => Some(new DFDLAssert(node, self))
      case <dfdl:discriminator>{ content @ _* }</dfdl:discriminator> => Some(new DFDLDiscriminator(node, self))
      case <dfdl:setVariable>{ content @ _* }</dfdl:setVariable> => Some(new DFDLSetVariable(node, self))
      case <dfdl:newVariableInstance>{ content @ _* }</dfdl:newVariableInstance> => Some(new DFDLNewVariableInstance(node, self))
      //
      // property element annotations aren't "statements" so we don't want them back from this
      // and in fact can't construct them here without causing trouble (circular definitions)
      //
      case <dfdl:property>{ _* }</dfdl:property> =>
        SDE("A dfdl:property annotation element is not allowed without a surrounding dfdl:format, dfdl:element, etc. ")
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
    val names = svs.map { _.defv.globalQName }
    val areAllDistinct = names.distinct.size == names.size
    schemaDefinitionUnless(areAllDistinct, "Variable names must all be distinct at the same location: %s", names)
    svs
  }

  final lazy val localSetVariableStatements = {
    val svs = localStatements.collect { case sv: DFDLSetVariable => sv }
    checkDistinctVariableNames(svs)
  }

  private def getParserExprReferencedElements(s: DFDLStatement,
    f: ContentValueReferencedElementInfoMixin => Set[DPathElementCompileInfo]) = {
    s match {

      case a: DFDLAssertionBase if (a.testKind eq TestKind.Expression) => {
        a.gram match {
          case ab: AssertBase => f(ab.expr)
          case _ => ReferencedElementInfos.None
        }
      }

      case _ => getUnparserExprReferencedElements(s, f)
    }
  }

  private def getUnparserExprReferencedElements(s: DFDLStatement,
    f: ContentValueReferencedElementInfoMixin => Set[DPathElementCompileInfo]) = {
    s match {
      case sv: DFDLSetVariable => {
        val mdv = sv.defv.maybeDefaultValueExpr
        if (mdv.isDefined)
          f(mdv.get)
        else
          ReferencedElementInfos.None
      }
      case nv: DFDLNewVariableInstance => {
        // nv.defaultValueExpr.contentReferencedElementInfos
        s.notYetImplemented("dfdl:newVariableInstance")
      }
      case _ => ReferencedElementInfos.None
    }
  }

  private def creis(rei: ContentValueReferencedElementInfoMixin) = rei.contentReferencedElementInfos
  private def vreis(rei: ContentValueReferencedElementInfoMixin) = rei.valueReferencedElementInfos

  private def statementReferencedElementInfos(f: DFDLStatement => Set[DPathElementCompileInfo]) = {

    val stmtSets: Seq[DPathElementCompileInfo] = {
      val s = statements
      val sets = s.flatMap(f)
      sets
    }
    stmtSets.toSet
  }

  final protected lazy val statementContentParserReferencedElementInfos =
    statementReferencedElementInfos(x => getParserExprReferencedElements(x, creis(_)))

  final protected lazy val statementContentUnparserReferencedElementInfos =
    statementReferencedElementInfos(x => getUnparserExprReferencedElements(x, creis(_)))

  final protected lazy val statementValueParserReferencedElementInfos =
    statementReferencedElementInfos(x => getParserExprReferencedElements(x, vreis(_)))

  final protected lazy val statementValueUnparserReferencedElementInfos =
    statementReferencedElementInfos(x => getUnparserExprReferencedElements(x, vreis(_)))
}
