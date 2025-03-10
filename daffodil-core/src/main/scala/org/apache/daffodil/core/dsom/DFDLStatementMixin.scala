/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.core.dsom

import scala.xml.Elem
import scala.xml.Node

import org.apache.daffodil.core.grammar.primitives.AssertBase
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.schema.annotation.props.gen.TestKind
import org.apache.daffodil.runtime1.dsom._

trait ResolvesDFDLStatementMixin extends ThrowsSDE with ProvidesDFDLStatementMixin {
  self: Term =>

  requiredEvaluationsIfActivated(statements)

  final lazy val statements: Seq[DFDLStatement] =
    optReferencedStatementSource.toSeq.flatMap { _.resolvedStatements } ++
      localStatements

  private def getParserExprReferencedElements(
    s: DFDLStatement,
    f: ContentValueReferencedElementInfoMixin => Set[DPathElementCompileInfo]
  ) = {
    s match {
      case a: DFDLAssertionBase if (a.testKind eq TestKind.Expression) => {
        a.gram(self) match {
          case ab: AssertBase => f(ab.expr)
          case _ => ReferencedElementInfos.None
        }
      }
      case _ => getUnparserExprReferencedElements(s, f)
    }
  }

  private def getUnparserExprReferencedElements(
    s: DFDLStatement,
    f: ContentValueReferencedElementInfoMixin => Set[DPathElementCompileInfo]
  ) = {
    s match {
      case sv: DFDLSetVariable => {
        val v = sv.gram(self).expr
        f(v)
      }
      case nv: DFDLNewVariableInstance => {
        val mdv = nv.maybeDefaultValueExpr
        if (mdv.isDefined)
          f(mdv.get)
        else
          ReferencedElementInfos.None
      }
      case _ => ReferencedElementInfos.None
    }
  }

  final protected def creis(rei: ContentValueReferencedElementInfoMixin) =
    rei.contentReferencedElementInfos

  final protected def vreis(rei: ContentValueReferencedElementInfoMixin) =
    rei.valueReferencedElementInfos

  private def statementReferencedElementInfos(
    f: DFDLStatement => Set[DPathElementCompileInfo]
  ) = {
    val stmtSets: Seq[DPathElementCompileInfo] = {
      val s = resolvedStatements
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

/**
 * The other kind of DFDL annotations are DFDL 'statements'.
 * This trait is everything shared by schema components that can carry
 * statements.
 *
 * Factory for creating the corresponding DFDLAnnotation objects.
 */
trait ProvidesDFDLStatementMixin extends ThrowsSDE with HasTermCheck {
  self: AnnotatedSchemaComponent =>

  final def annotationFactoryForDFDLStatement(
    node: Node,
    self: AnnotatedSchemaComponent
  ): Option[DFDLAnnotation] = {
    val term = self
    node match {
      case Elem("dfdl", "assert", _, _, _*) => Some(new DFDLAssert(node, term))
      case Elem("dfdl", "discriminator", _, _, _*) => Some(new DFDLDiscriminator(node, term))
      case Elem("dfdl", "setVariable", _, _, _*) => Some(new DFDLSetVariable(node, term))
      case Elem("dfdl", "newVariableInstance", _, _, _*) =>
        Some(new DFDLNewVariableInstance(node, term))
      //
      // property element annotations aren't "statements" so we don't want them back from this
      // and in fact can't construct them here without causing trouble (circular definitions)
      //
      case Elem("dfdl", "property", _, _, _*) =>
        SDE(
          "A dfdl:property annotation element is not allowed without a surrounding dfdl:format, dfdl:element, etc. "
        )
      case e: scala.xml.Elem if mismatchedFormatAnnotation(e) =>
        SDE(
          "DFDL annotation type 'dfdl:%s' invalid. Expected 'dfdl:%s'.",
          e.label,
          self.formatAnnotationExpectedName
        )
      case _ => SDE("Invalid DFDL annotation found: %s", node)
    }
  }

  private def mismatchedFormatAnnotation(e: scala.xml.Elem): Boolean = {
    (e.label, self) match {
      case ("element", _: ElementBase) => false
      case ("choice", _: ChoiceTermBase) => false
      case ("sequence", _: SequenceTermBase) => false
      case ("simpleType", _: SimpleTypeBase) => false
      case _ => true
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

  /**
   * Combine our statements with those of what we reference.
   * Elements reference types
   * ElementRefs reference elements, etc.
   *
   * The order here is important. The statements from type come first, then from declaration, then from
   * reference.
   */

  final lazy val optReferencedStatementSource: Option[ProvidesDFDLStatementMixin] =
    self.optReferredToComponent.asInstanceOf[Option[ProvidesDFDLStatementMixin]]

  final lazy val resolvedStatements: Seq[DFDLStatement] =
    optReferencedStatementSource.toSeq.flatMap { _.resolvedStatements } ++ localStatements

  final lazy val newVariableInstanceStatements: Seq[DFDLNewVariableInstance] =
    optReferencedStatementSource.toSeq.flatMap { _.newVariableInstanceStatements } ++
      localNewVariableInstanceStatements

  final lazy val (discriminatorStatements, assertStatements) =
    checkDiscriminatorsAssertsDisjoint(combinedDiscrims, combinedAsserts)

  private lazy val combinedAsserts: Seq[DFDLAssert] =
    optReferencedStatementSource.toSeq.flatMap { _.assertStatements } ++ localAssertStatements

  private lazy val combinedDiscrims: Seq[DFDLDiscriminator] =
    optReferencedStatementSource.toSeq.flatMap {
      _.discriminatorStatements
    } ++ localDiscriminatorStatements

  final lazy val setVariableStatements: Seq[DFDLSetVariable] = {
    val combinedSvs = optReferencedStatementSource.toSeq.flatMap {
      _.setVariableStatements
    } ++ localSetVariableStatements
    checkDistinctVariableNames(combinedSvs)
  }

  private lazy val patternAsserts: Seq[DFDLAssert] = combinedAsserts.filter { st =>
    st.testKind == TestKind.Pattern
  }
  private lazy val nonPatternAsserts: Seq[DFDLAssert] = combinedAsserts.filter { st =>
    st.testKind != TestKind.Pattern
  }

  private lazy val patternDiscrims: Seq[DFDLDiscriminator] = combinedDiscrims.filter { st =>
    st.testKind == TestKind.Pattern
  }
  private lazy val nonPatternDiscrims: Seq[DFDLDiscriminator] = combinedDiscrims.filter { st =>
    st.testKind != TestKind.Pattern
  }

  final lazy val patternStatements: Seq[DFDLStatement] = patternAsserts ++ patternDiscrims

  final lazy val lowPriorityStatements: Seq[DFDLStatement] =
    setVariableStatements ++ nonPatternAsserts ++ nonPatternDiscrims

  final protected lazy val localStatements = this.annotationObjs.collect {
    case st: DFDLStatement => st
  }

  private lazy val localNewVariableInstanceStatements = {
    val nvis = localStatements.collect { case nve: DFDLNewVariableInstance => nve }
    checkDistinctNewVariableInstances(nvis)
  }

  final protected lazy val (localDiscriminatorStatements, localAssertStatements) = {
    val discrims = localStatements.collect { case disc: DFDLDiscriminator => disc }
    val asserts = localStatements.collect { case asrt: DFDLAssert => asrt }
    checkDiscriminatorsAssertsDisjoint(discrims, asserts)
  }

  private def checkDiscriminatorsAssertsDisjoint(
    discrims: Seq[DFDLDiscriminator],
    asserts: Seq[DFDLAssert]
  ): (Seq[DFDLDiscriminator], Seq[DFDLAssert]) = {
    schemaDefinitionUnless(
      discrims.size <= 1,
      "At most one discriminator allowed at same location: %s",
      discrims
    )
    schemaDefinitionUnless(
      asserts == Nil || discrims == Nil,
      "Cannot have both dfdl:discriminator annotations and dfdl:assert annotations at the same location."
    )
    (discrims, asserts)
  }

  private def checkDistinctVariableNames(svs: Seq[DFDLSetVariable]) = {
    val names = svs.map { _.defv.globalQName }
    val areAllDistinct = names.distinct.size == names.size
    schemaDefinitionUnless(
      areAllDistinct,
      "Variables referenced by setVariable must all be distinct at the same location: %s",
      names.distinct
    )
    svs
  }

  private def checkDistinctNewVariableInstances(nvis: Seq[DFDLNewVariableInstance]) = {
    val names = nvis.map { _.defv.globalQName }
    val areAllDistinct = names.distinct.size == names.size
    schemaDefinitionUnless(
      areAllDistinct,
      "Variables referenced by newVariableInstances must all be distinct within the same scope: %s",
      names.distinct
    )
    nvis
  }

  final protected lazy val localSetVariableStatements = {
    val svs = localStatements.collect { case sv: DFDLSetVariable => sv }
    checkDistinctVariableNames(svs)
  }
}
