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

package org.apache.daffodil.dsom

import scala.collection.mutable
import scala.xml.Node
import scala.xml.UnprefixedAttribute

import org.apache.daffodil.dsom.walker.RootView
import org.apache.daffodil.grammar.RootGrammarMixin
import org.apache.daffodil.xml.NamedQName
import org.apache.daffodil.xml.XMLUtils

/**
 * Root is a special kind of ElementRef that has no enclosing group.
 *
 * This is the entity that is compiled by the schema compiler.
 */
final class Root(defXML: Node, parentArg: SchemaDocument,
  namedQNameArg: NamedQName,
  globalElementDecl: GlobalElementDecl)
  extends AbstractElementRef(null, parentArg, 1)
  with RootGrammarMixin
  with RootView {

  requiredEvaluationsAlways({
    val ac = allComponents
    ac.foreach {
      _.setRequiredEvaluationsActive()
    }
  })

  final override lazy val xml = {
    val elem = XMLUtils.getXSDElement(defXML.scope)
    val res = elem % new UnprefixedAttribute("ref", refQName.toQNameString, scala.xml.Null)
    res
  }

  override lazy val refQName = namedQNameArg.toRefQName

  override lazy val referencedElement = globalElementDecl

  lazy val rootParseUnparsePolicy = defaultParseUnparsePolicy

  /**
   * For any given global schema component, tells us what schema components contain
   * references to it, and if that reference is from a sequence or choice, the index of
   * the group member within that sequence/choice.
   *
   * This is intended to be used when compilation needs to understand
   * the context where an object is referenced. This allows the various referencing contexts
   * to be known, without making copies of schema components for each such context.
   */
  lazy val refMap: Map[GlobalComponent, Seq[(String, Seq[RefSpec])]] = {
    val refEntries: Seq[(GlobalComponent, Seq[RefSpec])] =
      refTargets.groupBy { _.to }.toSeq
    val m: Seq[(GlobalComponent, Seq[(String, Seq[RefSpec])])] = refEntries.map {
      case (to, seq) => (to, seq.groupBy { _.from.shortSchemaComponentDesignator }.toSeq)
    }
    m.toMap
  }

  final def elementRefsTo(decl: GlobalElementDecl): Seq[ElementRef] =
    refsTo(decl).asInstanceOf[Seq[ElementRef]]

  final def groupRefsTo(gdef: GlobalGroupDef): Seq[ModelGroup with GroupRef] =
    refsTo(gdef).asInstanceOf[Seq[ModelGroup with GroupRef]]

  private def refsTo(g: GlobalComponent): Seq[SchemaComponent] =
    refMap.get(g).toSeq.flatMap { refSpec =>
      refSpec.flatMap {
        case (_, seqRS) =>
          seqRS.map { rs: RefSpec => rs.from }
      }
    }

  /**
   * Used in unit testing
   */
  private[dsom] lazy val refPairsMap: Map[GlobalComponent, Seq[String]] = {
    refMap.toSeq.map {
      case (to, seq: Seq[(String, _)]) => (to, seq.map { case (sscd, _) => sscd }.toSeq)
    }.toMap
  }

  private lazy val allComponentsSet = new mutable.HashSet[SchemaComponent]

  private def allSchemaComponents(component: SchemaComponent, optIndex: Option[Int]): Unit = {
    if (allComponentsSet.contains(component)) {
      // ok
    } else {
      allComponentsSet.add(component)
      component match {
        case aer: AbstractElementRef => {
          val edecl = aer.referencedElement
          allSchemaComponents(edecl, None)
          if (!aer.isRepresented) {
            // ok // could be inputValueCalc with typeCalc expression referring to a type.
            // TODO: We're not finding those relationships. But typeCalc is in flux. Not sure we want to fix it.
          }
          if (aer.outputValueCalcOption.isDefined) {
            // ok // could be outputValueCalc option with typeCalc expression referring to a type.
            // TODO: We're not finding those relationships. But typeCalc is in flux. Not sure we want to fix it.
          }
        }
        case edecl: ElementDeclMixin => edecl.typeDef match {
          case std: SimpleTypeDefBase => {
            allSchemaComponents(std, None)
            std.bases.foreach { allSchemaComponents(_, None) }
            std.optRepTypeDef.foreach { allSchemaComponents(_, None) }
          }
          case ctd: ComplexTypeBase => allSchemaComponents(ctd, None)
          case pt: PrimitiveType => {
            // An element decl with primitive type can still reference a simple type by way
            // of dfdl:inputValueCalc that calls dfdlx:inputTypeCalc('QNameOfType', ....)
            //
            // TBD: ok for now, but needs addressing.
          }
        }
        case gr: GroupRef => allSchemaComponents(gr.groupDef, None)
        case ct: ComplexTypeBase => allSchemaComponents(ct.modelGroup, None)
        case mg: ModelGroup => mg.groupMembers.foreach { gm =>
          allSchemaComponents(gm, Some(gm.position))
        }
        case gstd: GlobalSimpleTypeDef => gstd.bases.foreach { allSchemaComponents(_, None) }
        case gd: GlobalGroupDef => gd.groupMembers.foreach { gm =>
          allSchemaComponents(gm, Some(gm.position))
        }
        case std: SimpleTypeDefBase => {
          std.bases.foreach { allSchemaComponents(_, None) }
        }
      }
    }
  }

  final lazy val allComponents = {
    allSchemaComponents(this, None)
    allComponentsSet.toSeq
  }

  final lazy val numComponents =
    allComponents.length

  final lazy val allComponentSSCDs =
    allComponents.map { _.shortSchemaComponentDesignator }.distinct

  final lazy val numUniqueComponents =
    allComponentSSCDs.length

  final lazy val refTargets: Seq[RefSpec] = {
    allComponents.collect {
      case ed: ElementDeclMixin => {
        val optGSTD = ed.optNamedSimpleType.collect { case gstd: GlobalSimpleTypeDef => gstd }
        val optNamedTypeDef = optGSTD ++ ed.optNamedComplexType
        optNamedTypeDef.map { ntd => RefSpec(ed, ntd, 1) }.toSeq
      }
      case er: AbstractElementRef => {
        val ed = er.referencedElement
        RefSpec(er, ed, er.position) +:
          ed.optNamedComplexType.map { gctd => RefSpec(ed, gctd, 1) }.toSeq
      }
      case gr: GroupRef => Seq(RefSpec(gr, gr.groupDef, gr.asModelGroup.position))
    }.flatten
  }

  lazy val allERefs = allComponents.filter {
    case er: ElementRef => true
    case _ => false
  }.map { _.shortSchemaComponentDesignator }.distinct

  lazy val allGRefs = allComponents.filter {
    case _: GroupRef => true
    case _ => false
  }.map { _.shortSchemaComponentDesignator }.distinct

  lazy val allCTRefs = {
    val cts = allComponents.collect {
      case e: ElementDeclMixin if (
        e.optComplexType.isDefined &&
        e.complexType.isInstanceOf[GlobalComplexTypeDef]) =>
        e.complexType
    }
    val ctsIDs = cts.map { _.shortSchemaComponentDesignator }.distinct
    ctsIDs
  }
}

case class RefSpec(from: SchemaComponent, to: GlobalComponent, index: Int) {

  override def toString = "RefSpec(from=" +
    from.shortSchemaComponentDesignator + ", to=" +
    to.shortSchemaComponentDesignator + ", " + index +
    ")"
}
