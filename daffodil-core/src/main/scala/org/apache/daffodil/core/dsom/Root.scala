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

import scala.xml.Node
import scala.xml.UnprefixedAttribute

import org.apache.daffodil.core.dsom.walker.RootView
import org.apache.daffodil.core.grammar.RootGrammarMixin
import org.apache.daffodil.lib.xml.NamedQName
import org.apache.daffodil.lib.xml.XMLUtils

object Root {
  def apply(
    defXML: Node,
    parentArg: SchemaDocument,
    namedQNameArg: NamedQName,
    globalElementDecl: GlobalElementDecl
  ) = {
    val r = new Root(defXML, parentArg, namedQNameArg, globalElementDecl)
    r.initialize()
    r
  }
}

/**
 * Root is a special kind of ElementRef that has no enclosing group.
 *
 * This is the entity that is compiled by the schema compiler.
 */
final class Root private (
  defXML: Node,
  parentArg: SchemaDocument,
  namedQNameArg: NamedQName,
  globalElementDecl: GlobalElementDecl
) extends AbstractElementRef(null, parentArg, 1)
  with RootGrammarMixin
  with RootView {

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
      refSpec.flatMap { case (_, seqRS) =>
        seqRS.map { (rs: RefSpec) => rs.from }
      }
    }

  /**
   * Used in unit testing
   */
  private[dsom] lazy val refPairsMap: Map[GlobalComponent, Seq[String]] = {
    refMap.toSeq.map { case (to, seq: Seq[(String, _)]) =>
      (to, seq.map { case (sscd, _) => sscd }.toSeq)
    }.toMap
  }

  /*
   * Important: the back-pointers allowing a shared object to know what is
   * referencing it, those are constructed from this allComponents list.
   * This implies that no reference to those things can occur in any
   * computation needed to construct the allComponents list.
   *
   * So anything using the back-pointers (eg., enclosingComponents member)
   * or anything derived from that, is effectively in a second pass that has to
   * happen AFTER allComponents is computed.
   */
  def allComponents = schemaSet.allSchemaComponents

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

  lazy val allERefs = allComponents
    .filter {
      case er: ElementRef => true
      case _ => false
    }
    .map { _.shortSchemaComponentDesignator }
    .distinct

  lazy val allGRefs = allComponents
    .filter {
      case _: GroupRef => true
      case _ => false
    }
    .map { _.shortSchemaComponentDesignator }
    .distinct

  lazy val allCTRefs = {
    val cts = allComponents.collect {
      case e: ElementDeclMixin
          if (e.optComplexType.isDefined &&
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
