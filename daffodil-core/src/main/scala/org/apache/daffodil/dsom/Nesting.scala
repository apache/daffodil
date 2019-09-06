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

import org.apache.daffodil.exceptions.Assert

/**
 * When a global schema component is referenced, the "backpointer" is represented by
 * this class. The lexical position is the index within the enclosing construct's lexical parent.
 * For example, if an element ref is referencing a global element decl, then the encloser is
 * the elementRef, and the lexical position is which index, within the sequence/choice that
 * contains that particular elementRef.
 */
case class EnclosingComponentDef(encloser: SchemaComponent, lexicalPosition: Int)

trait NestingMixin {

  /** The lexically enclosing schema component */
  def optLexicalParent: Option[SchemaComponent]

  /**
   * Define this for schema components that have back-references to ref
   * objects.
   * So group def to group ref, globalelementdecl to element ref,
   * type to element, base type to derived type.
   *
   * Not for format annotations however. We don't backpoint those to
   * other format annotations that ref them.
   */
  //
  // Uncomment this to chase down these usages and revise them.
  //
  //
  //@deprecated("2019-06-03", "Use enclosingComponentDefs method, and deal with sharing.")
  protected def enclosingComponentDef: Option[SchemaComponent]

  protected def enclosingComponentDefs: Seq[EnclosingComponentDef]

  def shortSchemaComponentDesignator: String

  /**
   * The enclosing component, and follows back-references
   *  from types to their elements, from globalElementDef to
   *  elementRefs, from simpleType defs to derived simpletype defs,
   *  from global group defs to group refs
   *
   *  Note: the enclosing component of a global element or global group
   *  referenced from a element ref or group ref, is NOT the ref object, but the
   *  component that contains the ref object
   */
  //
  // Uncomment this to chase down these usages and revise them.
  //
  //
  //@deprecated("2019-06-03", "Rewrite to use lexicalParent or enclosingComponents methods, and deal with sharing.")
  final lazy val enclosingComponent: Option[SchemaComponent] = enclosingComponentDef

  final lazy val enclosingComponents: Seq[EnclosingComponentDef] = {
    val res = enclosingComponentDefs
    //    System.err.println("enclosingComponents: Component " + this.shortSchemaComponentDesignator + (
    //      if (res.isEmpty) " has no enclosing component."
    //      else " has enclosing components " + res.map { _.encloser.shortSchemaComponentDesignator }.mkString(", ")))
    res
  }
}

/**
 * Mixin for all schema factories and schema components with no
 * backpointers, just a lexical parent. This means all the non-global
 * schema components.
 */
trait NestingLexicalMixin
  extends NestingMixin { self: SchemaComponent =>

  //@deprecated("2019-06-03", "Use enclosingComponentDefs method, and deal with sharing.")
  override protected lazy val enclosingComponentDef = optLexicalParent

  final override protected lazy val enclosingComponentDefs: Seq[EnclosingComponentDef] =
    optLexicalParent.map {
      lp =>
        val pos = self match {
          case t: Term => t.position
          case _ => 1
        }
        EnclosingComponentDef(lp, pos)
    }.toSeq
}

/**
 * Mixin for all global schema components
 */
trait NestingTraversesToReferenceMixin
  extends NestingMixin { self: SchemaComponent =>

  def factory: SchemaComponentFactory

  //@deprecated("2019-06-03", "Use referringComponents method, and deal with sharing.")
  def referringComponent: Option[SchemaComponent]

  //@deprecated("2019-06-03", "Use enclosingComponentDefs method, and deal with sharing.")
  final override protected lazy val enclosingComponentDef: Option[SchemaComponent] = LV('enclosingComponentDef) {
    Assert.invariant(optLexicalParent.isDefined &&
      optLexicalParent.get.isInstanceOf[SchemaDocument]) // global things have schema documents as their parents.
    referringComponent
  }.value

  /**
   * Enables compilation to know all the points of use of a global
   * component.
   */
  lazy val referringComponents: Seq[(String, Seq[RefSpec])] = {
    schemaSet.root.refMap.get(this.factory) match {
      case None => Seq()
      case Some(seq) => seq
    }
  }

  final override protected lazy val enclosingComponentDefs: Seq[EnclosingComponentDef] = LV('enclosingComponentDefs) {
    Assert.invariant(optLexicalParent.isDefined &&
      optLexicalParent.get.isInstanceOf[SchemaDocument]) // global things have schema documents as their parents.
    val res = referringComponents.flatMap {
      case (_, seq: Seq[RefSpec]) =>
        seq.map { rs =>
          EnclosingComponentDef(rs.from, rs.index)
        }
    }
    res
  }.value
}
