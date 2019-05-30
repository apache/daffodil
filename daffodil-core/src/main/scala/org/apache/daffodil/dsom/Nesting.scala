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

trait NestingMixin {

  /** The lexically enclosing schema component */
  def lexicalParent: SchemaComponent

  /**
   * Define this for schema components that have back-references to ref
   * objects.
   * So group def to group ref, globalelementdecl to element ref,
   * type to element, base type to derived type.
   *
   * Not for format annotations however. We don't backpoint those to
   * other format annotations that ref them.
   */
  protected def enclosingComponentDef: Option[SchemaComponent]

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
  final lazy val enclosingComponent: Option[SchemaComponent] = enclosingComponentDef
}

/**
 * Mixin for all schema factories and schema components with no
 * backpointers, just a lexical parent. This means all the non-global
 * schema components.
 */
trait NestingLexicalMixin
  extends NestingMixin {

  override protected def enclosingComponentDef =
    if (lexicalParent eq null) None else Some(lexicalParent)

}

/**
 * Mixin for all global schema components
 */
trait NestingTraversesToReferenceMixin
  extends NestingMixin {

  def referringComponent: Option[SchemaComponent]

  final override protected def enclosingComponentDef: Option[SchemaComponent] = {
    Assert.invariant(lexicalParent.isInstanceOf[SchemaDocument]) // global things have schema documents as their parents.
    referringComponent
  }

}
