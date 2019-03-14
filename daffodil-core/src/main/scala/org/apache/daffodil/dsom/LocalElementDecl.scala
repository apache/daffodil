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

import scala.xml.Node

sealed class LocalElementDecl(
  final override val xml: Node,
  final override val parent: SchemaComponent,
  final override val position: Int)
  extends ElementBase
  with LocalElementComponentMixin
  with ElementDeclMixin
  with NestingLexicalMixin {

  requiredEvaluations(minOccurs, maxOccurs)
}

/**
 * A QuasiElement is similar to a LocalElement except it will have no
 * representation in the infoset, acting only as a temporary element that can
 * be parsed/unparsed. As an example, this is used as an element foar
 * parsing/unparsing prefix lengths. No element exists in the infoset or in the
 * schema to represent a prefix length (only a simple type), so a
 * DetachedElementDecl is used as a place where properties related to the
 * prefix simpel type can be accessed.
 */
sealed abstract class QuasiElementDeclBase(
  val detachedReference: ElementBase,
  xml: Node,
  parent: SchemaComponent)
  extends LocalElementDecl(xml, parent, -1){
  
  override lazy val isQuasiElement = true
}

final class PrefixLengthQuasiElementDecl(
  detachedReference: ElementBase,
  xml: Node,
  parent: SchemaComponent)
  extends QuasiElementDeclBase(detachedReference, xml, parent){
}

final class RepTypeQuasiElementDecl(
  detachedReference: ElementBase,
  xml: Node,
  parent: SchemaComponent)
  extends QuasiElementDeclBase(detachedReference, xml, parent){
  
}
