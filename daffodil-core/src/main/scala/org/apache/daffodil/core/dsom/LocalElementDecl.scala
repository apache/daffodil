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
import scala.xml.TopScope

import org.apache.daffodil.core.runtime1.LocalElementDeclBaseRuntime1Mixin

sealed abstract class LocalElementDeclBase(
  final override val xml: Node,
  final override val optLexicalParent: Option[SchemaComponent],
  final override val position: Int
) extends ElementBase
  with LocalElementComponentMixin
  with ElementDeclMixin
  with NestingLexicalMixin
  with LocalElementDeclBaseRuntime1Mixin {

  requiredEvaluationsIfActivated(minOccurs)
  requiredEvaluationsIfActivated(maxOccurs)
}

object LocalElementDecl {
  def apply(xml: Node, lexicalParent: SchemaComponent, position: Int) = {
    val led = new LocalElementDecl(xml, lexicalParent, position)
    led.initialize()
    led
  }
}
class LocalElementDecl private (xml: Node, lexicalParent: SchemaComponent, position: Int)
  extends LocalElementDeclBase(xml, Option(lexicalParent), position)

/**
 * A QuasiElement is similar to a LocalElement except it will have no
 * representation in the infoset, acting only as a temporary element that can
 * be parsed/unparsed. As an example, this is used as an element for
 * parsing/unparsing prefix lengths. No element exists in the infoset or in the
 * schema to represent a prefix length (only a simple type), so a
 * quasi-element is used as a place where properties related to the
 * prefix simple type can be accessed.
 */
sealed abstract class QuasiElementDeclBase(xml: Node, lexicalParent: SchemaComponent)
  extends LocalElementDeclBase(xml, Option(lexicalParent), -1) {

  override lazy val minimizedScope = TopScope

  override def isQuasiElement = true
}

object PrefixLengthQuasiElementDecl {
  def apply(xml: Node, lexicalParent: SchemaComponent) = {
    val pl = new PrefixLengthQuasiElementDecl(xml, lexicalParent)
    pl.initialize()
    pl
  }
}

class PrefixLengthQuasiElementDecl private (xml: Node, lexicalParent: SchemaComponent)
  extends QuasiElementDeclBase(xml, lexicalParent) {}

object RepTypeQuasiElementDecl {
  def apply(xml: Node, lexicalParent: SchemaComponent) = {
    val rt = new RepTypeQuasiElementDecl(xml, lexicalParent)
    rt.initialize()
    rt
  }
}

class RepTypeQuasiElementDecl private (xml: Node, lexicalParent: SchemaComponent)
  extends QuasiElementDeclBase(xml, lexicalParent) {}
