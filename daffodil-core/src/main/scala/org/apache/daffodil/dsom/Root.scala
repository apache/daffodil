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

import org.apache.daffodil.grammar.RootGrammarMixin
import org.apache.daffodil.xml.NamedQName
import org.apache.daffodil.xml.XMLUtils
import scala.xml.Node
import scala.xml.UnprefixedAttribute

/**
 * Root is a special kind of ElementRef that has no enclosing group.
 *
 * This is the entity that is compiled by the schema compiler.
 */
final class Root(defXML: Node, parentArg: SchemaDocument,
  namedQNameArg: NamedQName,
  globalElementDecl: => GlobalElementDecl)
  extends AbstractElementRef(null, parentArg, 1)
  with RootGrammarMixin {

  final override lazy val xml = {
    val elem = XMLUtils.getXSDElement(defXML.scope)
    val res = elem % new UnprefixedAttribute("ref", refQName.toQNameString, scala.xml.Null)
    res
  }

  override lazy val refQName = namedQNameArg.toRefQName

  override lazy val referencedElement = globalElementDecl

  lazy val rootParseUnparsePolicy = defaultParseUnparsePolicy
}
