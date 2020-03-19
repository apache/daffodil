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

final class GlobalElementDecl(
  xmlArg: Node,
  schemaDocument: SchemaDocument)
  extends AnnotatedSchemaComponentImpl(xmlArg, schemaDocument)
  with GlobalElementComponentMixin
  with ElementDeclMixin
  with NestingLexicalMixin
  // Needed to check some properties are
  // NOT on global element decls such as dfdl:choiceBranchKey
  with ResolvesLocalProperties {

  lazy val asRoot = new Root(xml, schemaDocument, namedQName, this)

  requiredEvaluationsIfActivated(validateChoiceBranchKey)

  private def validateChoiceBranchKey(): Unit = {
    // Ensure that the global element decl does not have choiceBranchKey set.
    val found = findPropertyOption("choiceBranchKey")
    if (found.isDefined) {
      SDE("dfdl:choiceBranchKey cannot be specified on a global element declaration")
    }
  }
}
