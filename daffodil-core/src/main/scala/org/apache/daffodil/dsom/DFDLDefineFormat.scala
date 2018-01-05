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
import scala.xml.Utility
import org.apache.daffodil.xml.XMLUtils

final class DFDLDefineFormat(node: Node, sd: SchemaDocument)
  extends DFDLDefiningAnnotation(node, sd) // Note: DefineFormat is not a format annotation
  {

  requiredEvaluations(formatAnnotation)

  // baseFormat was removed from the DFDL spec. Just use a ref from the
  // dfdl:format inside.
  // lazy val baseFormat = getAttributeOption("baseFormat") // nor baseFormat

  lazy val formatAnnotation = LV('formatAnnotation) {
    XMLUtils.removeComments(Utility.trim(node)) match {
      case <defineFormat>{ f @ <format>{ _* }</format> }</defineFormat> =>
        new DFDLFormat(f, sd)
      case _ =>
        schemaDefinitionError("dfdl:defineFormat does not contain a dfdl:format element.")
    }
  }.value

}
