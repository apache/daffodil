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

import scala.xml.UnprefixedAttribute
import scala.xml.Null
import scala.xml.Elem
import org.apache.daffodil.processors.RepValueSet

/*
 * To make use of a reptype, it is common for the parser/unparser to need an element associated with said type
 * Since such an element does not nessasarily exist in the schema itself, those elements which have a reptype also
 * need to get a quasi element with said type.
 */
trait HasOptRepTypeMixinImpl extends SchemaComponent with HasOptRepTypeMixin {
  
  override lazy val optRepTypeElement: Option[RepTypeQuasiElementDecl] = optRepTypeFactory.map(repType => {
    val xmlElem = Elem(null, "QuasiElementForTypeCalc", new UnprefixedAttribute("type", repType.namedQName.toAttributeNameString, Null), namespaces, true)
    new RepTypeQuasiElementDecl(this.enclosingElement.get, xmlElem, parent)
  })

}

trait HasOptRepTypeMixin {
  def optRepTypeFactory: Option[SimpleTypeFactory with NamedMixin]
  
  lazy val optRepTypeDefFactory: Option[SimpleTypeDefFactory with NamedMixin] = optRepTypeFactory match {
    case Some(x:SimpleTypeDefFactory) => Some(x)
    case _ => None
  }
  def optRepValueSet: Option[RepValueSet[AnyRef]]
  
  def optRepTypeElement: Option[RepTypeQuasiElementDecl]

}
