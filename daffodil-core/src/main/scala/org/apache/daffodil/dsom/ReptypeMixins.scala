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

  override lazy val optRepTypeElement: Option[RepTypeQuasiElementDecl] =
    optRepTypeDef.map(repType => {
      val xmlElem = Elem(null, "QuasiElementForTypeCalc", new UnprefixedAttribute("type", repType.namedQName.toAttributeNameString, Null), namespaces, true)
      RepTypeQuasiElementDecl(xmlElem, this)
    })

}

trait HasOptRepTypeMixin {

  /**
   *  Provides the repType. Can be a "def" or a primitive type.
   *
   *  optRepType and optRepTypeDef are different, because it is possible for the
   *  repType to be a primitive type. The resulting type would be difficult
   *  (maybe impossible?) to use as the type for an element, as a primitive repType
   *  would be missing any annotations that tell Daffodil what the physical
   *  characteristics are; however the type can be used purely to define a
   *  function for use by DPath expressions.
   */
  def optRepType: Option[SimpleTypeBase]

  /**
   * optRepTypeDef - there is no "def" for a primitive type.
   */
  final lazy val optRepTypeDef: Option[SimpleTypeDefBase] = optRepType match {
    case Some(x: SimpleTypeDefBase) => Some(x)
    case _ => None
  }
  def optRepValueSet: Option[RepValueSet]

  def optRepTypeElement: Option[RepTypeQuasiElementDecl]

}
