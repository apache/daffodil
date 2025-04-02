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

import org.apache.daffodil.lib.iapi.WarnID
import org.apache.daffodil.lib.schema.annotation.props.Found
import org.apache.daffodil.lib.schema.annotation.props.NotFound

trait EscapeSchemeRefMixin { self: Term =>

  /**
   * Changed to use findProperty, and to resolve the namespace properly.
   *
   * We lookup a property like escapeSchemeRef, and that actual property
   * binding can be local, in scope, by way of a format reference, etc.
   *
   * It's value is a QName, and the definition of the prefix is from the
   * location where we found the property, and NOT where we consume the property.
   *
   * Hence, we resolve w.r.t. the location that provided the property.
   *
   * The point of findProperty vs. getProperty is just that the former returns
   * both the value, and the object that contained it. That object is what
   * we resolve QNames with respect to.
   *
   * Note: Same is needed for properties that have expressions as their values.
   * E.g., consider "{ ../foo:bar/.. }". That foo prefix must be resolved relative
   * to the object where this property was written, not where it is evaluated. (JIRA
   * issue DFDL-77)
   */
  final lazy val optionEscapeScheme: Option[DFDLEscapeScheme] = {
    val er = self.findPropertyOption("escapeSchemeRef")
    er match {
      case _: NotFound => {
        SDW(
          WarnID.EscapeSchemeRefUndefined,
          "Property escapeSchemeRef was undefined. Please add escapeSchemeRef='' to your schema."
        )
        None
      }
      case Found("", _, _, _) => None // empty string means no escape scheme
      case Found(qName, loc, _, _) => {
        val qn = loc.resolveQName(qName) // loc is where we resolve the QName prefix.
        val defESFactory = schemaSet.getDefineEscapeScheme(qn)
        //
        // We have escape scheme factories because an escape schema can have
        // expressions (for escapeCharacter and escapeEscapeCharacter), and
        // those need to be compiled for each context where the escape scheme
        // is referenced, not just once.
        //
        defESFactory match {
          case None => SDE("Define Escape Scheme %s Not Found", qName)
          case Some(desf) => Some(desf.forComponent(this).escapeScheme)
        }
      }
    }
  }

}
