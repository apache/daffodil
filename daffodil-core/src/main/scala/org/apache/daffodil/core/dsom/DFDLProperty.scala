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

import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq

import org.apache.daffodil.lib.xml.NS
import org.apache.daffodil.lib.xml.NamedQName
import org.apache.daffodil.lib.xml.QName
import org.apache.daffodil.lib.xml.XMLUtils

final class DFDLProperty(xmlArg: Node, formatAnnotation: DFDLFormatAnnotation)
  extends DFDLAnnotation(xmlArg, formatAnnotation.annotatedSC)
  with LocalNonElementComponentMixin {

  /**
   * Variable use to differentiate between same properities that are provided
   * via differnt means (i.e. daf:property, name="dfdlx:...").
   */
  lazy val propertyNamespace = {
    if (super.name.startsWith("dfdlx:")) {
      XMLUtils.DFDLX_NAMESPACE
    } else {
      NS(xml.namespace)
    }
  }

  /**
   * Extension properties are provided in the dfdl:property element, but the
   * name field is prefixed with dfdlx:. Strip that off to get the actual
   * property name. The propertyNamespace above can be used to determine if
   * this property was defined as an extension or not.
   */
  override val name = super.name.stripPrefix("dfdlx:")

  override lazy val namedQName: NamedQName =
    QName.createGlobal(name, propertyNamespace, xml.scope)

  override lazy val path = formatAnnotation.path + "::" + diagnosticDebugName

  //  override lazy val schemaComponent: LookupLocation = formatAnnotation.annotatedSC
  //
  //  override lazy val schemaDocument = formatAnnotation.schemaDocument
  //  override lazy val uriString = xmlSchemaDocument.uriString

  // TODO: if we grab the value from here, then any qnames inside that value
  // have to be resolved by THIS Object
  lazy val value = {
    val values: Option[NodeSeq] = xml match {
      case Elem("dfdl", "property", _, _, valueNodes @ _*) if valueNodes.isEmpty => None
      case Elem("daf", "property", _, _, valueNodes @ _*) if valueNodes.isEmpty => None
      case Elem("dfdl", "property", _, _, valueNodes @ _*) => Some(valueNodes)
      case Elem("daf", "property", _, _, valueNodes @ _*) => Some(valueNodes)
    }

    values match {
      case None => ""
      case Some(valueNodes) => {
        //
        // We have to implement our own trim logic.
        // and that is somewhat subtle. E.g., textNumberPattern where
        // spaces are meaningful active characters. lengthPattern,
        // assert patterns, etc.
        //
        // Inside dfdl:property, since it is an element, XML's typical
        // whitespace fungibility applies. So use CDATA if you care
        // about space inside these.
        //
        val values = valueNodes.flatMap { valueNode =>
          valueNode match {
            case scala.xml.PCData(s) => Some(valueNode)
            case scala.xml.Text(s) => {
              if (s.matches("""\s+""")) {
                // all whitespace. Remove the node.
                None
              } else {
                val trimmed = s.trim
                if (trimmed.length == 0) None
                else Some(scala.xml.Text(trimmed))
              }
            }
            case scala.xml.Comment(_) => None
            case scala.xml.EntityRef(_) => Some(valueNode)
            case _: scala.xml.Atom[_] =>
              Some(valueNode) // &lt; comes through as this... should be EntityRef
          }
        }
        val res = values.map { _.text }.mkString
        res
      }
    }
  }

}
