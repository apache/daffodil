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

package org.apache.daffodil.externalvars

import scala.xml.Node
import org.apache.daffodil.xml._
import org.apache.daffodil.api.DaffodilTunables

class Binding(val varQName: RefQName, val varValue: String, scope: scala.xml.NamespaceBinding = null) {

  override def toString() = {
    "<binding name='" + varQName + "'>" + varValue + "</binding>"
  }

  def hasNamespaceSpecified: Boolean = !varQName.namespace.isUnspecified

  override def hashCode = varQName.hashCode

  override def equals(o: Any): Boolean = {
    o match {
      case that: Binding => this.varQName == that.varQName
      case _ => false
    }
  }

  def globalQName =
    if (scope ne null)
      QName.createGlobal(varQName.local, varQName.namespace, scope)
    else
      GlobalQName(varQName.prefix, varQName.local, varQName.namespace)
}

/**
 * This object is for cases when external variable bindings
 * are passed in via the Command Line Interface.
 */
object Binding {

  /**
   * extSyntax is {uri}ncName, or {}ncName, or ncName
   */
  def apply(extSyntax: String, value: String): Binding = {
    val tryRefQName = QName.refQNameFromExtendedSyntax(extSyntax)
    new Binding(tryRefQName.get, value, null)
  }

  def apply(node: Node, tunable: DaffodilTunables): Binding = {
    val name = (node \ "@name").head.text
    val refQName = QName.resolveRef(name, node.scope, tunable)
    val value = node.text
    new Binding(refQName.get, value, node.scope)
  }

  def apply(name: String, namespace: Option[NS], value: String): Binding = {
    new Binding(RefQName(None, name, namespace.getOrElse(UnspecifiedNamespace)), value)
  }

}
