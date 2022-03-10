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
package org.apache.daffodil.api

import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.xml.DaffodilXMLLoader
import org.apache.daffodil.xml.NS
import org.apache.daffodil.xml.XMLUtils

import java.io.File
import java.net.URI
import scala.xml.Elem
import scala.xml.Node

object DaffodilConfig {

  /**
   * Create from a dafext:dfdlConfig element, which is often in a file.
   * Can also create from a tdml:defineConfig element, since the children
   * are the same.
   *
   * @param xml
   * @return
   */
  def fromXML(xml: Node) = {
    val optBindingsNode = (xml \ "externalVariableBindings").headOption
    val extVarBindings = optBindingsNode.map{ Binding.getBindings(_) }.getOrElse(Seq())
    val optTunablesXML = (xml \ "tunables").headOption /* had to add trim here to get rid of #PCDATA */
    val tunablesMap = optTunablesXML.map{ DaffodilTunables.tunablesMap(_) }.getOrElse(Map.empty)
    new DaffodilConfig(extVarBindings, tunablesMap)
  }

  def fromSchemaSource(source: DaffodilSchemaSource) = {
    val loader = new DaffodilXMLLoader()
    var node = loader.load(source, None) // might not be daf:dfdlConfig, so don't validate.
    val rootElem = node.asInstanceOf[Elem]
    if (rootElem.label == "dfdlConfig" &&
        NS(rootElem.namespace) == XMLUtils.EXT_NS_APACHE ) {
      // it's a daf:dfdlConfig element, so reload with validation
      // which will cause it to throw validation errors
      node = loader.load(source, Some(XMLUtils.dafextURI))
    }
    fromXML(node)
  }

  def fromURI(uri: URI) = fromSchemaSource(URISchemaSource(uri))

  def fromFile(file: File) = fromURI(file.toURI)

}

/**
 * Class representing the contents of a dfdlConfig XML node.
 * (Also a TDML defineConfig node)
 * @param externalVariableBindings
 * @param tunablesMap
 */
final class DaffodilConfig private (
  val externalVariableBindings: Seq[Binding],
  val tunablesMap: Map[String, String]) {
  // no methods
}
