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
package org.apache.daffodil.lib.iapi

import java.io.File
import java.net.URI
import scala.xml.Elem
import scala.xml.Node

import org.apache.daffodil.lib.externalvars.Binding
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.DaffodilXMLLoader
import org.apache.daffodil.lib.xml.NS
import org.apache.daffodil.lib.xml.XMLUtils

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
    val extVarBindings = optBindingsNode.map { Binding.getBindings(_) }.getOrElse(Seq())
    val optTunablesXML =
      (xml \ "tunables").headOption /* had to add trim here to get rid of #PCDATA */
    val tunablesMap =
      optTunablesXML.map { DaffodilTunables.tunablesMap(_) }.getOrElse(Map.empty)
    new DaffodilConfig(extVarBindings, tunablesMap)
  }

  def fromSchemaSource(source: DaffodilSchemaSource) = {
    val loader = new DaffodilXMLLoader()
    try {
      var node = loader.load(source, None) // might not be daf:dfdlConfig, so don't validate.
      val rootElem = node.asInstanceOf[Elem]
      if (
        rootElem.label == "dfdlConfig" &&
        NS(rootElem.namespace) == XMLUtils.EXT_NS_APACHE
      ) {
        // it's a daf:dfdlConfig element, so reload with validation
        // which will cause it to throw validation errors
        node = loader.load(source, Some(XMLUtils.dafextURI))
      }
      fromXML(node)
    } catch {
      case e: org.xml.sax.SAXParseException =>
        throw DaffodilConfigException(s"Unable to load configuration: $e", e)
    }
  }

  def fromURI(uri: URI) = fromSchemaSource(
    URISchemaSource(Misc.uriToDiagnosticFile(uri), uri)
  )

  def fromFile(file: File) = fromSchemaSource(
    URISchemaSource(file, file.toURI)
  )

}

/**
 * Class representing the contents of a dfdlConfig XML node.
 * (Also a TDML defineConfig node)
 * @param externalVariableBindings
 * @param tunablesMap
 */
final class DaffodilConfig private (
  val externalVariableBindings: Seq[Binding],
  val tunablesMap: Map[String, String]
) {
  // no methods
}

/**
 * Thrown when the configuration file cannot be parsed.
 * @param message used in the Exception
 * @param cause the exception thrown while trying to parse
 */
final case class DaffodilConfigException(message: String, cause: Exception)
  extends Exception(message, cause)
