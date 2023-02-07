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

import org.apache.daffodil.api.XMLConversionControl.CarriageReturnMapping
import org.apache.daffodil.externalvars.Binding
import org.apache.daffodil.util.Misc
import org.apache.daffodil.xml.DaffodilXMLLoader
import org.apache.daffodil.xml.NS
import org.apache.daffodil.xml.XMLUtils

import java.io.File
import java.net.URI
import scala.xml.Elem
import scala.xml.Node

/**
 * Makes it simple to define enum corresponding to an XSD attribute declaration.
 *
 * TODO: Move to daffodil-lib xml package.
 */
trait AttrEnum extends Enumeration {
  type Type = Value

  final def fromXML(xml: Node): Value = {
    var rawtxt = (xml \ ("@" + attributeName)).text
    val opt: Option[Value] =
      if (rawtxt == "")
        None
      else
        Some(withName(rawtxt))
    opt.getOrElse(default)
  }

  def default: Value

  private lazy val nameFromClass =
    Misc.toInitialLowerCaseUnlessAllUpperCase(
      Misc.getNameFromClass(this)
    )

  /**
   * object class name must match the config file element name
   * except for starting with upper case letter.
   *
   * Or you can override.
   */
  def attributeName = nameFromClass

  /**
   * Appended to diagnostic messages. But these
   * should not happen if the XML Loading does validation.
   */
  protected def adviceString: String
}

/**
 * Makes it easy to construct structures corresponding
 * to sub-elements.
 */
trait SubElement extends Serializable {

  final def parseFromParentXML(parentXML: Node): Option[Node] = {
    var optNode = (parentXML \ subElementName).headOption
    optNode
  }

  private lazy val nameFromClass =
    Misc.toInitialLowerCaseUnlessAllUpperCase(
      Misc.getNameFromClass(this)
    )

  def subElementName: String = nameFromClass
}

/**
 * For use with Config files for defining
 * enums for XSD attributes.
 */
trait ConfigAttrEnum extends AttrEnum {
  override protected def adviceString = "Config files should be XSD validated before processing them."
}


object XMLConversionControl extends SubElement {
  override def subElementName = "xmlConversionControl"

  def apply(parentXML: Node) =
    new XMLConversionControl(parseFromParentXML(parentXML))
  def apply() =
    new XMLConversionControl(None)
  /**
   * TODO: for DAFFODIL-2234/DAFFODIL-2346 control of XMLTextEscapeStyle
   * just add another object like CarriageReturnMapping, and
   * create syntax for it in dafext.xsd as an attribute of
   * the xmlConversionControl element.
   */
  object CarriageReturnMapping extends ConfigAttrEnum {
    val ConvertCR2LF, PreserveCR = Value

    def default = ConvertCR2LF

    def apply(xml: Node) = fromXML(xml)
  }
}

class XMLConversionControl(xml: Node) extends Serializable {
  def this(optNode: Option[Node]) =
   this(optNode.getOrElse(<xmlConversionControl/>))

  val crm = CarriageReturnMapping(xml)
}


object DaffodilConfig {
  /**
   * Create from a dafext:dfdlConfig element, which is often in a file.
   * Can also create from a tdml:defineConfig element, since the children
   * are the same.
   *
   * @param xml
   * @return
   */
  def fromXML(xml: Node): DaffodilConfig = {
    val optBindingsNode = (xml \ "externalVariableBindings").headOption
    val extVarBindings = optBindingsNode.map{ Binding.getBindings(_) }.getOrElse(Seq())
    val optTunablesXML = (xml \ "tunables").headOption /* had to add trim here to get rid of #PCDATA */
    val tunablesMap = optTunablesXML.map{ DaffodilTunables.tunablesMap(_) }.getOrElse(Map.empty)

    // XCC's are done differently as they are not generated (currently)
    val xcc = XMLConversionControl(xml)
    new DaffodilConfig(extVarBindings, tunablesMap, xcc)
  }

  def fromSchemaSource(source: DaffodilSchemaSource): DaffodilConfig = {
    val loader = new DaffodilXMLLoader()
    // might not be daf:dfdlConfig, so don't validate.
    // configs embedded in TDML have a different root element
    // and are pre-validated as part of validating the TDML.
    var node = loader.load(source, None)
    val rootElem = node.asInstanceOf[Elem]
    if (rootElem.label == "dfdlConfig" &&
        NS(rootElem.namespace) == XMLUtils.EXT_NS_APACHE ) {
      // it's a daf:dfdlConfig element, so reload with validation
      // which will cause it to throw validation errors
      node = loader.load(source, Some(XMLUtils.dafextURI))
    }
    fromXML(node)
  }

  def fromURI(uri: URI): DaffodilConfig = fromSchemaSource(URISchemaSource(uri))

  def fromFile(file: File): DaffodilConfig = fromURI(file.toURI)

  def apply() = new DaffodilConfig(Seq(), Seq().toMap, XMLConversionControl())

}

/**
 * Class representing the contents of a dfdlConfig XML node.
 * (Also a TDML defineConfig node)
 * @param externalVariableBindings
 * @param tunablesMap
 */
final class DaffodilConfig private (
  val externalVariableBindings: Seq[Binding],
  val tunablesMap: Map[String, String],
  val xmlConversionControl: XMLConversionControl)
  extends Serializable {
  // no methods
}
