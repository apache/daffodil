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

package org.apache.daffodil.cli

import org.apache.daffodil.api.infoset.XMLTextEscapeStyle
import org.apache.daffodil.api.infoset.XMLTextLineEndingStyle

/**
 * One supported key: the infoset types it applies to, its allowed values for help
 * text, its default, and how to parse its value text (throwing an Exception with
 * a message if the text is invalid). Every InfosetOptions creates its own keys, so
 * keys with the same name are the same key.
 */
case class InfosetOptionKey[V](
  name: String,
  applicableInfosetTypes: Set[InfosetType.Type],
  valueDescription: String,
  defaultValue: V,
  parseValue: String => V
) {

  override def equals(that: Any): Boolean = that match {
    case key: InfosetOptionKey[?] => name == key.name
    case _ => false
  }

  override def hashCode: Int = name.hashCode
}

type AnyInfosetOptionKey = InfosetOptionKey[?]

/**
 * The options given on the command line, as each key mapped to its parsed value.
 */
type ParsedInfosetOptions = Map[AnyInfosetOptionKey, Any]

/**
 * Options for infoset inputters and outputters, given on the command line as a
 * list of key=value pairs. Each option is read from the parsed pairs once, when
 * this is created, so using an option is a field access. A key that was not given
 * has its default value.
 */
final case class InfosetOptions(parsed: ParsedInfosetOptions) {

  private val optionKeys = scala.collection.mutable.ArrayBuffer.empty[AnyInfosetOptionKey]

  val pretty: Boolean = value(
    InfosetOptionKey(
      name = "pretty",
      applicableInfosetTypes = Set(InfosetType.XML, InfosetType.JSON, InfosetType.SAX),
      valueDescription = "true|false",
      defaultValue = true,
      parseValue = _.toBoolean
    )
  )

  val exiCompression: Boolean = value(
    InfosetOptionKey(
      name = "exiCompression",
      applicableInfosetTypes = Set(InfosetType.EXI, InfosetType.EXISA),
      valueDescription = "true|false",
      defaultValue = false,
      parseValue = _.toBoolean
    )
  )

  val xmlTextEscapeStyle: XMLTextEscapeStyle = value(
    InfosetOptionKey(
      name = "xmlTextEscape",
      applicableInfosetTypes = Set(InfosetType.XML),
      valueDescription = XMLTextEscapeStyle.values.mkString("|"),
      defaultValue = XMLTextEscapeStyle.Standard,
      parseValue = text => InfosetOptions.enumValue(XMLTextEscapeStyle.values, text)
    )
  )

  val lineEndingStyle: XMLTextLineEndingStyle = value(
    InfosetOptionKey(
      name = "lineEndings",
      applicableInfosetTypes =
        Set(InfosetType.XML, InfosetType.SCALA_XML, InfosetType.JDOM, InfosetType.W3CDOM),
      valueDescription = XMLTextLineEndingStyle.values.mkString("|"),
      defaultValue = XMLTextLineEndingStyle.Standard,
      parseValue = text => InfosetOptions.enumValue(XMLTextLineEndingStyle.values, text)
    )
  )

  /**
   * The keys of the options above.
   */
  lazy val supportedKeys: Seq[AnyInfosetOptionKey] = optionKeys.toSeq

  private def value[V](key: InfosetOptionKey[V]): V = {
    optionKeys += key
    parsed.get(key).map(_.asInstanceOf[V]).getOrElse(key.defaultValue)
  }
}

object InfosetOptions {

  private def enumValue[E <: java.lang.Enum[E]](values: Array[E], text: String): E =
    values
      .find(_.toString.toLowerCase == text.toLowerCase)
      .getOrElse(throw new Exception("Must be one of " + values.mkString(", ")))

  val Default: InfosetOptions = InfosetOptions(Map.empty)

  /**
   * The supported keys.
   */
  val supportedKeys: Seq[AnyInfosetOptionKey] = Default.supportedKeys

  /**
   * Description of the supported keys, one per line, for help text.
   */
  val keysDescription: String =
    supportedKeys
      .map(k => "%s=%s (default: %s)".format(k.name, k.valueDescription, k.defaultValue))
      .mkString("\n")
}
