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
import org.apache.daffodil.lib.util.Misc

/**
 * Options for infoset inputters and outputters, given on the command line as a
 * comma separated list of key=value pairs.
 */
final case class InfosetOptions(
  pretty: Boolean = true,
  xmlTextEscapeStyle: XMLTextEscapeStyle = XMLTextEscapeStyle.Standard,
  exiCompression: Boolean = false
)

object InfosetOptions {

  val Default: InfosetOptions = InfosetOptions()

  /**
   * One supported key: the infoset types it applies to, its allowed values and
   * description for help text, how to get its default from the default options,
   * and how it updates the options from the value text.
   */
  private case class InfosetOptionKey(
    name: String,
    applicableInfosetTypes: Set[InfosetType.Type],
    valueDescription: String,
    description: String,
    defaultValue: InfosetOptions => Any,
    update: (InfosetOptions, String) => Either[String, InfosetOptions]
  )

  private def enumValue[E <: java.lang.Enum[E]](
    values: Array[E],
    text: String
  ): Either[String, E] =
    values
      .find(_.toString.equalsIgnoreCase(text))
      .toRight("Must be one of " + values.mkString(", "))

  private def booleanValue(text: String): Either[String, Boolean] =
    text.toLowerCase match {
      case "true" => Right(true)
      case "false" => Right(false)
      case _ => Left("Must be true or false")
    }

  private val keys: Seq[InfosetOptionKey] = Seq(
    InfosetOptionKey(
      name = "pretty",
      applicableInfosetTypes = Set(InfosetType.XML, InfosetType.JSON, InfosetType.SAX),
      valueDescription = "true|false",
      description = "Indent the infoset and add newlines where the content is not affected.",
      defaultValue = _.pretty,
      update = (o, v) => booleanValue(v).map(b => o.copy(pretty = b))
    ),
    InfosetOptionKey(
      name = "exiCompression",
      applicableInfosetTypes = Set(InfosetType.EXI, InfosetType.EXISA),
      valueDescription = "true|false",
      description =
        "Use EXI compression. The same setting must be used when unparsing an EXI infoset.",
      defaultValue = _.exiCompression,
      update = (o, v) => booleanValue(v).map(b => o.copy(exiCompression = b))
    ),
    InfosetOptionKey(
      name = "xmlTextEscape",
      applicableInfosetTypes = Set(InfosetType.XML),
      valueDescription = XMLTextEscapeStyle.values.mkString("|"),
      description = "Standard escapes special characters in xs:string values. CDATA instead " +
        "wraps xs:string values that contain special characters or whitespace in CDATA sections.",
      defaultValue = _.xmlTextEscapeStyle,
      update = (o, v) =>
        enumValue(XMLTextEscapeStyle.values, v).map(s => o.copy(xmlTextEscapeStyle = s))
    )
  )

  /**
   * The names of the supported keys.
   */
  val keyNames: Seq[String] = keys.map(_.name)

  /**
   * Description of the supported keys, one per paragraph, for help text.
   */
  val keysDescription: String =
    keys
      .map { k =>
        "%s=%s\n%s Applies to: %s infosets. Default: %s.".format(
          k.name,
          k.valueDescription,
          k.description,
          k.applicableInfosetTypes.toSeq.sorted.mkString(", "),
          k.defaultValue(Default)
        )
      }
      .mkString("\n")

  /**
   * Parses a comma separated list of key=value pairs for the given infoset type.
   * Later occurrences of a key replace earlier ones.
   */
  def parse(text: String, infosetType: InfosetType.Type): Either[String, InfosetOptions] =
    Misc.parseKeyValuePairs(text, ",") match {
      case Left(badItem) =>
        Left("Invalid infoset option '%s'. Expected key=value".format(badItem))
      case Right(pairs) =>
        pairs.foldLeft[Either[String, InfosetOptions]](Right(Default)) {
          case (result, (name, value)) =>
            result.flatMap { options =>
              keys.find(_.name == name) match {
                case None =>
                  Left(
                    "Unrecognized infoset option '%s'. Must be one of %s"
                      .format(name, keys.map(_.name).mkString(", "))
                  )
                case Some(key) if !key.applicableInfosetTypes.contains(infosetType) =>
                  Left(
                    "Infoset option '%s' does not apply to the %s infoset type"
                      .format(name, infosetType)
                  )
                case Some(key) =>
                  key
                    .update(options, value)
                    .left
                    .map(msg =>
                      "Invalid value '%s' for infoset option '%s'. %s".format(value, name, msg)
                    )
              }
            }
        }
    }

  /**
   * For arguments that the command line parser has already validated with parse.
   */
  def parseValidated(text: String, infosetType: InfosetType.Type): InfosetOptions =
    parse(text, infosetType).fold(msg => throw new IllegalStateException(msg), identity)
}
