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

package org.apache.daffodil.propGen

class TunableGenerator(schemaRootConfig: scala.xml.Node, schemaRootExt: scala.xml.Node) {

  val top = """
    |/*
    | * Licensed to the Apache Software Foundation (ASF) under one or more
    | * contributor license agreements.  See the NOTICE file distributed with
    | * this work for additional information regarding copyright ownership.
    | * The ASF licenses this file to You under the Apache License, Version 2.0
    | * (the "License"); you may not use this file except in compliance with
    | * the License.  You may obtain a copy of the License at
    | *
    | *     http://www.apache.org/licenses/LICENSE-2.0
    | *
    | * Unless required by applicable law or agreed to in writing, software
    | * distributed under the License is distributed on an "AS IS" BASIS,
    | * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    | * See the License for the specific language governing permissions and
    | * limitations under the License.
    | */
    |
    |package org.apache.daffodil.lib.iapi
    |
    |////////////////////////////////////////////////////////////////////////////////////////////
    |//
    |// Generated Code - Do not hand modify!
    |//
    |// This file is entirely generated code created from the
    |// XML Schema files that describe Daffodil configuration files.
    |//
    |// Don't edit this. Go fix the generator to create what you need instead.
    |//
    |////////////////////////////////////////////////////////////////////////////////////////////
    |
    |import java.nio.file.Paths
    |
    |import org.apache.daffodil.lib.exceptions.Assert
    |import org.apache.daffodil.lib.exceptions.ThrowsSDE
    |import org.apache.daffodil.lib.schema.annotation.props.EmptyElementParsePolicy
    |import org.apache.daffodil.lib.schema.annotation.props.Enum
    |import org.apache.daffodil.lib.schema.annotation.props.EnumValue
    |import org.apache.daffodil.lib.util.Misc
    |import org.apache.daffodil.lib.xml.DaffodilXMLLoader
    |import org.apache.daffodil.lib.xml.XMLUtils
    |
    |object DaffodilTunables extends DaffodilTunablesStaticMixin {
    |
    |  def apply(tunables: Map[String, String]): DaffodilTunables = {
    |    apply().withTunables(tunables)
    |  }
    |
    |  def apply(tunable: String, value: String): DaffodilTunables = {
    |    apply().withTunable(tunable, value)
    |  }
    |
    |  def apply(): DaffodilTunables = {
    |    // override tunables from the global configuration file on the class path, if it exists
    |    val configPath = "/daffodil-config.xml"
    |    val (configOpt, _) = Misc.getResourceOption(configPath)
    |    val configTunables: Map[String, String] =
    |      if (configOpt.isDefined) {
    |        val loader = new DaffodilXMLLoader()
    |        val node = loader.load(URISchemaSource(Paths.get(configPath).toFile, configOpt.get), Some(XMLUtils.dafextURI))
    |        tunablesMap(node)
    |      } else {
    |        Map.empty
    |      }
    |
    |    new DaffodilTunables().withTunables(configTunables)
    |  }
    |}
    |
    |case class DaffodilTunables private (
    """.trim.stripMargin

  val middle = """
    |  extends Serializable {
    |
    |  def withTunables(tunables: Map[String, String]): DaffodilTunables = {
    |    tunables.foldLeft(this) { case (dafTuns, (tunable, value)) => dafTuns.withTunable(tunable, value) }
    |  }
    |
    |  def withTunable(tunable: String, value: String): DaffodilTunables = {
    |    tunable match {
    """.trim.stripMargin

  val bottom = """
    |      case _ => throw new IllegalArgumentException("Unknown tunable: " + tunable)
    |    }
    |  }
    |
    |  private def throwInvalidTunableValue(tunable: String, value: String) = {
    |    throw new IllegalArgumentException("Invalid value for tunable " + tunable + ": " + value)
    |  }
    |
    |}
    """.trim.stripMargin

  val tunablesRoot = (schemaRootConfig \ "element").find(_ \@ "name" == "tunables").get
  val tunableNodes = tunablesRoot \\ "all" \ "element"

  val excludedSimpleTypes =
    Seq("TunableEmptyElementParsePolicy", "TunableSuppressSchemaDefinitionWarnings")
  val tunableSimpleTypeNodes = (schemaRootConfig \ "simpleType")
    .filter { st => (st \@ "name").startsWith("Tunable") }
    .filter { st => !excludedSimpleTypes.contains(st \@ "name") }

  def writeGeneratedCode(w: java.io.FileWriter): Unit = {
    val tunables =
      tunableNodes
        .map { tunableNode =>
          val schemaName = tunableNode \@ "name"
          val schemaType =
            if (tunableNode \@ "type" != "") tunableNode \@ "type"
            else tunableNode \\ "restriction" \@ "base"
          val schemaDefault = tunableNode \@ "default"

          if (schemaName == "")
            throw new Exception("Tunable missing mandatory name attribute: " + tunableNode)
          if (schemaType == "")
            throw new Exception(
              "Tunable missing mandatory type or restriction base attribute: " + schemaName
            )
          if (schemaDefault == "")
            throw new Exception("Tunable missing mandatory default attribute: " + schemaName)

          val tunable =
            if (schemaName == "suppressSchemaDefinitionWarnings") {
              // special case, list of enums
              new EnumListTunable(schemaName, schemaType, schemaDefault, "WarnID")
            } else if (!schemaType.startsWith("xs:")) {
              // non-primitive type, assume a single enum
              new EnumTunable(schemaName, schemaType, schemaDefault)
            } else if (schemaName == "tempFilePath") {
              // special case, creates actual file object instead of string
              new FileTunable("tempFilePath", """System.getProperty("java.io.tmpdir")""")
            } else {
              // primitive type
              new PrimitiveTunable(schemaName, schemaType, schemaDefault, tunableNode)
            }
          tunable
        }
        .sortBy(_.name)

    val definitionString =
      tunables
        .map(_.scalaDefinition)
        .mkString("  ", ",\n  ", ")")

    val conversionString =
      tunables
        .map { tunable =>
          tunable.scalaConversion
            .split("\n")
            .filter(_.trim.length > 0)
            .mkString("      ", "\n      ", "")
        }
        .mkString("\n")

    w.write(top)
    w.write("\n")
    w.write(definitionString)
    w.write("\n")
    w.write(middle)
    w.write("\n")
    w.write(conversionString)
    w.write("\n")
    w.write(bottom)
    w.write("\n")
    w.write("\n")

    val tunableDefinitions =
      tunableSimpleTypeNodes.map { n =>
        new TunableEnumDefinition(schemaRootConfig, schemaRootExt, n)
      }

    w.write(tunableDefinitions.map(_.scalaEnumeration).mkString("\n"))

    w.flush();
  }
}

abstract class TunableBase {
  def scalaDefinition: String
  def scalaConversion: String
  def name: String
}

class PrimitiveTunable(
  override val name: String,
  schemaType: String,
  schemaDefault: String,
  node: scala.xml.Node
) extends TunableBase {

  private val scalaType = schemaType match {
    case "xs:boolean" => "Boolean"
    case "xs:int" => "Int"
    case "xs:long" => "Long"
    case "xs:string" => "String"
    case _ => throw new Exception("Type not supported for tunable: " + schemaType)
  }

  private val scalaDefault = schemaType match {
    case "xs:string" => "\"" + schemaDefault + "\""
    case _ => schemaDefault
  }

  private def restrictionCheck(rCheck: String, rValue: String): Option[String] = {
    if (rValue != "") {
      Some(s"""  if (!(v ${rCheck} ${rValue})) throwInvalidTunableValue(tunable, value)""")
    } else {
      None
    }
  }

  private val minInclusive = node \\ "minInclusive" \@ "value"
  private val maxInclusive = node \\ "maxInclusive" \@ "value"
  private val minExclusive = node \\ "minExclusive" \@ "value"
  private val maxExclusive = node \\ "maxExclusive" \@ "value"

  private val restrictionChecks = Seq(
    restrictionCheck(">=", minInclusive),
    restrictionCheck("<=", maxInclusive),
    restrictionCheck(">", minExclusive),
    restrictionCheck("<", maxExclusive)
  ).flatten.mkString("\n")

  override val scalaDefinition = s"""val ${name}: ${scalaType} = ${scalaDefault}"""
  override val scalaConversion = s"""
    |case "${name}" => {
    |  val v = scala.util.Try(value.to${scalaType}).getOrElse { throwInvalidTunableValue(tunable, value) }
    |${restrictionChecks}
    |  this.copy(${name} = v)
    |}
    """.trim.stripMargin
}

class FileTunable(override val name: String, default: String) extends TunableBase {

  override val scalaDefinition = s"""val ${name}: java.io.File = new java.io.File(${default})"""
  override val scalaConversion = s"""
    |case "${name}" => {
    |  this.copy(${name} = new java.io.File(value))
    |}
    """.trim.stripMargin
}

class EnumTunable(override val name: String, schemaType: String, schemaDefault: String)
  extends TunableBase {

  private val scalaType = schemaType.stripPrefix("daf:Tunable")
  private val scalaDefault = scalaType + "." + schemaDefault.head.toUpper + schemaDefault.tail

  override val scalaDefinition = s"""val ${name}: ${scalaType} = ${scalaDefault}"""
  override val scalaConversion = s"""
    |case "${name}" => {
    |  val vOpt = ${scalaType}.optionStringToEnum("${scalaType}", value)
    |  val v = vOpt.getOrElse { throwInvalidTunableValue(tunable, value) }
    |  this.copy(${name} = v)
    |}
    """.trim.stripMargin
}

class EnumListTunable(
  override val name: String,
  schemaType: String,
  schemaDefault: String,
  listType: String
) extends TunableBase {

  val scalaDefault = {
    val trimmedDefault = schemaDefault.trim
    if (trimmedDefault == "") {
      "Nil"
    } else {
      val defaultSeq =
        trimmedDefault.split("\\s+").map(d => s"${listType}.${d.head.toUpper}${d.tail}")
      s"""Seq(${defaultSeq.mkString(", ")})"""
    }

  }

  override val scalaDefinition = s"val ${name}: Seq[${listType}] = ${scalaDefault}"
  override val scalaConversion = s"""
    |case "${name}" => {
    |  val values = value.split("\\\\s+").toSeq.map { v =>
    |    val vOpt = ${listType}.optionStringToEnum("${listType}", v)
    |    vOpt.getOrElse { throwInvalidTunableValue(tunable, value) }
    |  }
    |  this.copy(${name} = values)
    |}
    """.trim.stripMargin
}

class TunableEnumDefinition(
  schemaRootConfig: scala.xml.Node,
  schemaRootExt: scala.xml.Node,
  simpleTypeNode: scala.xml.Node
) {
  private val nodeName = (simpleTypeNode \@ "name").stripPrefix("Tunable")
  private val scalaType = nodeName.head.toUpper +: nodeName.tail

  /**
   * Returns a list of all string values of enumerations. If a simpletype is a
   * untion of other simple types, it recursively gets their enumeration values
   * and combines everything.
   */
  private def getAllEnumerationValues(node: scala.xml.Node): Seq[String] = {
    val restrictions = node \\ "restriction"
    restrictions.flatMap { r =>
      val base = r \@ "base"
      val enumerationValues =
        if (base.startsWith("xs:")) {
          (r \ "enumeration").map(_ \@ "value")
        } else {
          val Array(pre, local) = base.split(":")
          val schemaToSearch = pre match {
            case "daf" => schemaRootConfig
            case "dfdlx" => schemaRootExt
          }
          val restrictionSimpleTypeNode =
            (schemaToSearch \ "simpleType").find(_ \@ "name" == base.split(":")(1)).get
          getAllEnumerationValues(restrictionSimpleTypeNode)
        }
      enumerationValues
    }
  }

  private val allEnumerationValues = getAllEnumerationValues(simpleTypeNode)

  private val top = s"""
    |sealed trait ${scalaType} extends EnumValue
    |object ${scalaType} extends Enum[${scalaType}] {
""".trim.stripMargin

  private val scalaEnums = {
    val scalaEnumValues = allEnumerationValues.map { e => e.head.toUpper +: e.tail }
    scalaEnumValues.map { e => s"""  case object ${e} extends ${scalaType}""" }
  }

  private val values = {
    val scalaEnumValues = allEnumerationValues.map { e => e.head.toUpper +: e.tail }
    scalaEnumValues.mkString("  override lazy val values = Array(", ", ", ")")
  }

  private val bottom = s"""
    |  override def apply(name: String, context: ThrowsSDE) = Assert.usageError("not to be called. Use optionStringToEnum")
    |}
""".stripMargin

  val scalaEnumeration = {
    top + "\n" + scalaEnums.mkString("\n") + "\n" + values + "\n" + bottom
  }

}
