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
    |package org.apache.daffodil.api
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
    |import org.apache.daffodil.exceptions.Assert
    |import org.apache.daffodil.exceptions.ThrowsSDE
    |import org.apache.daffodil.schema.annotation.props.EmptyElementParsePolicy
    |import org.apache.daffodil.schema.annotation.props.Enum
    |import org.apache.daffodil.util.Logging
    |import org.apache.daffodil.util.Misc
    |import org.apache.daffodil.xml.DaffodilXMLLoader
    |
    |object DaffodilTunables {
    |
    |  def apply(tunables: Map[String, String]): DaffodilTunables = {
    |    apply().setTunables(tunables)
    |  }
    |
    |  def apply(tunable: String, value: String): DaffodilTunables = {
    |    apply().setTunable(tunable, value)
    |  }
    |
    |  def apply(): DaffodilTunables = {
    |    // override tunables from the global configuration file on the class path, if it exists
    |    val (configOpt, _) = Misc.getResourceOption("/daffodil-config.xml")
    |    val configTunables: Map[String, String] =
    |      if (configOpt.isDefined) {
    |        val loader = new DaffodilXMLLoader()
    |        val node = loader.load(URISchemaSource(configOpt.get))
    |        val trimmed = scala.xml.Utility.trim(node)
    |        val tunablesNode = (trimmed \ "tunables").headOption
    |        val tunablesMap: Map[String, String] = tunablesNode match {
    |          case None => Map.empty
    |          case Some(tunableNode) => {
    |            tunableNode.child.map { n => (n.label, n.text) }.toMap
    |          }
    |        }
    |        tunablesMap
    |      } else {
    |        Map.empty
    |      }
    |
    |    new DaffodilTunables().setTunables(configTunables)
    |  }
    |}
    |
    |case class DaffodilTunables private (
    """.trim.stripMargin

  val middle = """
    |  extends Serializable
    |  with Logging {
    |
    |  def setTunables(tunables: Map[String, String]): DaffodilTunables = {
    |    tunables.foldLeft(this) { case (dafTuns, (tunable, value)) => dafTuns.setTunable(tunable, value) }
    |  }
    |
    |  def setTunable(tunable: String, value: String): DaffodilTunables = {
    |    tunable match {
    """.trim.stripMargin

  val bottom = """
    |      case _ => throw new IllegalArgumentException("Unknown tunable: " + tunable)
    |    }
    |  }
    |
    |  def notSuppressedWarning(warnID: WarnID) =
    |    !suppressSchemaDefinitionWarnings.contains(warnID) &&
    |      !suppressSchemaDefinitionWarnings.contains(WarnID.All)
    |
    |}
    """.trim.stripMargin

  val tunablesRoot = (schemaRootConfig \ "element").find(_ \@ "name" == "tunables").get
  val tunableNodes = tunablesRoot \\ "all" \ "element"

  val excludedSimpleTypes = Seq(
    "TunableEmptyElementParsePolicy",
    "TunableSuppressSchemaDefinitionWarnings")
  val tunableSimpleTypeNodes = (schemaRootConfig \ "simpleType")
    .filter { st => (st \@ "name").startsWith("Tunable") }
    .filter { st => !excludedSimpleTypes.contains(st \@ "name") }

  def writeGeneratedCode(w: java.io.FileWriter): Unit = {
    val tunables =
      tunableNodes.map { tunableNode =>
        val schemaName = tunableNode \@ "name"
        val schemaType = tunableNode \@ "type"
        val schemaDefault = tunableNode \@ "default"

        val tunable =
          if (schemaName == "suppressSchemaDefinitionWarnings") {
            // special case, list of enums
            new EnumListTunable(schemaName, schemaType, schemaDefault, "WarnID")
          } else if (!schemaType.startsWith("xs:")) {
            // non-primitive type, assume a single enum
            new EnumTunable(schemaName, schemaType, schemaDefault)
          } else if (schemaName == "tempFilePath") {
            // special case, creates actual file object instead of string
            new TempFilePathTunable()
          } else {
            // primitive type
            new PrimitiveTunable(schemaName, schemaType, schemaDefault)
          }
        tunable
      }

    w.write(top)
    w.write("\n")
    w.write(tunables.map(_.scalaDefinition).mkString("  ", ",\n  ", ")"))
    w.write("\n")
    w.write(middle)
    w.write("\n")
    w.write(tunables.map(_.scalaConversion.split("\n").mkString("      ", "\n      ", "")).mkString("\n"))
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
}

class PrimitiveTunable(name: String, schemaType: String, schemaDefault: String)
  extends TunableBase {

  private val scalaType = schemaType match {
    case "xs:boolean" => "Boolean"
    case "xs:int" => "Int"
    case "xs:long" => "Long"
    case "xs:string" => "String"
  }

  private val scalaDefault = schemaType match {
    case "xs:string" => "\"" + schemaDefault + "\""
    case _ => schemaDefault
  }

  override val scalaDefinition = s"""val ${name}: ${scalaType} = ${scalaDefault}"""
  override val scalaConversion = s"""case "${name}" => this.copy(${name} = value.to${scalaType})"""
}

class TempFilePathTunable() extends TunableBase {
  override val scalaDefinition = s"""val tempFilePath: java.io.File = new java.io.File(System.getProperty(\"java.io.tmpdir\"))"""
  override val scalaConversion = s"""case "tempFilePath" => this.copy(tempFilePath = new java.io.File(value))"""
}

class EnumTunable(name: String, schemaType: String, schemaDefault: String)
  extends TunableBase {

  private val scalaType = schemaType.stripPrefix("daf:Tunable")
  private val scalaDefault = scalaType + "." + schemaDefault.head.toUpper + schemaDefault.tail

  override val scalaDefinition = s"""val ${name}: ${scalaType} = ${scalaDefault}"""
  override val scalaConversion = s"""
    |case "${name}" => {
    |  val vOpt = ${scalaType}.optionStringToEnum("${scalaType}", value)
    |  val v = vOpt.getOrElse(throw new IllegalArgumentException("For input string: \\"" + value + "\\""))
    |  this.copy(${name} = v)
    |}
    """.trim.stripMargin
}

class EnumListTunable(name: String, schemaType: String, schemaDefault: String, listType: String)
  extends TunableBase {

  val scalaDefault = {
    val trimmedDefault = schemaDefault.trim
    if (trimmedDefault == "") {
      "Nil"
    } else {
      val defaultSeq = trimmedDefault.split("\\s+").map(d => s"${listType}.${d.head.toUpper + d.tail}")
      s"""Seq(${defaultSeq.mkString(", ")})"""
    }

  }

  override val scalaDefinition = s"val ${name}: Seq[${listType}] = ${scalaDefault}"
  override val scalaConversion = s"""
    |case "${name}" => {
    |  val values = value.split("\\\\s+").toSeq.map { v =>
    |    val vOpt = ${listType}.optionStringToEnum("${listType}", v)
    |    vOpt.getOrElse(throw new IllegalArgumentException("For input string: \\"" + v + "\\""))
    |  }
    |  this.copy(${name} = values)
    |}
    """.trim.stripMargin
}

class TunableEnumDefinition(schemaRootConfig: scala.xml.Node, schemaRootExt: scala.xml.Node, simpleTypeNode: scala.xml.Node) {
  private val nodeName = (simpleTypeNode \@ "name").stripPrefix("Tunable")
  private val scalaType = nodeName.head.toUpper + nodeName.tail

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
    |sealed trait ${scalaType} extends ${scalaType}.Value
    |object ${scalaType} extends Enum[${scalaType}] {
""".trim.stripMargin

  private val scalaEnums = {
    val scalaEnumValues = allEnumerationValues.map { e => e.head.toUpper + e.tail }
    scalaEnumValues.map { e => s"""  case object ${e} extends ${scalaType}; forceConstruction(${e})""" }
  }

  private val bottom = s"""
    |  override def apply(name: String, context: ThrowsSDE) = Assert.usageError("not to be called. Use optionStringToEnum")
    |}
""".stripMargin

  val scalaEnumeration = {
    top + "\n" + scalaEnums.mkString("\n") + "\n" + bottom
  }

}
