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

class WarnIDGenerator(schema: scala.xml.Node) {

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
    |import org.apache.daffodil.lib.exceptions.Assert
    |import org.apache.daffodil.lib.exceptions.ThrowsSDE
    |import org.apache.daffodil.lib.schema.annotation.props.Enum
    |import org.apache.daffodil.lib.schema.annotation.props.EnumValue
    |
    |sealed trait WarnID extends EnumValue
    |object WarnID extends Enum[WarnID] {
    """.trim.stripMargin

  val bottom = """
    |  override def apply(name: String, context: ThrowsSDE) = Assert.usageError("not to be called. Use optionStringToEnum")
    |}
    """.trim.stripMargin

  val ssdwNode =
    (schema \ "simpleType").find(_ \@ "name" == "TunableSuppressSchemaDefinitionWarnings").get
  val enumerationNodes = (ssdwNode \\ "enumeration")

  def writeGeneratedCode(w: java.io.FileWriter): Unit = {
    w.write(top)
    w.write("\n")

    val scalaNames = enumerationNodes.map { node =>
      val enumName = node \@ "value"
      val scalaName = enumName.head.toUpper +: enumName.tail
      scalaName
    }

    scalaNames.foreach { scalaName =>
      w.write(s"  case object ${scalaName} extends WarnID\n")
    }

    w.write(scalaNames.mkString("  override lazy val values = Array(", ", ", ")\n"))

    w.write("\n")
    w.write(bottom)
    w.flush();
  }
}
