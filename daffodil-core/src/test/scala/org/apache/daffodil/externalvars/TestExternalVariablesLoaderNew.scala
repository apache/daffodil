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

import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.util._
import scala.xml._
import org.apache.daffodil.dsom.SchemaSet
import org.apache.daffodil.xml.NS
import org.apache.daffodil.Implicits._; object INoWarn1 { ImplicitsSuppressUnusedImportWarning() }

class TestExternalVariablesLoaderNew extends Logging {
  val xsd = XMLUtils.XSD_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE

  val dummyGroupRef = null // just because otherwise we have to construct too many things.

  def generateSD(topLevelAnnotations: Seq[Node] = <dfdl:format ref="tns:GeneralFormat"/>) = {
    lazy val sch = SchemaUtils.dfdlTestSchema(
      topLevelAnnotations,
      <xs:element name="fake" type="xs:string" dfdl:lengthKind="delimited"/>
      <xs:element name="fake2" type="tns:fakeCT"/>
      <xs:complexType name="fakeCT">
        <xs:sequence>
          <xs:group ref="tns:fakeGroup"/>
          <xs:element ref="tns:fake"/>
        </xs:sequence>
      </xs:complexType>
      <xs:group name="fakeGroup">
        <xs:choice>
          <xs:sequence/>
        </xs:choice>
      </xs:group>)
    lazy val xsd_sset = new SchemaSet(sch, "http://example.com", "fake")
    lazy val xsd_schema = xsd_sset.getSchema(NS("http://example.com")).get
    lazy val fakeSD = xsd_schema.schemaDocuments(0)
    (fakeSD, xsd_sset)
  }

  def FindValue(collection: Map[String, String], key: String, value: String): Boolean = {
    val found: Boolean = Option(collection.find(x => x._1 == key && x._2 == value)) match {
      case Some(_) => true
      case None => false
    }
    found
  }

}
