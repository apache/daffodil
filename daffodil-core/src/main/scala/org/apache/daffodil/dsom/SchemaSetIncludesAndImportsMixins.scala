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

package org.apache.daffodil.dsom

import org.apache.daffodil.dsom.IIUtils._
import org.apache.daffodil.xml.XMLUtils
import org.apache.daffodil.api.WarnID

/**
 * Mixin for SchemaSet
 */

trait SchemaSetIncludesAndImportsMixin { self: SchemaSet =>

  /**
   * Let's take the list of file names given, and make a fake schema
   * document with import statements for them. Then the algorithms
   * are all isolated to just the SchemaDocument class and the Include
   * and Import classes.
   */
  lazy val fakeXMLSchemaDocument = {
    val xsd = XMLUtils.XSD_NAMESPACE.toString

    // Any time we synthesize xml we have to grab the namespace definitions and
    // make sure we drag them along onto the new structures.
    val fakeImportStatementsXML =
      <xs:import schemaLocation={ schemaSource.uriForLoading.toString } xmlns:xs={ xsd }/>

    val fakeSchemaDocXML =
      <xs:schema xmlns:xs={ xsd }>{ fakeImportStatementsXML }</xs:schema>

    val initialEmptyIIMap: IIMap = IIUtils.emptyIIMap

    val fakeSD = XMLSchemaDocument(fakeSchemaDocXML, self, None, None, initialEmptyIIMap, isBootStrapSD = true)
    fakeSD
  }

  lazy val allSchemaDocuments = {
    allSchemaFiles.map { _.iiSchemaDocument }
  }

  lazy val allSchemaFiles = {
    val fd = fakeXMLSchemaDocument //bootstrap
    val sa = fd.seenAfter
    val sfl = sa.value.flatMap {
      case (_, ii) => {
        val sf = ii.iiSchemaFileMaybe // maybe not if we've already seen this file for the same namespace.
        sf.filter { f =>
          if (f.isDFDLSchemaFile)
            true
          else {
            f.SDW(WarnID.IgnoreImport, "Non-DFDL Schema file ignored. Does not have DFDL namespace definition on schema root element.\n" +
              "Add xmlns:dfdl='%s' to the root element if this file must be part of the DFDL schema.", XMLUtils.DFDL_NAMESPACE)
            false
          }
        }
      }
    }.toList
    sfl
  }

}
