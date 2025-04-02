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

package org.apache.daffodil.core.dsom

import java.net.URI
import scala.xml.Node

import org.apache.daffodil.core.dsom.IIUtils._
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.DaffodilSchemaSource
import org.apache.daffodil.lib.iapi.URISchemaSource
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml._

/**
 * An import statement.
 *
 * The enclosingGoalNamespace argument is Some(noNamespace) for a topLevel schema file
 * that has no targetNamespace attribute.
 *
 * Now consider that we could be an import which is inside an included schema which includes another
 * included, etc. A nest of included schemas the innermost of which then contains an import.
 * We have to verify that the ultimate goal namespace at the start of that chain of includes
 * is different from this imported schema's goalNamespace.
 */
final class Import(importNode: Node, xsd: XMLSchemaDocument, seenArg: IIMap)
  extends IIBase(importNode, xsd, seenArg) {

  final lazy val mapPair = LV(Symbol("mapPair")) {
    val mpOpt = importElementNS.map { ieNS => (ieNS, resolvedLocation) }
    val mp = mpOpt.getOrElse {
      //
      // we didn't have a namespace property in the import component
      // So we have to load the file to find out what the namespace
      // is, and then we might decide to use it, or not use it.
      //
      // This means we need to unconditionally load the schemaDocument
      // and not do checks nor use the incoming set of "seenBefore"
      // schemas - as we need to open this schema file simply to see
      // its namespace.

      // FIXME: if you have an import like this:
      // <import schemaLocation="..."/>
      // This code will read the file TWICE. Once just to peek at the
      // namespace.
      //
      // This should be cached. i.e., cache the loaded schema file
      // object by way of a factory without the final map parameter
      // (similarly, for DFDLSchemaDocument, the Import/Exports etc.)
      //
      val sf = iiSchemaFile
      val xsd = sf.iiXMLSchemaDocument
      val xsdtns = xsd.targetNamespace
      (xsdtns, resolvedLocation)
    }
    mp
  }.value

  lazy val importElementNS = getAttributeOption("namespace").map { NS(_) }

  override lazy val targetNamespace: NS = LV(Symbol("targetNamespace")) {
    val tns = importElementNS match {
      case Some(ns) => ns // don't load it just to check compatibility.
      case None =>
        iiSchemaFile.iiSchemaDocument.targetNamespace // load it because we have to have it.
    }
    tns
  }.value

  /**
   * Only import has a namespace URI.
   *
   * This will be Some(URL) for reading an imported schema,
   * if we resolved the namespace URI via the XML Catalog.
   */
  lazy val resolvedNamespaceURI: Option[DaffodilSchemaSource] =
    LV(Symbol("resolvedNamespaceURI")) {
      importElementNS match {
        case None => {
          schemaDefinitionUnless(
            schemaLocationProperty != None,
            "When there is no namespace specified, there must be a schemaLocation specified."
          )
          None
        }
        case Some(ns) => {
          val uriString = resolver.resolveURI(ns.toString)
          if (uriString == null) None
          else {
            val uri = URI.create(uriString)
            val dfp = Misc.uriToDiagnosticFile(uri)
            val res =
              URISchemaSource(dfp, uri)
            Some(res)
          }
        }
      }
    }.value

  private lazy val resolver = xsd.schemaSet.resolver
  private lazy val catFiles = resolver.catalogFiles.mkString(", ")

  /**
   * XML Catalog is tried first, then classpath
   */
  lazy val resolvedLocation: DaffodilSchemaSource = LV(Symbol("resolvedLocation")) {

    Logger.log.debug(
      s"Computing resolvedLocation\nimportElementNS='${importElementNS}'\nresolvedNamespaceURI='${resolvedNamespaceURI}'\nschemaLocationProperty='${schemaLocationProperty}'\nresolvedSchemaLocation='${resolvedSchemaLocation}'"
    )

    val rl = (
      importElementNS,
      resolvedNamespaceURI,
      schemaLocationProperty,
      resolvedSchemaLocation
    ) match {
      case (None, _, Some(sl), None) => {
        if (xsd.isBootStrapSD) {
          //
          // special case - one of the user-supplied files (that we wrap in a
          // fake import statement and a fake surrounding document
          // doesn't exist.
          // We don't want the message to discuss those fake things.
          //
          schemaDefinitionError("No schema document at location %s.", sl)
        } else {
          schemaDefinitionError(
            "Unable to import a no-namespace schema from schema location %s. %s",
            importNode,
            whereSearched
          )
        }
      }
      case (Some(_), Some(rnURI), _, _) =>
        rnURI // found it in the catalog based on namespace attribute
      case (Some(ns), None, Some(sl), None) =>
        schemaDefinitionError(
          "Unable to import namespace %s from XML catalog(s) %s or schema location %s. %s",
          ns,
          catFiles,
          importNode,
          whereSearched
        )
      case (_, None, Some(sl), Some(rsl)) => rsl // found it by way of the schemaLocation
      case (Some(ns), None, None, None) => {
        schemaDefinitionError(
          "Unable to import namespace %s using XML Catalog(s) %s",
          ns,
          catFiles
        )
      }
      case _ => Assert.invariantFailed("illegal combination of namespace and schemaLocation")
    }
    rl
  }.value

}
