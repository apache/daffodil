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

import scala.xml.Node

import org.apache.daffodil.core.dsom.IIUtils._
import org.apache.daffodil.lib.oolag.OOLAG
import org.apache.daffodil.lib.util.Logger

/**
 * enclosingGoalNS is None if this include
 * is being included (by one include hop, or several) into a schema having
 * 'no namespace'
 *
 * enclosingGoalNS is Some(str) if this include
 * is being included (by one include hop, or several) into a schema having
 * a targetNamespace.
 */
final class Include(xml: Node, xsd: XMLSchemaDocument, seenArg: IIMap)
  extends IIBase(xml, xsd, seenArg) {

  protected final lazy val mapPair = LV(Symbol("mapPair")) {
    // for an include, the targetNamespace of the schema document that contained us is right.
    val mp = (targetNamespace, resolvedLocation)
    mp
  }.value

  private lazy val slText =
    schemaLocationProperty.get // include always has a schemaLocation property

  lazy val resolvedNamespaceURI = None // include doesn't have a namespace.

  // include always has a schemaLocation
  lazy val resolvedLocation = LV(Symbol("resolvedLocation")) {
    resolvedSchemaLocation match {
      case Some(rsl) => {
        //
        // We use keepGoing here so we can capture diagnostics, but then
        // issue a final summary diagnostic about the target namespace.
        //
        val ns =
          OOLAG.keepGoing(schemaDefinitionError("Unable to determine target namespace.")) {
            xsd.targetNamespace
          }
        Logger.log.debug(s"Included schema from ${rsl} into namespace ${ns}.")
        rsl
      }
      case None =>
        schemaDefinitionError(
          "Included schema not found at location %s. %s",
          slText,
          whereSearched
        )
    }
  }.value

}
