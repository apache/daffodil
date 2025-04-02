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
package org.apache.daffodil.lib.iapi

/**
 * Static methods for the DaffodilTunables object (which is generated)
 */
trait DaffodilTunablesStaticMixin {

  final def tunablesMap(node: scala.xml.Node): Map[String, String] = {
    val trimmed = scala.xml.Utility.trim(node).headOption
    val tunablesMap =
      trimmed
        .map {
          _.child.map { n => (n.label, n.text) }.toMap
        }
        .getOrElse(Map.empty)
    tunablesMap
  }

  final def configPlusMoreTunablesMap(
    tunablesMap: Map[String, String],
    optDafConfig: Option[DaffodilConfig]
  ): Map[String, String] = {
    val configFileTunablesMap = optDafConfig.map { _.tunablesMap }.getOrElse(Map.empty)
    // Note, ++ on Maps replaces any key/value pair from the left with that on the
    // right, so key/value pairs defined in tunables overrule those defiend in
    // the config file
    val combined = configFileTunablesMap ++ tunablesMap
    combined
  }
}
