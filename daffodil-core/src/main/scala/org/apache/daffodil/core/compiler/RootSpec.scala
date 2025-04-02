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

package org.apache.daffodil.core.compiler

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.UnqualifiedPathStepPolicy.NoNamespace
import org.apache.daffodil.lib.xml.NS

/**
 * Contains a specification of the root element to be used.
 *
 * The whole RootSpec is generally optional, but if you have one,
 * the namespace part of it is optional as well.
 *
 * When the namespace part is None, it means "you, daffodil, figure out the namespace".
 * Which it will do so long as it is unambiguous.
 */
case class RootSpec(ns: Option[NS], name: String) {
  override def toString() = {
    val nsStr = ns.getOrElse("")
    "{" + nsStr + "}" + name
  }
}

object RootSpec {
  def makeRootSpec(optName: Option[String], optNamespace: Option[String]) = {
    val ns = optNamespace.map { NS(_) }
    if (optNamespace.isDefined && optName.isEmpty)
      Assert.usageError(
        "Cannot specify only a namespace without a name. Namespace argument was: "
          + (if (ns eq NoNamespace) "\"\" " + ns else ns)
      )
    optName.map { RootSpec(ns, _) }
  }
}
