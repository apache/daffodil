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

package org.apache.daffodil.runtime1.externalvars

import java.io.File
import java.net.URI
import scala.collection.immutable.Queue
import scala.io.Codec.string2codec
import scala.xml.Node

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.externalvars._
import org.apache.daffodil.lib.iapi.URISchemaSource
import org.apache.daffodil.lib.util.Misc._
import org.apache.daffodil.lib.xml.DaffodilXMLLoader
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.processors.VariableMap
import org.apache.daffodil.runtime1.processors.VariableUtils

/**
 * The purpose of this object is to be able to take
 * variables specified in different ways (Map[String,String], File, Node, URI, etc.)
 * and transform it into a Seq[Binding].
 *
 * We use the Seq[Binding] to
 * populate the VariableMap with external variable values.
 *
 */
object ExternalVariablesLoader {

  def loadVariables(
    bindings: Seq[Binding],
    referringContext: ThrowsSDE,
    vmap: VariableMap
  ): VariableMap = {
    Assert.usage(
      referringContext != null,
      "loadVariables expects 'referringContext' to not be null!"
    )
    VariableUtils.setExternalVariables(vmap, bindings, referringContext)
    vmap
  }

  // The following are methods that retrieve and transform variables into Seq[Binding]

  def mapToBindings(vars: Map[String, String]): Queue[Binding] = {
    val varsKVP = vars.map {
      case (name, value) => {
        Binding(name, value)
      }
    }
    Queue.empty.enqueueAll(varsKVP)
  }

  def uriToBindings(uri: URI): Queue[Binding] = {
    Assert.usage(uri ne null)
    val file = new File(uri)
    fileToBindings(file)
  }

  def fileToBindings(file: File): Queue[Binding] = {
    Assert.usage(file ne null)
    val enc = determineEncoding(file) // The encoding is needed for ConstructingParser
    val input = scala.io.Source.fromURI(file.toURI)(enc)
    val ldr = new DaffodilXMLLoader()
    val dafextURI = XMLUtils.dafextURI
    val node = ldr.load(
      URISchemaSource(file, file.toURI),
      Some(dafextURI)
    )
    nodeToBindings(node)
  }

  def nodeToBindings(node: Node): Queue[Binding] = {
    Assert.usage(node ne null)
    val newBindings = Binding.getBindings(node)
    var res = Queue.empty[Binding]
    // couldn't get the enqueue(iterable) method overload to resolve.
    // So just doing this one by one
    newBindings.foreach { b => res = res.enqueue(b) }
    res
  }
}
