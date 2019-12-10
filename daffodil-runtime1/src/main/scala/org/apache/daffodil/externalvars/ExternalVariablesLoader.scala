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

import scala.xml.parsing.ConstructingParser
import java.io.File
import java.net.URI
import scala.xml.Node
import scala.io.Codec.string2codec
import org.apache.daffodil.processors.VariableMap
import org.apache.daffodil.processors.VariableUtils
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.Misc._
import org.apache.daffodil.exceptions.ThrowsSDE
import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.api.UnqualifiedPathStepPolicy

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

  // The following are methods that retrieve and transform variables
  // into Seq[Binding] or Node.

  private def createBindings(vars: Map[String, String]): Seq[Binding] = {
    val varsKVP = vars.map {
      case (name, value) => {
        Binding(name, value)
      }
    }.toSeq
    varsKVP
  }

  def getVariables(vars: Map[String, String]): Seq[Binding] = {
    val bindings = createBindings(vars)
    bindings
  }

  def getVariables(node: Node): Seq[Binding] =
    Binding.getBindings(node, UnqualifiedPathStepPolicy.DefaultNamespace)

  def getVariables(varsFile: File): Seq[Binding] = {
    val node = getVariablesAsNode(varsFile)
    val bindings = Binding.getBindings(node, UnqualifiedPathStepPolicy.DefaultNamespace)
    bindings
  }

  private def getVariablesAsNode(file: File): Node = {
    Assert.usage(file != null, "getVariablesAsNode expects 'file' to not be null!")
    // The difference here is that we want to validate
    // if variables are loaded from a file.
    ExternalVariablesValidator.validate(file) match {
      case Left(ex) => Assert.abort(ex)
      case Right(_) => // Success
    }
    val enc = determineEncoding(file) // The encoding is needed for ConstructingParser
    val input = scala.io.Source.fromURI(file.toURI)(enc)
    val node = ConstructingParser.fromSource(input, true).document.docElem
    node
  }

  // The following are methods that load the variables into the VariableMap and
  // return the mutated map.

  def loadVariables(vars: Map[String, String], referringContext: ThrowsSDE, vmap: VariableMap): VariableMap = {
    val bindings = try {
      getVariables(vars)
    } catch {
      case e: Throwable => referringContext.SDE("Exception when processing external variable binding file: %s", e.getMessage)
    }

    val finalVMap = this.loadVariables(bindings, referringContext, vmap)
    finalVMap
  }

  def loadVariables(fileName: String, referringContext: ThrowsSDE, vmap: VariableMap, tunableArg: DaffodilTunables): VariableMap = {
    Assert.usage(fileName != null, "loadVariables expects 'fileName' to not be null!")
    val f = new File(fileName)
    loadVariables(f, referringContext, vmap, tunableArg)
  }

  def loadVariables(uri: URI, referringContext: ThrowsSDE, vmap: VariableMap, tunableArg: DaffodilTunables): VariableMap = {
    Assert.usage(uri != null, "loadVariables expects 'uri' to not be null!")
    val file = new File(uri)
    loadVariables(file, referringContext, vmap, tunableArg)
  }

  def loadVariables(file: File, referringContext: ThrowsSDE, vmap: VariableMap, tunableArg: DaffodilTunables): VariableMap = {
    Assert.usage(file != null, "loadVariables expects 'file' to not be null!")
    ExternalVariablesValidator.validate(file) match {
      case Left(ex) => Assert.abort(ex)
      case Right(_) => // Success
    }
    val enc = determineEncoding(file) // The encoding is needed for ConstructingParser
    val input = scala.io.Source.fromURI(file.toURI)(enc)
    val node = ConstructingParser.fromSource(input, true).document.docElem
    loadVariables(node, referringContext, vmap, tunableArg)
  }

  def loadVariables(node: Node, referringContext: ThrowsSDE, vmap: VariableMap, tunableArg: DaffodilTunables): VariableMap = {
    Assert.usage(node != null, "loadVariables expects 'node' to not be null!")
    Assert.usage(referringContext != null, "loadVariables expects 'referringContext' to not be null!")
    val bindings = try {
      Binding.getBindings(node, tunableArg.unqualifiedPathStepPolicy)
    } catch {
      case e: Throwable => referringContext.SDE("Exception when processing external variable binding file: %s", e.getMessage)
    }

    loadVariables(bindings, referringContext, vmap)
  }

  def loadVariables(bindings: Seq[Binding], referringContext: ThrowsSDE, vmap: VariableMap): VariableMap = {
    Assert.usage(referringContext != null, "loadVariables expects 'referringContext' to not be null!")
    val finalVMap = VariableUtils.setExternalVariables(vmap, bindings, referringContext)
    finalVMap
  }

}
