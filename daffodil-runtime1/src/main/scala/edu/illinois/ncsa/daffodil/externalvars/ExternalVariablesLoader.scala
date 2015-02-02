/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */

package edu.illinois.ncsa.daffodil.externalvars

import scala.xml.parsing.ConstructingParser
import java.io.File
import java.net.URI
import scala.xml.Node
import scala.io.Codec.string2codec
import edu.illinois.ncsa.daffodil.processors.EmptyVariableMap
import edu.illinois.ncsa.daffodil.processors.VariableMap
import edu.illinois.ncsa.daffodil.processors.VariableUtils
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.Misc._
import edu.illinois.ncsa.daffodil.dsom.SchemaComponentBase
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.xml.XMLUtils

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

  def getVariables(node: Node): Seq[Binding] = getBindings(node)

  def getVariables(varsFile: File): Seq[Binding] = {
    val node = getVariablesAsNode(varsFile)
    val bindings = this.getBindings(node)
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
    val bindings = getVariables(vars)
    val finalVMap = this.loadVariables(bindings, referringContext, vmap)
    finalVMap
  }

  def loadVariables(fileName: String, referringContext: ThrowsSDE, vmap: VariableMap): VariableMap = {
    Assert.usage(fileName != null, "loadVariables expects 'fileName' to not be null!")
    val f = new File(fileName)
    loadVariables(f, referringContext, vmap)
  }

  def loadVariables(uri: URI, referringContext: ThrowsSDE, vmap: VariableMap): VariableMap = {
    Assert.usage(uri != null, "loadVariables expects 'uri' to not be null!")
    val file = new File(uri)
    loadVariables(file, referringContext, vmap)
  }

  def loadVariables(file: File, referringContext: ThrowsSDE, vmap: VariableMap): VariableMap = {
    Assert.usage(file != null, "loadVariables expects 'file' to not be null!")
    ExternalVariablesValidator.validate(file) match {
      case Left(ex) => Assert.abort(ex)
      case Right(_) => // Success
    }
    val enc = determineEncoding(file) // The encoding is needed for ConstructingParser
    val input = scala.io.Source.fromURI(file.toURI)(enc)
    val node = ConstructingParser.fromSource(input, true).document.docElem
    loadVariables(node, referringContext, vmap)
  }

  def loadVariables(node: Node, referringContext: ThrowsSDE, vmap: VariableMap): VariableMap = {
    Assert.usage(node != null, "loadVariables expects 'node' to not be null!")
    Assert.usage(referringContext != null, "loadVariables expects 'referringContext' to not be null!")
    val bindings = getBindings(node)
    loadVariables(bindings, referringContext, vmap)
  }

  def loadVariables(bindings: Seq[Binding], referringContext: ThrowsSDE, vmap: VariableMap): VariableMap = {
    Assert.usage(referringContext != null, "loadVariables expects 'referringContext' to not be null!")
    val finalVMap = VariableUtils.setExternalVariables(vmap, bindings, referringContext)
    finalVMap
  }

  private def getBindings(extVarBindings: Node) = {
    val bindings = extVarBindings \ "bind"
    bindings.map(b => Binding(b))
  }

}
