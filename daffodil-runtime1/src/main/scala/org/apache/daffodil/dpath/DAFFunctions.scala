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

package org.apache.daffodil.dpath

import org.apache.daffodil.processors.unparsers.UnparseError
import org.apache.daffodil.util.Maybe.Nope
import org.apache.daffodil.util.Maybe.One
import org.apache.daffodil.infoset.DISimple
import org.apache.daffodil.infoset.XMLTextInfosetOutputter
import org.apache.daffodil.infoset.InfosetCommon

case class DAFTrace(recipe: CompiledDPath, msg: String)
  extends RecipeOpWithSubRecipes(recipe) {

  private def asXMLString(ie: InfosetCommon) = {
    val bos = new java.io.ByteArrayOutputStream()
    val xml = new XMLTextInfosetOutputter(bos, true)
    ie.visit(xml, false)
    xml.endDocument() // causes the outputter to flush to the stream
    bos.toString("UTF-8")
  }

  override def run(dstate: DState): Unit = {
    recipe.run(dstate)
    val nodeString: String = dstate.currentNode match {
      case _: DISimple | null => {
        // if there is no current node (null case) then there must be a
        // current value.
        val v = dstate.currentValue
        dstate.setCurrentValue(v)
        v.toString()
      }
      case other: InfosetCommon => "\n" + asXMLString(other)
    }
    System.err.println("trace " + msg + ":" + nodeString)
  }

  // This is toXML for the case class object, not the infoset node it is
  // dealing with.
  override def toXML = toXML(recipe.toXML)

}

case object DAFError extends RecipeOp {

  override def run(dstate: DState) {
    val maybeSFL =
      if (dstate.runtimeData.isDefined) One(dstate.runtimeData.get.schemaFileLocation)
      else Nope
    dstate.mode match {
      case UnparserNonBlocking | UnparserBlocking =>
        UnparseError(maybeSFL, dstate.contextLocation, "The error function was called.")
      case _: ParserMode => {
        val fe = new FNErrorFunctionException(maybeSFL, dstate.contextLocation, "The error function was called.")
        throw fe
      }
    }
  }
}
