package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

import scala.xml.Node

class GlobalGroupDefFactory(xmlArg: Node, schemaDocumentArg: SchemaDocument)
  extends SchemaComponent(xmlArg, schemaDocumentArg) with NamedMixin {

  def forGroupRef(gref: GroupRef, position: Int) = {
    scala.xml.Utility.trim(xml) match {
      case <group><sequence>{ _* }</sequence></group> =>
        new GlobalSequenceGroupDef(xml, schemaDocument, gref, position)
      case <group><choice>{ _* }</choice></group> =>
        new GlobalChoiceGroupDef(xml, schemaDocument, gref, position)
    }
  }

}

abstract class GlobalGroupDef(xmlArg: Node, schemaDocumentArg: SchemaDocument, val groupRef: GroupRef, position: Int)
  extends SchemaComponent(xmlArg, schemaDocumentArg)
  with GlobalComponentMixin {

  requiredEvaluations(modelGroup)

  lazy val referringComponent = {
    val res = Some(groupRef)
    res
  }

  override lazy val enclosingComponent = groupRef.enclosingComponent

  //
  // Note: Dealing with XML can be fragile. It's easy to forget some of these children
  // might be annotations and Text nodes. Even if you trim the text nodes out, there are
  // places where annotations can be.
  //
  lazy val <group>{ xmlChildren @ _* }</group> = xml
  //
  // So we have to flatMap, so that we can tolerate annotation objects (like documentation objects).
  // and our ModelGroup factory has to return Nil for annotations and Text nodes.
  //
  lazy val Seq(modelGroup: ModelGroup) = xmlChildren.flatMap { GroupFactory(_, this, position) }

}

class GlobalSequenceGroupDef(xmlArg: Node, schemaDocument: SchemaDocument, groupRef: GroupRef, position: Int)
  extends GlobalGroupDef(xmlArg, schemaDocument, groupRef, position)

class GlobalChoiceGroupDef(xmlArg: Node, schemaDocument: SchemaDocument, groupRef: GroupRef, position: Int)
  extends GlobalGroupDef(xmlArg, schemaDocument, groupRef, position)