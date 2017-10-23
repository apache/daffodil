/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://woverride val ww.tresys.com
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

package edu.illinois.ncsa.daffodil.dsom

import scala.xml.Node
import edu.illinois.ncsa.daffodil.exceptions.Assert

object GlobalGroupDefFactoryFactory {

  def apply(defXML: Node, schemaDocument: SchemaDocument): GlobalGroupDefFactory = {
    val trimmedXml = scala.xml.Utility.trim(defXML)
    trimmedXml match {
      case <group>{ contents @ _* }</group> => {
        val ggd = contents.collect {
          case <sequence>{ _* }</sequence> => new GlobalSequenceGroupDefFactory(defXML, schemaDocument)
          case <choice>{ _* }</choice> => new GlobalChoiceGroupDefFactory(defXML, schemaDocument)
        }
        ggd(0)
      }
      case _ => Assert.invariantFailed("not a group")
    }
  }
}

sealed abstract class GlobalGroupDefFactory(defXML: Node, schemaDocument: SchemaDocument)
  extends SchemaComponentFactory(defXML, schemaDocument)
  with GlobalNonElementComponentMixin {

  def forGroupRef(gref: => GroupRef): GlobalGroupDef
}

class GlobalSequenceGroupDefFactory(defXML: Node, schemaDocument: SchemaDocument)
  extends GlobalGroupDefFactory(defXML, schemaDocument) {

  override def forGroupRef(gref: => GroupRef) =
    new GlobalSequenceGroupDef(defXML, schemaDocument, gref.asInstanceOf[SequenceGroupRef])
}

class GlobalChoiceGroupDefFactory(defXML: Node, schemaDocument: SchemaDocument)
  extends GlobalGroupDefFactory(defXML, schemaDocument) {

  override def forGroupRef(gref: => GroupRef) =
    new GlobalChoiceGroupDef(defXML, schemaDocument, gref.asInstanceOf[ChoiceGroupRef])
}

sealed abstract class GlobalGroupDef(xmlArg: Node, schemaDocumentArg: SchemaDocument, grefArg: => GroupRef)
  extends SchemaComponentFactory(xmlArg, schemaDocumentArg)
  with GlobalNonElementComponentMixin
  with NestingTraversesToReferenceMixin {

  lazy val groupRef = grefArg

  requiredEvaluations(modelGroup)

  override def referringComponent = Some(groupRef.asModelGroup)
  //
  // Note: Dealing with XML can be fragile. It's easy to forget some of these children
  // might be annotations and Text nodes. Even if you trim the text nodes out, there are
  // places where annotations can be.
  //
  private lazy val <group>{ xmlChildren @ _* }</group> = xml
  //
  // So we have to flatMap, so that we can tolerate annotation objects (like documentation objects).
  // and our ModelGroup factory has to return Nil for annotations and Text nodes.
  //
  final lazy val Seq(modelGroup: ModelGroup) = xmlChildren.flatMap { ModelGroupFactory(_, this, -1) }
}

final class GlobalSequenceGroupDef(defXMLArg: Node, schemaDocument: SchemaDocument, gref: => SequenceGroupRef)
  extends GlobalGroupDef(defXMLArg, schemaDocument, gref)

final class GlobalChoiceGroupDef(defXMLArg: Node, schemaDocument: SchemaDocument, gref: => ChoiceGroupRef)
  extends GlobalGroupDef(defXMLArg, schemaDocument, gref) {

  requiredEvaluations(validateChoiceBranchKey)

  def validateChoiceBranchKey(): Unit = {
    // Ensure the model group of a global group def do not define choiceBranchKey.
    val found = modelGroup.findPropertyOptionThisComponentOnly("choiceBranchKey")
    if (found.isDefined) {
      SDE("dfdl:choiceBranchKey cannot be specified on the choice/sequence child of a global group definition")
    }
  }
}

