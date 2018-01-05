/* Copyright (c) 2017 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.troverride val esys.com
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

import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocatable
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import scala.xml.NamespaceBinding
import edu.illinois.ncsa.daffodil.xml.NS
import edu.illinois.ncsa.daffodil.oolag.OOLAG.OOLAGHost
import scala.xml.Node

/**
 * Anything that can be computed without reference to the point of use
 * or point of reference can be computed here on these factory objects.
 */
abstract class SchemaComponentFactory( final override val xml: Node,
  final override val schemaDocument: SchemaDocument)
  extends SchemaComponent
  with NestingLexicalMixin {

  final override def parent = schemaDocument

}

abstract class AnnotatedSchemaComponentFactory( final override val xml: Node,
  final override val schemaDocument: SchemaDocument)
  extends AnnotatedSchemaComponent
  with NestingLexicalMixin {

  final override def parent = schemaDocument

}

trait SchemaFileLocatableImpl
  extends SchemaFileLocatable {

  def xml: scala.xml.Node
  def schemaFile: Option[DFDLSchemaFile]
  def parent: SchemaComponent

  /**
   * Annotations can contain expressions, so we need to be able to compile them.
   *
   * We need our own instance so that the expression compiler has this schema
   * component as its context.
   */

  override lazy val lineAttribute: Option[String] = {
    val attrText = xml.attribute(XMLUtils.INT_NS, XMLUtils.LINE_ATTRIBUTE_NAME).map { _.text }
    if (attrText.isDefined) {
      attrText
    } else if (parent != null) parent.lineAttribute
    else None
  }

  final override lazy val columnAttribute = xml.attribute(XMLUtils.INT_NS, XMLUtils.COLUMN_ATTRIBUTE_NAME) map { _.text }

  final override lazy val fileAttribute: Option[String] = {
    val optAttrNode = schemaFile.map { _.node.attribute(XMLUtils.INT_NS, XMLUtils.FILE_ATTRIBUTE_NAME) }.flatten
    val optAttrText = optAttrNode.map { _.text }
    optAttrText
  }
}

trait CommonContextMixin
  extends NestingMixin { self: OOLAGHost with ThrowsSDE =>

  def parent: SchemaComponent

  lazy val schemaFile: Option[DFDLSchemaFile] = parent.schemaFile
  lazy val schemaSet: SchemaSet = parent.schemaSet
  def schemaDocument: SchemaDocument = parent.schemaDocument
  lazy val xmlSchemaDocument: XMLSchemaDocument = parent.xmlSchemaDocument
  lazy val schema: Schema = parent.schema
  def uriString: String = parent.uriString

  def xml: scala.xml.Node

  /**
   * Namespace scope for resolving QNames.
   *
   * We insist that the prefix "xsi" is properly defined for use
   * in xsi:nil attributes, which is how we represent nilled elements
   * when we convert to XML.
   */
  final lazy val namespaces = {
    val scope = xml.scope
    val foundXsiURI = scope.getURI("xsi")
    val xsiURI = XMLUtils.xsiURI.toString
    val newScope =
      (foundXsiURI, this) match {
        case (null, e: ElementBase) => new NamespaceBinding("xsi", xsiURI, scope)
        case (`xsiURI`, _) => scope
        case (s: String, _) => schemaDefinitionError("Prefix 'xsi' must be bound to the namespace '%s', but was bound to the namespace '%s'.", xsiURI, s)
        case (null, _) => scope
      }
    newScope
  }

  /**
   * This is the root, or basic target namespace. Every schema component
   * gets its target namespace from its xmlSchemaDocument.
   */
  def targetNamespace: NS = xmlSchemaDocument.targetNamespace

  final lazy val targetNamespacePrefix = xml.scope.getPrefix(targetNamespace.toString)

}
