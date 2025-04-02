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

import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.util.NamedMixinBase
import org.apache.daffodil.lib.xml._

/**
 * Common Mixin for things that have a name attribute.
 */
trait NamedMixin extends GetAttributesMixin with NamedMixinBase {

  def name = getAttributeOption("name").getOrElse(Misc.getNameFromClass(this))

  override def xml: Node
  def schemaDocument: SchemaDocument

  def namespace: NS

  def prefix: String

  def namedQName: NamedQName

  override protected lazy val diagnosticDebugNameImpl = namedQName.diagnosticDebugName

}

sealed trait PrefixAndNamespaceMixin {
  def xml: Node
  def schemaDocument: SchemaDocument

  final lazy val prefix =
    xml.scope.getPrefix(namespace.toString) // can be null meaning no prefix

  lazy val namespace: NS = schemaDocument.targetNamespace

}

sealed trait LocalComponentMixinBase extends NamedMixin {

  /**
   * True for elements if elementFormDefault is 'qualified'
   * False for any other named local component type
   */
  def isQualified: Boolean

  override lazy val namedQName: NamedQName =
    QName.createLocal(name, namespace, isQualified, xml.scope)
}

trait LocalNonElementComponentMixin
  extends LocalComponentMixinBase
  with PrefixAndNamespaceMixin {

  final override def isQualified = false // non elements never have qualfied names

}

trait LocalElementComponentMixin extends LocalComponentMixinBase with ElementFormDefaultMixin {

  final override def isQualified = elementFormDefault == "qualified"

}

/**
 * All global components share these characteristics.
 * The difference between this and the not-Global flavor
 * has to do with the elementFormDefault attribute of the xs:schema
 * element. Global things are always qualified
 */
sealed trait GlobalComponent extends NamedMixin with PrefixAndNamespaceMixin {

  override def namedQName: NamedQName = globalQName

  final lazy val globalQName =
    QName.createGlobal(name, namespace, xml.scope)

  def shortSchemaComponentDesignator: String
}

trait GlobalNonElementComponentMixin extends GlobalComponent {
  // nothing for now
}

trait GlobalElementComponentMixin extends GlobalComponent

/**
 * elementFormDefault is an attribute of the xs:schema element.
 * It defaults to 'qualified'. That means nested local element definitions,
 * their names are in the target namespace. So, if you have
 * @example {{{
 * <schema elementFormDefault='qualified'
 *         targetNamespace="myURI" xmlns:tns="myURI"...>
 * <element name='foo'...>
 *    <complexType>
 *       <sequence>
 *          <element name='bar'.../>
 *       ...
 * }}}
 *  Now a DFDL/Xpath expression to reach that 'bar' element looks like /tns:foo/tns:bar
 *  Contrarywise, if elementFormDefault='unqualfied'...
 *  <pre>
 *  <schema elementFormDefault='unqualified'
 *         targetNamespace="myURI" xmlns:tns="myURI"...>
 * <element name='foo'...>
 *    <complexType>
 *       <sequence>
 *          <element name='bar'.../>
 *       ...
 * }}}
 * Now a path to reach element bar would look like /tns:foo/bar.
 *
 * See how 'bar' isn't preceded by the tns prefix. That's becasue the child elements are
 * all 'no namespace' elements.
 *
 * This also affects what a result document is like from namespaces perspective.
 * Suppose the above 'bar' element is an xs:int. Then with elemenFormDefault='qualified', an
 * instance would look like:
 * @example {{{
 * <tns:foo><tns:bar>42</tns:bar></tns:foo>
 * }}}
 * or the possibly nicer (for a large result)
 * @example {{{
 * <foo xmlns="myURI"><bar>42</bar></foo>
 * }}}
 * But if elementFormDefault='unqualified', the instance doc would be like:
 * @example {{{
 * <tns:foo><bar>42</bar></tns:foo>
 * }}}
 * In this case you really don't want to setup xmlns='myURI' because this happens:
 * @example {{{
 *  <foo xmlns="myURI><bar xmlns="">42</bar></foo>
 * }}}
 * That is, you must explicitly go to the no-namespace syntax. It doesn't happen implicitly.
 *
 * This trait is mixed into things that are affected by elementFormDefault.
 * Namely the local element declaration class.
 */
trait ElementFormDefaultMixin {

  def tunable: DaffodilTunables

  def xml: Node

  def xmlSchemaDocument: XMLSchemaDocument

  lazy val elementFormDefault = xmlSchemaDocument.elementFormDefault

  /**
   * handle elementFormDefault to qualify
   */
  final lazy val namespace =
    if (xmlSchemaDocument.elementFormDefault == "unqualified")
      NoNamespace // unqualified means no namespace
    else xmlSchemaDocument.targetNamespace

  final lazy val prefix =
    if (xmlSchemaDocument.elementFormDefault == "unqualified")
      "" // unqualified means no prefix
    else {
      //
      // name is supposed to be qualified by the target namespace
      //
      val tns = namespace
      // record this error on the schemaDocument
      xmlSchemaDocument.schemaDefinitionUnless(
        tns != NoNamespace,
        "Must have a targetNamespace if elementFormDefault='qualified'."
      )
      val prefix = {
        val existingPrefix = xml.scope.getPrefix(tns.toString)
        if (existingPrefix != null) existingPrefix
        else {
          // There is no prefix bound to this namespace
          // So we have to create a prefix. Let's try "tns", "tns1", "tns2" etc. until
          // we find one that is not bound to a namespace.
          val newPrefix = (0 until Int.MaxValue).flatMap { i =>
            // flatMap collapses the List(None, None, Some(tryPre),...) => List(tryPre,...)
            // then we just take head of this list, and we get tryPre
            // Note: this does NOT create the giant list of all Int values.
            val uniqueSuffix = if (i == 0) "" else i.toString
            val prefixStem = tunable.generatedNamespacePrefixStem
            val tryPre = prefixStem + uniqueSuffix
            if (xml.scope.getURI(tryPre) == null)
              Some(tryPre) // tryPre is not bound to a namespace, so we can use it.
            else None
          }.head
          newPrefix
        }
      }
      prefix
    }
}
