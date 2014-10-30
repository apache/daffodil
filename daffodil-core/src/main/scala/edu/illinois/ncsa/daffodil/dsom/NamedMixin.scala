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

import edu.illinois.ncsa.daffodil.xml.GetAttributesMixin
import edu.illinois.ncsa.daffodil.util.Misc
import scala.xml.Node
import edu.illinois.ncsa.daffodil.xml.NoNamespace
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.xml.QName

/**
 * Common Mixin for things that have a name attribute.
 */
trait NamedMixin
  extends GetAttributesMixin { self: SchemaComponentBase =>
  override lazy val prettyName = Misc.getNameFromClass(this) + "(" + name + ")"

  requiredEvaluations(name)

  lazy val name = nameFromNameAttribute
  private lazy val nameFromNameAttribute = nameFromNameAttribute_.valueOrElse("??name??")
  private val nameFromNameAttribute_ = LV('nameFromNameAttribute) { getAttributeRequired("name") }

  def xml: Node
  def schemaDocument: SchemaDocument

  lazy val namespace = schemaDocument.targetNamespace // can be "" meaning no namespace
  lazy val prefix = {
    val prefix = xml.scope.getPrefix(namespace.toString) // can be null meaning no prefix
    // cannot be ""
    prefix
  }
}

/**
 * All global components share these characteristics.
 * The difference between this and the not-Global flavor
 * has to do with the elementFormDefault attribute of the xs:schema
 * element. Global things are always qualified
 */
trait GlobalComponentMixin
  extends NamedMixin { self: SchemaComponent =>

  lazy val namedQName = QName.createGlobal(name, targetNamespace)

  /**
   * polymorphic way to get back to what is referring to this global
   */
  def referringComponent: Option[SchemaComponent]

  /**
   * Global components have a different way to find their enclosing component,
   * which is to go back to their referring component (which will be None only for
   * the root element.
   */
  override lazy val enclosingComponent = {
    Assert.invariant(context.isInstanceOf[SchemaDocument]) // global things have schema documents as their parents.
    referringComponent
  }

}

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
trait ElementFormDefaultMixin
  extends NamedMixin { self: SchemaComponent =>

  lazy val elementFormDefault = xmlSchemaDocument.elementFormDefault

  /**
   * handle elementFormDefault to qualify
   */
  override lazy val namespace =
    if (xmlSchemaDocument.elementFormDefault == "unqualified")
      NoNamespace // unqualified means no namespace
    else xmlSchemaDocument.targetNamespace

  override lazy val prefix =
    if (xmlSchemaDocument.elementFormDefault == "unqualified")
      "" // unqualified means no prefix
    else {
      //
      // name is supposed to be qualified by the target namespace
      //
      val tns = namespace
      // record this error on the schemaDocument
      xmlSchemaDocument.schemaDefinitionUnless(tns != NoNamespace, "Must have a targetNamespace if elementFormDefault='qualified'.")
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
            val prefixStem = DaffodilTunableParameters.generatedNamespacePrefixStem
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
