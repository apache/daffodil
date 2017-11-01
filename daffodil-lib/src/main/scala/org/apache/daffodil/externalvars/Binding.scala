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

package org.apache.daffodil.externalvars

import scala.xml.Node
import org.apache.daffodil.xml._
import org.apache.daffodil.api.DaffodilTunables

class Binding(val varQName: RefQName, val varValue: String, scope: scala.xml.NamespaceBinding = null) {

  override def toString() = {
    "<binding name='" + varQName + "'>" + varValue + "</binding>"
  }

  def hasNamespaceSpecified: Boolean = !varQName.namespace.isUnspecified

  override def hashCode = varQName.hashCode

  override def equals(o: Any): Boolean = {
    o match {
      case that: Binding => this.varQName == that.varQName
      case _ => false
    }
  }

  def globalQName =
    if (scope ne null)
      QName.createGlobal(varQName.local, varQName.namespace, scope)
    else
      GlobalQName(varQName.prefix, varQName.local, varQName.namespace)
}

/**
 * This object is for cases when external variable bindings
 * are passed in via the Command Line Interface.
 */
object Binding {

  /**
   * extSyntax is {uri}ncName, or {}ncName, or ncName
   */
  def apply(extSyntax: String, value: String): Binding = {
    val tryRefQName = QName.refQNameFromExtendedSyntax(extSyntax)
    new Binding(tryRefQName.get, value, null)
  }

  def apply(node: Node, tunable: DaffodilTunables): Binding = {
    val name = (node \ "@name").head.text
    val refQName = QName.resolveRef(name, node.scope, tunable)
    val value = node.text
    new Binding(refQName.get, value, node.scope)
  }

  def apply(name: String, namespace: Option[NS], value: String): Binding = {
    new Binding(RefQName(None, name, namespace.getOrElse(UnspecifiedNamespace)), value)
  }

}
