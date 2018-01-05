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

package org.apache.daffodil.xml

import org.apache.daffodil.api.DaffodilTunables
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.exceptions.ThrowsSDE

/**
 * Element references and Group References use this.
 */
trait HasRefMixin extends GetAttributesMixin with ResolvesQNames {

  private lazy val xsdRef = getAttributeRequired("ref")

  /**
   * This is the only "safe" thing to use as the name of this
   * object for diagnostic/debug purposes.
   *
   * The problem is that the ref might not actually dereference to anything.
   * The namespace prefix could be invalid, or there may not be any object
   * that is the target of the reference.
   *
   * We need to be able to issue diagnostics even if those failures are there,
   * so use this to provide name/info. Not something that could fail
   * on resolving.
   */
  lazy val ref = xsdRef

  /**
   * This just resolves the namespace prefix. But even that might fail
   *  if there is no namespace definition for it.
   */
  lazy val refQName = resolveQName(ref)
}

object ResolvesQNames {
  def qNameForProperty(local: String, ns: NS, namespaces: scala.xml.NamespaceBinding) = {
    val pre = namespaces.getPrefix(ns.uri.toString)
    val prefix = Some(if (pre eq null) "daf" else pre)
    GlobalQName(prefix, local, ns)
  }
}

trait ResolvesQNames
  extends ThrowsSDE {
  def namespaces: scala.xml.NamespaceBinding
  protected def tunable: DaffodilTunables

  /**
   * If prefix of name is unmapped, SDE
   */
  def resolveQName(qnString: String): RefQName = {
    val eQN = QName.resolveRef(qnString, namespaces, tunable)
    // we don't want to just throw the exception, we want to
    // convert to an SDE, so we use recover
    val res = eQN.recover { ThrowSDE }.get
    res
  }

  def qNameForProperty(local: String, ns: NS = XMLUtils.DFDL_NAMESPACE) = ResolvesQNames.qNameForProperty(local, ns, namespaces)

  /**
   * Just chop off the prefix.
   */
  def removePrefix(prefixedValue: String): String = {
    prefixedValue match {
      case QNameRegex.QName(pre, local) => local
      case _ => Assert.usageError("The argument was not in QName syntax: '%s'".format(prefixedValue))
    }
  }
}
