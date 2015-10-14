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

package edu.illinois.ncsa.daffodil.dsom

import scala.xml.Node
import edu.illinois.ncsa.daffodil.xml._
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.exceptions.Assert

/**
 * Element references and Group References use this.
 */
trait HasRefMixin extends GetAttributesMixin with ResolvesQNames {

  private lazy val xsdRef = getAttributeRequired("ref")
  lazy val ref = xsdRef

  lazy val refQName = resolveQName(ref)
}

trait ResolvesQNames
  extends ThrowsSDE {
  def namespaces: scala.xml.NamespaceBinding

  /**
   * If prefix of name is unmapped, SDE
   */
  def resolveQName(qnString: String): RefQName = {
    val eQN = QName.resolveRef(qnString, namespaces)
    // we don't want to just throw the exception, we want to
    // convert to an SDE, so we use recover
    val res = eQN.recover { ThrowSDE }.get
    res
  }

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
