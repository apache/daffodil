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

package org.apache.daffodil.lib.xml

import org.apache.daffodil.lib.iapi.UnqualifiedPathStepPolicy
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.ThrowsSDE

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

trait ResolvesQNames extends ThrowsSDE {

  def namespaces: scala.xml.NamespaceBinding
  def unqualifiedPathStepPolicy: UnqualifiedPathStepPolicy
  def noPrefixNamespace: NS

  /**
   * If prefix of name is unmapped, SDE
   */
  def resolveQName(qnString: String): RefQName = {
    val eQN = QName.resolveRef(
      qnString,
      namespaces,
      noPrefixNamespace,
      unqualifiedPathStepPolicy
    )
    // we don't want to just throw the exception, we want to
    // convert to an SDE, so we use recover
    val res = eQN.recover { ThrowSDE }.get
    res
  }

  def qNameForProperty(local: String, ns: NS = XMLUtils.DFDL_NAMESPACE) =
    ResolvesQNames.qNameForProperty(local, ns, namespaces)

  /**
   * Just chop off the prefix.
   */
  def removePrefix(prefixedValue: String): String = {
    prefixedValue match {
      case QNameRegex.QName(pre, local) => local
      case _ =>
        Assert.usageError("The argument was not in QName syntax: '%s'".format(prefixedValue))
    }
  }
}
