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

import java.net.URI

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Maybe._
import org.apache.daffodil.lib.util.UniquenessCache

/**
 * Central factory for, and class to represent namespace URIs
 *
 * Import this object. I.e., import org.apache.daffodil.lib.xml.NS._
 */
object NS extends UniquenessCache[URI, NS] {

  /**
   * Import these implicit conversions for convenience if you like
   */
  import scala.language.implicitConversions
  implicit def implicitNStoString(ns: NS): String = ns.toString
  implicit def implicitNStoURI(ns: NS): URI = ns.uri

  override def apply(uri: URI): NS = {
    Assert.usage(uri != null)
    super.apply(uri)
  }

  def apply(nsString: String): NS = {
    // NoNamespace and UnspecifiedNamespace do not have a URI, and so they are
    // not retrieved from the uniqueness cache
    if (nsString == null || nsString == "" || nsString == NoNamespace.toString) {
      NoNamespace
    } else if (nsString == UnspecifiedNamespace.toString) {
      UnspecifiedNamespace
    } else {
      apply(URI.create(nsString))
    }
  }

  protected def valueFromKey(uri: URI): NS = new NS(uri)
  protected def keyFromValue(ns: NS): Option[URI] = Some(ns.uri)

  /**
   * Finds all prefixes for a given namespace. Used to suggest
   * possible missing prefix in diagnostic error messages.
   */
  def allPrefixes(ns: NS, nsb: scala.xml.NamespaceBinding): Seq[String] = {
    if (ns == NoNamespace) return Nil
    if (nsb == null) return Nil
    if (nsb == scala.xml.TopScope) return Nil
    val uri = ns.uri.toString
    Assert.invariant(uri != null && uri != "")
    Assert.invariant(nsb.uri != null && nsb.uri != "")
    lazy val moreMatches = allPrefixes(ns, nsb.parent)
    if (uri == nsb.uri) nsb.prefix +: moreMatches
    else moreMatches
  }
}

object NoNamespace extends NS(null) {
  override def isNoNamespace = true
  override def isUnspecified = false
  override def toString = "No_Namespace"
  override def uri = Assert.usageError("No-namespace has no URI.")
  override def optURI = Nope
  override def toStringOrNullIfNoNS: String =
    null // most places in Java APIs, no namespace is represented by null.
  override def explainForMsg = "in no namespace"
}

/**
 * Used when we need to represent input from the user where they may have typed
 * just "foo" meaning "there's only one foo, so I'm not going to bother to specify the namespace"
 */
object UnspecifiedNamespace extends NS(null) {
  override def isNoNamespace = false
  override def isUnspecified = true
  override def toString = "Unspecified_Namespace"
  override def uri = Assert.usageError("UnspecifiedNamespace has no URI.")
  override def optURI = Nope
  override def toStringOrNullIfNoNS: String =
    null // most places in Java APIs, no namespace is represented by null.
  override def explainForMsg = "with unspecified namespace"
}

sealed class NS protected (uriArg: URI) extends Serializable { // protected constructor. Must use factory.
  override def toString = uri.toString
  def uri = uriArg
  def optURI = One(uriArg)
  def toStringOrNullIfNoNS = uri.toString
  def isNoNamespace = false
  def isUnspecified = false
  override def hashCode() = toString.hashCode()
  def explainForMsg = "in namespace " + toString

  /**
   * The readResolve function is called when these objects are deserialized. We
   * don't want deserialization to create new NS objects that aren't in the
   * cache. Instead we want to use the NS objects already in the cache, or add
   * a new one to the cache if it's not already cached and use that.
   * Additionally, because NoNamespace and UnspecifiedNamespace are not stored
   * in the cache, special case those to use those objects when necessary.
   */
  @throws(classOf[java.io.ObjectStreamException])
  protected def readResolve(): Any = {
    if (this.toString == NoNamespace.toString) NoNamespace
    else if (this.toString == UnspecifiedNamespace.toString) UnspecifiedNamespace
    else NS.apply(uri)
  }

  override def equals(other: Any): Boolean = {
    if (this eq other.asInstanceOf[AnyRef]) return true
    Assert.invariant(
      this.toString != other.toString // these do not allocate strings each time they are called.
    ) // this fails if the cache isn't being used
    false
  }
}
