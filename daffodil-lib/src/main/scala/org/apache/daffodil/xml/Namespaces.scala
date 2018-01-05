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

import java.net.URI
import org.apache.daffodil.exceptions.Assert
import org.apache.daffodil.util.UniquenessCache
import org.apache.daffodil.util.Maybe._

/**
 * Central factory for, and class to represent namespace URIs
 *
 * Import this object. I.e., import org.apache.daffodil.xml.NS._
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
  override def toStringOrNullIfNoNS: String = null // most places in Java APIs, no namespace is represented by null.
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
  override def toStringOrNullIfNoNS: String = null // most places in Java APIs, no namespace is represented by null.
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
    Assert.invariant(this.toString != other.toString) // this fails if the cache isn't being used
    false
  }
}
