package edu.illinois.ncsa.daffodil.xml

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

import java.net.URI
import edu.illinois.ncsa.daffodil.exceptions.Assert

/**
 * Central factory for, and class to represent namespace URIs
 *
 * Import this object. I.e., import edu.illinois.ncsa.daffodil.xml.NS._
 */
object NS {

  /**
   * Import these implicit conversions for convenience if you like
   */
  import scala.language.implicitConversions
  implicit def implicitNStoString(ns: NS): String = ns.toString
  implicit def implicitNStoURI(ns: NS): URI = ns.uri

  private val nsCache = scala.collection.mutable.Map[String, NS]()

  def apply(nsString: String): NS = {
    // null means NoNamespace. So does ""
    val s = if (nsString == null) "" else nsString
    fromNSString(s, None)
  }

  private def fromNSString(s: String, uriArg: Option[URI]): NS = {
    Assert.usage(s != null)
    Assert.usage(s != NoNamespace.toString)
    Assert.usage(s != UnspecifiedNamespace.toString)
    val haveIt = nsCache.get(s)
    (haveIt, uriArg) match {
      case (Some(ns), _) => ns
      case (None, Some(uri)) => {
        val newNS = new NS(uri)
        nsCache.put(s, newNS)
        newNS
      }
      case (None, None) => {
        val newNS = new NS(URI.create(s))
        nsCache.put(s, newNS)
        newNS
      }
    }
  }

  def apply(uri: URI): NS = fromNSString(uri.toString, Some(uri))

  nsCache.put("", NoNamespace)
  nsCache.put(UnspecifiedNamespace.toString, UnspecifiedNamespace)

  /**
   * Finds all prefixes for a given namespace. Used to suggest
   * possible missing prefix in diagnostic error messages.
   */
  def allPrefixes(ns: NS, nsb: scala.xml.NamespaceBinding): Seq[String] = {
    if (ns == NoNamespace) return Nil
    if (nsb == null) return Nil
    if (nsb == scala.xml.TopScope) return Nil
    val uri = ns.uri
    Assert.invariant(uri != null && uri != "")
    Assert.invariant(nsb.uri != null && nsb.uri != "")
    val moreMatches = allPrefixes(ns, nsb.parent)
    if (uri == nsb.uri) nsb.prefix +: moreMatches
    else moreMatches
  }
}

object NoNamespace extends NS(null) {
  override def isNoNamespace = true
  override def isUnspecified = false
  override def toString = "No_Namespace"
  override def uri = Assert.usageError("No-namespace has no URI.")
  override def toStringOrNullIfNoNS: String = null // most places in Java APIs, no namespace is represented by null.
  override def equalsNS(other: NS) = this eq other
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
  override def toStringOrNullIfNoNS: String = null // most places in Java APIs, no namespace is represented by null.
  override def equalsNS(other: NS) = this eq other
}

sealed class NS protected (uriArg: URI) extends Serializable { // protected constructor. Must use factory.
  override def toString = uri.toString
  def uri = uriArg
  def toStringOrNullIfNoNS = uri.toString
  def isNoNamespace = false
  def isUnspecified = false
  override def hashCode() = toString.hashCode()
  def equalsNS(other: NS): Boolean = {
    if (this eq other) return true
    Assert.invariant(this.toString != other.toString) // this fails if the cache isn't being used
    false
  }
  final override def equals(other: Any): Boolean = {
    other match {
      case otherNS: NS => equalsNS(otherNS)
      case _ => false
    }
  }
}
