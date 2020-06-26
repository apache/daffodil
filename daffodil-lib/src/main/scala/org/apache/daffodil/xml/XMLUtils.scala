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

package org.apache.daffodil.xml

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardOpenOption

import scala.collection.mutable
import scala.collection.mutable.ArrayBuilder
import scala.xml.NamespaceBinding
import scala.xml._

import org.apache.commons.io.IOUtils

import org.apache.daffodil.exceptions._
import org.apache.daffodil.util.Misc
import org.apache.daffodil.calendar.DFDLDateConversion
import org.apache.daffodil.calendar.DFDLDateTimeConversion
import org.apache.daffodil.calendar.DFDLTimeConversion
import org.apache.daffodil.schema.annotation.props.LookupLocation

/**
 * Utilities for handling XML
 *
 * @version 1
 * @author Alejandro Rodriguez
 */
object XMLUtils {

  /**
   * We must have xsi prefix bound to the right namespace.
   * That gets enforced elsewhere.
   */
  val xmlNilAttribute = new PrefixedAttribute("xsi", "nil", "true", scala.xml.Null)

  val PositiveInfinityString = "INF"
  val NegativeInfinityString = "-INF"
  val NaNString = "NaN"

  /**
   * Legal XML v1.0 chars are #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
   */
  def remapXMLIllegalCharToPUA(checkForExistingPUA: Boolean = true, replaceCRWithLF: Boolean = true)(c: Char): Char = {
    val cInt = c.toInt
    val res = cInt match {
      case 0x9 => c
      case 0xA => c
      case 0xD =>
        if (replaceCRWithLF) 0xA.toChar // Map CR to LF. That's what XML does.
        else 0xE00D.toChar // or remap it to PUA so it is non-whitespace, and preserved.
      case _ if (cInt < 0x20) => (cInt + 0xE000).toChar
      case _ if (cInt > 0xD7FF && cInt < 0xE000) => (cInt + 0x1000).toChar
      case _ if (cInt >= 0xE000 && cInt <= 0xF8FF) => {
        if (checkForExistingPUA)
          Assert.usageError("Pre-existing Private Use Area (PUA) character found in data: '%s'".format(c))
        else c
      }
      case 0xFFFE => 0xF0FE.toChar
      case 0xFFFF => 0xF0FF.toChar
      case _ if (cInt > 0x10FFFF) => {
        Assert.invariantFailed("Character code beyond U+10FFFF found in data. Codepoint: %s".format(cInt))
      }
      case _ => c

    }
    res
  }

  def needsXMLToPUARemapping(s: String): Boolean = {
    var i = 0
    val len = s.length
    while (i < len) {
      val v = s.charAt(i).toInt
      if ((v < 0x20 && !(v == 0xA || v == 0x9)) || (v > 0xD7FF && v < 0xE000) ||
          (v >= 0xE000 && v <= 0xF8FF) || (v == 0xFFFE) || (v == 0xFFFF) || (v > 0x10FFFF)) {
        return true
      }
      i += 1
    }
    false
  }

  /**
   * Reverse of the above method
   */
  def remapPUAToXMLIllegalChar(checkForExistingPUA: Boolean = true)(c: Char): Char = {
    val cInt = c.toInt
    val res = cInt match {
      case _ if (c >= 0xE000 && c < 0xE020) => (c - 0xE000).toChar
      case _ if (c > 0xE7FF && c < 0xF000) => (c - 0x1000).toChar
      case 0xF0FE => 0xFFFE.toChar
      case 0xF0FF => 0xFFFF.toChar
      case _ if (c > 0x10FFFF) => {
        Assert.invariantFailed("Character code beyond U+10FFFF found in data. Codepoint: %s".format(c.toInt))
      }
      case _ => c
    }
    res
  }

  def needsPUAToXMLRemapping(s: String): Boolean = {
    var i = 0
    val len = s.length
    while (i < len) {
      val v = s.charAt(i).toInt
      if ((v == 0xD) || // not PUA, but string still needs remapping since CR must be mapped to LF
          (v >= 0xE000 && v < 0xE020) || (v > 0xE7FF && v < 0xF000) ||
          (v == 0xF0FE) || (v == 0xF0FF) || (v > 0x10FFFF)) {
        return true
      }
      i += 1
    }
    false
  }

  def isLeadingSurrogate(c: Char) = {
    c >= 0xD800 && c <= 0xDBFF
  }

  def isTrailingSurrogate(c: Char) = {
    c >= 0xDC00 && c <= 0xDFFF
  }

  /**
   * Length where a surrogate pair counts as 1 character, not two.
   */
  def uncodeLength(s: String) = {
    // performance note: this might get called a lot. So needs to be fast.
    // it needs to scan the string once, examine each character.
    // using getBytes utf-32 isn't necessarily slow. It might be fine.
    val res = s.getBytes("UTF-32BE").length / 4
    res
  }

  /**
   * Because of surrogate pairs, and the difference between 16-bit string codepoints
   * and real character codes, lots of things that traverse strings need
   * to consider either the codepoint after (if current is a leading surrogate)
   * or codepoint before (if current is a trailing surrogate).
   *
   * This calls a body function with prev, current, next bound to those.
   * For first codepoint prev will be 0. For last codepoint next will be 0.
   *
   * NOTE: This function contains the same algorithm as
   * remapXMLIllegalCharactersToPUA, but is more general and is a bit slower.
   * Any changes made to this function probably need to be incorporated into
   * the other.
   */
  def walkUnicodeString[T](str: String)(bodyFunc: (Char, Char, Char) => T): Seq[T] = {
    val len = str.length
    if (len == 0) return Nil

    val list = new scala.collection.mutable.ListBuffer[T]

    var pos = 0;
    var prev = 0.toChar
    var curr = str(0)
    var next = 0.toChar

    while (pos < len) {
      next = if (pos + 1 < len) str(pos + 1) else 0.toChar
      list += bodyFunc(prev, curr, next)
      prev = curr
      curr = next

      pos += 1
    }
    list
  }

  /*
   * This function contains the same string traversal algorithm as
   * walkUnicodeString. The only difference is that it uses a StringBuilder
   * rather than a ListBuffer[T] that would be used in walkUnicodeString. Note
   * that since StringBuilder is not synchronized it is noticably faster than
   * StringBuffer, and since the StringBuilder is local to the function, we
   * don't have to worry about any threading issues. This specificity makes for
   * a noticable speed increase, so much so that the code duplication is worth
   * it. Any changes made to this function probably need to be incorporated
   * into the other.
   */
  def remapXMLCharacters(dfdlString: String, remapFunc: (Char) => Char): String = {
    // we want to remap XML-illegal characters
    // but leave legal surrogate-pair character pairs alone.
    def remapOneChar(previous: Char, current: Char, next: Char): Char = {
      if (isLeadingSurrogate(current) && isTrailingSurrogate(next)) return current
      if (isTrailingSurrogate(current) && isLeadingSurrogate(previous)) return current
      remapFunc(current)
    }

    val len = dfdlString.length
    if (len == 0) return dfdlString

    val sb = new StringBuilder()

    var pos = 0;
    var prev = 0.toChar
    var curr = dfdlString(0)
    var next = 0.toChar

    while (pos < len) {
      next = if (pos + 1 < len) dfdlString(pos + 1) else 0.toChar
      if (curr == 0xD) {
        if (next != 0xA) {
          // This is a lone CR (i.e. not a CRLF), so convert the CR to a LF
          sb.append(0xA.toChar)
        } else {
          // This is a CRLF. Skip the CR, essentially converting the CRLF to
          // just LF. Do nothing.
        }
      } else {
        sb.append(remapOneChar(prev, curr, next))
      }
      prev = curr
      curr = next

      pos += 1
    }

    sb.toString
  }

  def remapXMLIllegalCharactersToPUA(dfdlString: String): String = {
    if (needsXMLToPUARemapping(dfdlString)) {
      // This essentially doubles the work if remapping is needed (since we
      // scan the string once to see if it's needed, then scan again for
      // remapping). But the common case is that remapping is not needed, so we
      // only need to scan the string once AND we avoid allocating a new string
      // with characters remapped.
      remapXMLCharacters(dfdlString, remapXMLIllegalCharToPUA(false))
    } else {
      dfdlString
    }
  }

  def remapPUAToXMLIllegalCharacters(dfdlString: String): String = {
    if (needsPUAToXMLRemapping(dfdlString)) {
      // This essentially doubles the work if remapping is needed (since we
      // scan the string once to see if it's needed, then scan again for
      // remapping). But the common case is that remapping is not needed, so we
      // only need to scan the string once AND we avoid allocating a new string
      // with characters remapped.
      remapXMLCharacters(dfdlString, remapPUAToXMLIllegalChar(false))
    } else {
      dfdlString
    }
  }

  def coalesceAllAdjacentTextNodes(node: Node): Node = {
    node match {
      case Elem(prefix, label, attribs, scope, child @ _*) => {
        val coalescedChildren = child.map(coalesceAllAdjacentTextNodes(_))
        val newChildren = coalesceAdjacentTextNodes(coalescedChildren)
        Elem(prefix, label, attribs, scope, true, newChildren: _*)
      }
      case x => x
    }
  }

  /*
   * This is needed for equality comparison of XML.
   *
   * Ex: "foo&#x221;bar" is 3 nodes, not one string node.
   * But appears to be one string when serialized as XML.
   *
   * Once the XML has been read into XML objects, the 3 objects
   * are just 3 adjacent text nodes, so adjacent text nodes
   * can be coalesced for use in the DFDL Infoset, or for comparing
   * trees of XML that may have been created different ways.
   */
  def coalesceAdjacentTextNodes(seq: Seq[Node]): Seq[Node] = {
    if (seq.length == 0) return seq
    if (seq.length == 1) {
      seq(0) match {
        case a: Atom[_] => return seq
        case _ => // fall through to code below. (We need to process children)
      }
    }
    val ab = ArrayBuilder.make[Node]
    var i = 0
    //
    // invariant: either the tn node is null
    // or the stringbuilder is null or empty
    //
    // They never both have content.
    //
    var tn: Node = null
    var sb: StringBuilder = null
    def processText = {
      if (tn == null) {
        if (sb != null && sb.length > 0) {
          // we have accumulated text
          // let's output a text node
          // Note that a Text constructor
          // will escapify the text again.
          // We unescaped it
          // when we used .text to get data
          // out of the nodes.
          ab += new Text(sb.toString)
          sb.clear()
        }
      } else {
        // tn not null
        Assert.invariant(sb == null || sb.length == 0)
        ab += tn
        tn = null
      }
    }
    while (i < seq.length) {
      val current = seq(i)
      i = i + 1
      if ((current.isInstanceOf[Text] || current.isInstanceOf[Unparsed])) {
        if (tn == null) {
          if (sb == null || sb.length == 0) {
            // hold onto this text node. It might be isolated
            tn = current
          } else {
            // accumulate this text
            sb.append(current.text)
          }
        } else {
          if (sb == null) sb = new StringBuilder
          // accumulate both the pending tn text node
          // and this new one we just encountered.
          //
          // Note we use .text here - that unescapifies
          // Which is important since we're putting together
          // things that might be PCData (aka <![CDATA[...]]>
          // We want that stuff gone.
          //
          sb.append(tn.text)
          sb.append(current.text)
          //
          // set tn to null to indicate we're accumulating
          // into the string buffer
          //
          tn = null
        }
      } else {
        // not an atom
        processText // if there is pending text output that first
        ab += current // then the current non-atom node.
      }
    }
    // we fell out of the loop. So
    processText // in case there is text left pending when we hit the end
    ab.result
  }

  val XSD_NAMESPACE = NS("http://www.w3.org/2001/XMLSchema") // removed trailing slash (namespaces care)
  val XSI_NAMESPACE = NS("http://www.w3.org/2001/XMLSchema-instance")
  val XPATH_FUNCTION_NAMESPACE = NS("http://www.w3.org/2005/xpath-functions")
  val XPATH_MATH_NAMESPACE = NS("http://www.w3.org/2005/xpath-functions/math")
  val DFDL_NAMESPACE = NS("http://www.ogf.org/dfdl/dfdl-1.0/") // dfdl ns does have a trailing slash
  val DFDLX_NAMESPACE = NS("http://www.ogf.org/dfdl/dfdl-1.0/extensions")
  val TDML_NAMESPACE = NS("http://www.ibm.com/xmlns/dfdl/testData")
  val EXAMPLE_NAMESPACE = NS("http://example.com")
  val XHTML_NAMESPACE = NS("http://www.w3.org/1999/xhtml")

  /**
   * Returns an Elem with local name "element", and the scope provided
   * with the prefix of the Elem setup to match the scope's binding
   * for the XSD_NAMESPACE.
   *
   * If the XSD_NAMESPACE is the default namespace, then the prefix will
   * be null. If the XSD_NAMESPACE is bound to a prefix, the first such
   * prefix will be used for the returned Elem.
   */
  def getXSDElement(scope: NamespaceBinding): Elem = {
    val xsdPre = scope.getPrefix(XSD_NAMESPACE.toString)
    val isXSDTheDefaultNS = XSD_NAMESPACE.toString() == scope.getURI(null)
    val xsdPrefix =
      if (xsdPre ne null) xsdPre
      else if (isXSDTheDefaultNS) null
      else Assert.usageError("Scope argument must have a binding for the XSD namespace.")
    val res =
      Elem(xsdPrefix, "element", Null, scope, true)
    res
  }

  /**
   * Added to support extensions and proposed future features as part of daffodil.
   *
   * The DFDL standard requires us to keep these out of the primary DFDL namespace, and
   * we really should be using URN-style notation, not http URLs for these.
   * (for why http URLs are a bad idea for these, see:
   * http://www.w3.org/blog/systeam/2008/02/08/w3c_s_excessive_dtd_traffic/ )
   *
   * These definitions must match their XSD counterparts in dafint.xsd and dafext.xsd
   */
  private val DAFFODIL_EXTENSIONS_NAMESPACE_ROOT_NCSA = "urn:ogf:dfdl:2013:imp:opensource.ncsa.illinois.edu:2012"
  private val DAFFODIL_EXTENSION_NAMESPACE_NCSA = NS(DAFFODIL_EXTENSIONS_NAMESPACE_ROOT_NCSA + ":ext")
  val EXT_PREFIX_NCSA = "daf"
  val EXT_NS_NCSA = NS(DAFFODIL_EXTENSION_NAMESPACE_NCSA.uri)

  private val DAFFODIL_EXTENSIONS_NAMESPACE_ROOT_APACHE = "urn:ogf:dfdl:2013:imp:daffodil.apache.org:2018"
  private val DAFFODIL_EXTENSION_NAMESPACE_APACHE = NS(DAFFODIL_EXTENSIONS_NAMESPACE_ROOT_APACHE + ":ext")
  val EXT_PREFIX_APACHE = "daf"
  val EXT_NS_APACHE = NS(DAFFODIL_EXTENSION_NAMESPACE_APACHE.uri)

  private val DAFFODIL_INTERNAL_NAMESPACE = NS(DAFFODIL_EXTENSIONS_NAMESPACE_ROOT_APACHE + ":int")
  val INT_PREFIX = "dafint"
  val INT_NS = NS(DAFFODIL_INTERNAL_NAMESPACE.uri)

  val FILE_ATTRIBUTE_NAME = "file"
  val LINE_ATTRIBUTE_NAME = "line"
  val COLUMN_ATTRIBUTE_NAME = "col"

  // shorter forms, to make constructing XML literals,... make the lines shorter.
  val xsdURI = XSD_NAMESPACE
  val dfdlURI = DFDL_NAMESPACE
  val dfdlxURI = DFDLX_NAMESPACE
  val dfdlAppinfoSource = NS("http://www.ogf.org/dfdl/")
  val targetNS = EXAMPLE_NAMESPACE // we use this for tests.
  val xsiURI = XSI_NAMESPACE
  val fnURI = XPATH_FUNCTION_NAMESPACE
  val mathURI = XPATH_MATH_NAMESPACE
  val dafintURI = DAFFODIL_INTERNAL_NAMESPACE

  val DFDL_SIMPLE_BUILT_IN_TYPES =
    List(
      "string",
      "float",
      "double",
      "decimal",
      "integer",
      "long",
      "int",
      "short",
      "byte",
      "unsignedLong",
      "unsignedInt",
      "nonNegativeInteger",
      "unsignedShort",
      "unsignedByte",
      "boolean",
      "date",
      "time",
      "dateTime",
      "hexBinary")

  def slashify(s: String): String = if (s == "" || s.endsWith("/")) s else s + "/"

  /**
   * Annoying, but namespace bindings are never a collection you can process like a normal collection.
   * Instead they are linked by these parent chains.
   */
  def namespaceBindings(nsBinding: NamespaceBinding): Seq[NamespaceBinding] = {
    if (nsBinding == null) Nil
    else {
      val thisOne =
        if (nsBinding.uri != null) List(nsBinding)
        else Nil
      val others = namespaceBindings(nsBinding.parent)
      thisOne ++ others
    }
  }

  /**
   * We don't want to be sensitive to which prefix people bind
   */
  def dfdlAttributes(n: Node) = {
    n.attributes filter {
      _.getNamespace(n) == DFDL_NAMESPACE.toString
    }
  }

  def dfdlxAttributes(n: Node) = {
    n.attributes.filter {
      _.getNamespace(n) == DFDLX_NAMESPACE.toString
    }
  }

  def dafAttributes(n: Node) = {
    n.attributes.filter { a =>
      a.getNamespace(n) == XMLUtils.EXT_NS_NCSA.toString ||
        a.getNamespace(n) == XMLUtils.EXT_NS_APACHE.toString
    }
  }

  /**
   * Used to collapse the excessive xmlns proliferation.
   *
   * If a local scope has bindings in it that are not in the outer scope
   * then a new local scope is created which extends the outer scope.
   *
   * This algorithm is n^2 (or worse) in the length of the outer binding chain (worst case).
   */
  def combineScopes(local: NamespaceBinding, outer: NamespaceBinding): NamespaceBinding = {
    if (local == TopScope) outer
    else {
      val NamespaceBinding(pre, uri, moreBindings) = local
      val outerURI = outer.getURI(pre)
      if (outerURI == uri) {
        // same binding for this prefix in the outer, so we don't need
        // this binding from the local scope.
        combineScopes(moreBindings, outer)
      } else if (outerURI == null) {
        // outer lacks a binding for this prefix
        NamespaceBinding(pre, uri, combineScopes(moreBindings, outer))
      } else {
        // outer has a different binding for this prefix.
        // one would hope that we can just put our superceding binding on the
        // front, but you end up with two bindings for the same prefix
        // in the chain ... and things fail
        //
        // The problem this creates is that it un-shares all the sub-structure
        // of the scopes, and so we no longer have contained elements
        // that share scopes with enclosing parents. That may mean that
        // lots of xmlns:pre="ns" proliferate again even though they're
        // unnecessary.
        //
        val outerWithoutDuplicate = removeBindings(NamespaceBinding(pre, uri, TopScope), outer)
        val moreBindingsWithoutConflict = removeBindings(NamespaceBinding(pre, uri, TopScope), moreBindings)
        NamespaceBinding(pre, uri, combineScopes(moreBindingsWithoutConflict, outerWithoutDuplicate))
      }
    }
  }

  /**
   * remove all the binding s
   */
  def removeBindings(nb: NamespaceBinding, scope: NamespaceBinding): NamespaceBinding = {
    if (nb == TopScope) scope
    else if (scope == TopScope) scope
    else {
      val NamespaceBinding(pre, _, more) = scope
      if (nb.getURI(pre) != null) {
        // the scope has a binding for this prefix
        // so irrespective of the uri, we remove it.
        removeBindings(nb, more)
      } else {
        // no binding, so keep it
        scope.copy(parent = removeBindings(nb, more))
      }
    }
  }

  def combineScopes(prefix: String, ns: NS, outer: NamespaceBinding): NamespaceBinding = {
    if (ns.optURI.isEmpty) {
      outer
    } else {
      val uri = ns.optURI.get.toString
      val inner = NamespaceBinding(prefix, uri, TopScope)
      combineScopes(inner, outer)
    }
  }

  def collapseScopes(x: Node, outer: NamespaceBinding): Node = {
    x match {
      case Elem(pre, lab, md, scp, child @ _*) => {
        val newScope = combineScopes(scp, outer)
        Elem(pre, lab, md, newScope, true, (child flatMap { ch => collapseScopes(ch, newScope) }): _*)
      }
      case _ => x
    }
  }

  /**
   * Removes NamespaceBindings from a scope containing specified namespaces
   */
  def filterScope(nsb: NamespaceBinding, nss: Seq[NS]): NamespaceBinding = {
    val newHead =
      if (nsb == xml.TopScope) {
        xml.TopScope
      } else {
        val parentCopy = filterScope(nsb.parent, nss)
        if (nss.contains(NS(nsb.uri))) {
          parentCopy
        } else {
          nsb.copy(parent = parentCopy)
        }
      }
    newHead
  }

  /**
   * Determines if a prefix is defined inside a scope
   */
  def prefixInScope(prefix: String, scope: NamespaceBinding): Boolean = {
    val ret =
      if (scope == null) {
        false
      } else if (prefix == scope.prefix) {
        true
      } else {
        prefixInScope(prefix, scope.parent)
      }
    ret
  }

  /**
   * Remove Comments
   */

  def removeComments(e: Node): Node = {
    e match {
      case Elem(prefix, label, attribs, scope, child @ _*) => {
        val newChildren = child.filterNot { _.isInstanceOf[Comment] }.map { removeComments(_) }
        Elem(prefix, label, attribs, scope, true, newChildren: _*)
      }
      case x => x
    }
  }

  /**
   * Removes attributes that we want to ignore when comparing
   * infosets.
   *
   * Removes dafint namespace attributes such as dafint:line and dafint:col.
   *
   * If a sequence of namespaces are given, only those attributes and scopes in
   * those namepsaces are removed. Otherwise, all attributes and scopes (aside
   * from special ones like xsi:nil) are removed. Additionally, if a scope is
   * filtered, the prefixes of elements prefixed with filtered scopes are also
   * removed.
   *
   * If a scope is given, it will be used for a child element if the
   * childs filtered scope is the same as the scope.
   *
   * Also strips out comments and mixed whitespace nodes, and coalesces
   * adjacent text nodes.
   *
   * Throws an exception if it contains mixed non-whitespace nodes.
   */
  def removeAttributes(n: Node, ns: Seq[NS] = Seq[NS](), parentScope: Option[NamespaceBinding] = None): Node = {
    val res1 = removeAttributes1(n, ns, parentScope).asInstanceOf[scala.xml.Node]
    val res2 = removeMixedWhitespace(res1)
    val res = res2(0) // .asInstanceOf[scala.xml.Node]
    res
  }

  /**
   * removes insignificant whitespace from between elements
   */

  private def removeMixedWhitespace(ns: Node): Node = {
    if (!ns.isInstanceOf[Elem]) return ns
    val e = ns.asInstanceOf[Elem]
    val children = e.child
    val noMixedChildren =
      if (children.exists(_.isInstanceOf[Elem])) {
        children.filter {
          case Text(data) if data.matches("""\s*""") => false
          case Text(data) => throw new Exception("Element %s contains mixed data: %s".format(e.label, data))
          case _ => true
        }.map(removeMixedWhitespace)
      } else {
        children.filter {
          //
          // So this is a bit strange, but we're dropping nodes that are Empty String.
          //
          // In XML we cannot tell <foo></foo> where there is a Text("") child, from <foo></foo> with Nil children
          //
          case Text("") => false // drop empty strings
          case _ => true
        }
      }

    val res =
      if (noMixedChildren eq children) e
      else e.copy(child = noMixedChildren)
    res
  }

  def convertPCDataToText(n: Node): Node = {
    val res = n match {
      case t: Text => t
      case a: Atom[_] => Text(a.text)
      case Elem(prefix, label, attributes, scope, children @ _*) => {
        val newChildren = children.map { convertPCDataToText(_) }
        Elem(prefix, label, attributes, scope, true, newChildren: _*)
      }
      case _ => n
    }
    res
  }

  private def removeAttributes1(n: Node, ns: Seq[NS], parentScope: Option[NamespaceBinding]): NodeSeq = {
    val res = n match {

      case e @ Elem(prefix, label, attributes, scope, children @ _*) => {

        val filteredScope = if (ns.length > 0) filterScope(scope, ns) else xml.TopScope

        // If the filtered scope is logically the same as the parent scope, use
        // the parent scope. Scala uses references to determine if scopes are
        // the same during pretty printing. However, scopes are immutable, so
        // the filter algorithm creates new scopes. Because of this, we need to
        // ignore the newly filtered scope if it is logically the same as the
        // parent so that the scala pretty printer doesn't see them as
        // different scopes.
        val newScope = parentScope match {
          case Some(ps) => if (ps == filteredScope) ps else filteredScope
          case None => filteredScope
        }

        val newChildren: NodeSeq = children.flatMap { removeAttributes1(_, ns, Some(newScope)) }

        // Important to merge adjacent text. Otherwise when comparing
        // two structures that print out the same, they might not be equal
        // because they have different length lists of text nodes
        //
        // Ex: <foo>A&#xE000;</foo> creates an element containing TWO
        // text nodes. But coming from the Daffodil Infoset, a string like
        // that would be just one text node.
        // Similarly <foo>abc<![CDATA[def]]>ghi</foo> has 3 child nodes.
        // The middle one is PCData. The two around it are Text.
        // Both Text and PCData are Atom[String].

        // Note: as of 2018-04-30, Mike Beckerle said: I am unable to reproduce the above.
        // The first example: <foo>A&#xE000;</foo>.child returns an array buffer with 1 child in it
        // which is a Text node. The <foo>abc<![CDATA[def]]>ghi</foo> also has only
        // one Text node.  That said, this is from typing them at the scala shell.
        //
        // I suspect the above observations require that the scala.xml.parsing.ConstructingParser
        // is used. We do use this, because while the regular XML loader coalesces
        // text nodes well, but doesn't preserve whitespace for CDATA regions well. That's why we use the
        // scala.xml.parser.ConstructingParser, which doesn't coalesce text nodes
        // so well, and that's what motivates this explicit coalesce pass.
        //
        // See test test_scala_loader_cdata_bug - which characterizes the behavior
        // that is problematic for us in the standard loader, and why we have to use
        // the ConstructingParser.
        //
        val textMergedChildren = coalesceAdjacentTextNodes(newChildren)

        val newPrefix = if (prefixInScope(prefix, newScope)) prefix else null

        val newAttributes = attributes.filter { m =>
          m match {
            case xsiNilAttr @ PrefixedAttribute(_, "nil", Text("true"), _) if (NS(xsiNilAttr.getNamespace(e)) == XMLUtils.XSI_NAMESPACE) => {
              true
            }
            //
            // This tolerates xsi:nil='true' when xsi has no definition at all.
            case xsiNilAttr @ PrefixedAttribute("xsi", "nil", Text("true"), _) if (xsiNilAttr.getNamespace(e) == null) => {
              true
            }
            case dafIntAttr @ PrefixedAttribute(pre, _, _, _) if (pre ne null) && (dafIntAttr.getNamespace(e) == XMLUtils.DAFFODIL_INTERNAL_NAMESPACE.toString) => {
              Assert.invariant(pre != "")
              false // drop dafint attributes.
            }
            case xsiTypeAttr @ PrefixedAttribute(_, "type", _, _) if (NS(xsiTypeAttr.getNamespace(e)) == XMLUtils.XSI_NAMESPACE) => {
              // TODO: actually check xsi:type attributes are correct according
              // to the schema--requires schema-awareness in TDML Runner.
              // Do not hide xsi:type since it is used for hints for type aware
              // comparisons.
              true
            }
            case xsiTypeAttr @ PrefixedAttribute("xsi", "type", _, _) => {
              // TODO: actually check xsi:type attributes are correct according
              // to the schema--requires schema-awareness in TDML Runner.
              // Do not hide xsi:type since it is used for hints for type aware
              // comparisons.
              true
            }
            case attr =>
              true // keep all other attributes
          }
        }

        Elem(newPrefix, label, newAttributes, newScope, true, textMergedChildren: _*)
      }
      case c: scala.xml.Comment => NodeSeq.Empty // remove comments
      case other => other
    }
    res
  }

  /**
   * Prepares an XML node for diff comparison
   */
  private def prepareForDiffComparison(n: Node): Node = {
    val noComments = removeComments(n)
    val noPCData = convertPCDataToText(noComments)
    val combinedText = coalesceAllAdjacentTextNodes(noPCData)
    val noMixedWS = removeMixedWhitespace(combinedText)
    noMixedWS
  }

  class XMLDifferenceException(message: String) extends Exception(message)

  def compareAndReport(expected: Node, actual: Node, ignoreProcInstr: Boolean = true) = {
    val expectedMinimized = prepareForDiffComparison(expected)
    val actualMinimized = prepareForDiffComparison(actual)
    val diffs = XMLUtils.computeDiff(expectedMinimized, actualMinimized, ignoreProcInstr)
    if (diffs.length > 0) {
      throw new XMLDifferenceException("""
Comparison failed.
Expected (attributes stripped)
          %s
Actual (attributes ignored for diff)
          %s
Differences were (path, expected, actual):
%s""".format(
        removeAttributes(expected).toString,
        actual,
        diffs.map { _.toString }.mkString("- ", "\n- ", "\n")))
    }
  }

  /**
   * computes a precise difference list which is a sequence of triples.
   * Each triple is the path (an x-path-like string), followed by expected, and actual values.
   */
  def computeDiff(a: Node, b: Node, ignoreProcInstr: Boolean = true) = {
    computeDiffOne(a, b, None, Nil, ignoreProcInstr, None)
  }

  def childArrayCounters(e: Elem) = {
    val Elem(_, _, _, _, children @ _*) = e
    val labels = children.map { _.label }
    val groups = labels.groupBy { x => x }
    val counts = groups.map { case (label, labelList) => (label, labelList.length) }
    val arrayCounts = counts.filter { case (label, 1) => false; case _ => true } // remove counters for scalars
    val arrayCounters = arrayCounts.map { case (label, _) => (label, 1.toLong) } // 1 based like XPath!
    arrayCounters
  }

  def computeDiffOne(
    an: Node,
    bn: Node,
    maybeIndex: Option[Int],
    parentPathSteps: Seq[String],
    ignoreProcInstr: Boolean,
    maybeType: Option[String]): Seq[(String, String, String)] = {
    lazy val zPath = parentPathSteps.reverse.mkString("/")
    (an, bn) match {
      case (a: Elem, b: Elem) => {
        val Elem(_, labelA, attribsA, _, childrenA @ _*) = a
        val Elem(_, labelB, attribsB, _, childrenB @ _*) = b
        val typeA: Option[String] = a.attribute(XSI_NAMESPACE.toString, "type").map(_.head.text)
        val typeB: Option[String] = b.attribute(XSI_NAMESPACE.toString, "type").map(_.head.text)
        val maybeType: Option[String] = Option(typeA.getOrElse(typeB.getOrElse(null)))
        val nilledA = a.attribute(XSI_NAMESPACE.toString, "nil")
        val nilledB = b.attribute(XSI_NAMESPACE.toString, "nil")

        if (labelA != labelB) {
          // different label
          List((zPath, labelA, labelB))
        } else if (nilledA != nilledB) {
          // different xsi:nil
          List((zPath + "/" + labelA + "@xsi:nil",
            nilledA.map(_.toString).getOrElse(""),
            nilledB.map(_.toString).getOrElse("")))
        } else if (typeA != typeB && typeA.isDefined && typeB.isDefined) {
          // different xsi:type (if both suppplied)
          List((zPath + "/" + labelA + "@xsi:type",
            typeA.map(_.toString).getOrElse(""),
            typeA.map(_.toString).getOrElse("")))
        } else {
          val pathLabel = labelA + maybeIndex.map("[" + _ + "]").getOrElse("")
          val thisPathStep = pathLabel +: parentPathSteps

          val (childrenACompare, childrenBCompare) =
            if (ignoreProcInstr) {
              val ca = childrenA.filterNot(_.isInstanceOf[ProcInstr])
              val cb = childrenB.filterNot(_.isInstanceOf[ProcInstr])
              (ca, cb)
            } else {
              (childrenA, childrenB)
            }

          // for elements with repeats we want to use an index in any diff
          // outut. So for repeating children, we'll create a mutable map where
          // the key is the label and the value is the count of how many
          // children of that label we've seen
          val repeatingChildrenLabels = childrenA.groupBy(_.label).filter { case (k,v) => v.length > 1 }.keys
          val labelsWithZeroCount = repeatingChildrenLabels.map { _ -> 0 }
          val countMap = mutable.Map(labelsWithZeroCount.toSeq: _*)

          val childrenDiffs = childrenACompare.zip(childrenBCompare).flatMap { case (ca, cb) =>
            val maybeChildCount = countMap.get(ca.label)
            val maybeChildIndex = maybeChildCount.map { count =>
              countMap(ca.label) += 1
              count + 1
            }
            computeDiffOne(ca, cb, maybeChildIndex, thisPathStep, ignoreProcInstr, maybeType)
          }

          // if childrenA and childrenB have different length, zip will drop an
          // extra. This will report a diff if the lengths are off.
          val childrenLengthDiff =
            if (childrenA.length != childrenB.length) {
              List((zPath + "/" + labelA + "::child@count)",
                childrenA.length.toString,
                childrenB.length.toString))
            } else {
              Nil
            }

          childrenDiffs ++ childrenLengthDiff
        }
      }
      case (tA: Text, tB: Text) => {
        val thisDiff = computeTextDiff(zPath, tA, tB, maybeType)
        thisDiff
      }
      case (pA: ProcInstr, pB: ProcInstr) => {
        val ProcInstr(tA1label, tA1content) = pA
        val ProcInstr(tB1label, tB1content) = pB
        val labelDiff = computeTextDiff(zPath, tA1label, tB1label, None)
        //
        // The content of a ProcInstr is technically a big string
        // But our usage of them the content is XML-like so could be loaded and then compared
        // as XML, if the label is in fact an indicator that this is our special
        // PI with format info.
        //
        // Much of that XML-ish content is attributes however, so we need to be sure
        // we're comparing those too.
        //
        // TODO: implement XML-comparison for our data format info PIs.
        //
        val contentDiff = computeTextDiff(zPath, tA1content, tB1content, maybeType)
        labelDiff ++ contentDiff
      }
      case _ => {
        List((zPath, an.toString, bn.toString))
      }
    }
  }

  def computeTextDiff(
    zPath: String,
    tA: Text,
    tB: Text,
    maybeType: Option[String]): Seq[(String, String, String)] = {

    val dataA = tA.toString
    val dataB = tB.toString
    computeTextDiff(zPath, dataA, dataB, maybeType)
  }

  def computeBlobDiff(zPath: String, dataA: String, dataB: String) = {
    val uriA = Misc.searchResourceOption(dataA, None)
    val uriB = Misc.searchResourceOption(dataB, None)

    val pathA = uriA.map { u => Paths.get(u) }
    val pathB = uriB.map { u => Paths.get(u) }

    val canReadA = pathA.map { p => Files.isReadable(p) }.getOrElse(false)
    val canReadB = pathB.map { p => Files.isReadable(p) }.getOrElse(false)

    if (!canReadA || !canReadB) {
      val path = zPath + ".canRead"
      Seq((path, canReadA.toString, canReadB.toString))
    } else {
      val CHUNK_SIZE = 1024
      val arrayA = new Array[Byte](CHUNK_SIZE)
      val arrayB = new Array[Byte](CHUNK_SIZE)

      val streamA = Files.newInputStream(pathA.get, StandardOpenOption.READ)
      val streamB = Files.newInputStream(pathB.get, StandardOpenOption.READ)

      var lenA: Int = 0
      var lenB: Int = 0
      var numSameBytes: Int = 0
      var areSame: Boolean = true

      while ({
          lenA = streamA.read(arrayA)
          lenB = streamB.read(arrayB)
          areSame = lenA == lenB && arrayA.sameElements(arrayB)
          areSame && lenA != -1 && lenB != -1
      }) {
        numSameBytes += lenA
      }

      if (!areSame) {
        val zip = arrayA.zip(arrayB)
        val firstDiffIndex = zip.indexWhere(z => z._1 != z._2)

        val MAX_CONTEXT = 40
        val contextA = arrayA.take(lenA).drop(firstDiffIndex).take(MAX_CONTEXT)
        val contextB = arrayB.take(lenB).drop(firstDiffIndex).take(MAX_CONTEXT)
        val hexA = Misc.bytes2Hex(contextA)
        val hexB = Misc.bytes2Hex(contextB)

        val absoluteIndex = numSameBytes + firstDiffIndex
        val path = zPath + ".bytesAt(" + (absoluteIndex + 1) + ")"
        Seq((path, hexA, hexB))
      } else {
        Nil
      }
    }
  }

  def computeTextDiff(
    zPath: String,
    dataA: String,
    dataB: String,
    maybeType: Option[String]): Seq[(String, String, String)] = {

    if (maybeType.isDefined && maybeType.get == "xs:anyURI") computeBlobDiff(zPath, dataA, dataB)
    else if (textIsSame(dataA, dataB, maybeType)) Nil
    else {
      // There must be some difference, so let's find just the first index of
      // difference and we'll include that and some following characters for
      // context.
      val CHARS_TO_SHOW_AFTER_DIFF = 40

      val lenA = dataA.length
      val lenB = dataB.length
      var index = 0
      while (index < lenA && index < lenB && dataA(index) == dataB(index)) {
        index += 1
      }

      // We know there must be a diff once we got here. Either dataA/dataB is a
      // prefix of the other and index is where the prefix ends, or index is
      // the first difference found. Either way, we can safely use slice() to
      // get at most some number of characters at that index for context.
      val contextA = Misc.remapControlsAndLineEndingsToVisibleGlyphs(dataA.slice(index, index + CHARS_TO_SHOW_AFTER_DIFF))
      val contextB = Misc.remapControlsAndLineEndingsToVisibleGlyphs(dataB.slice(index, index + CHARS_TO_SHOW_AFTER_DIFF))
      val path = zPath + ".charAt(" + (index + 1) + ")"
      Seq((path, contextA, contextB))
    }
  }

  def textIsSame(dataA: String, dataB: String, maybeType: Option[String]): Boolean = {
    maybeType match {
      case Some("xs:hexBinary") => dataA.equalsIgnoreCase(dataB)
      case Some("xs:date") => {
        val a = DFDLDateConversion.fromXMLString(dataA)
        val b = DFDLDateConversion.fromXMLString(dataB)
        a == b
      }
      case Some("xs:time") => {
        val a = DFDLTimeConversion.fromXMLString(dataA)
        val b = DFDLTimeConversion.fromXMLString(dataB)
        a == b
      }
      case Some("xs:dateTime") => {
        val a = DFDLDateTimeConversion.fromXMLString(dataA)
        val b = DFDLDateTimeConversion.fromXMLString(dataB)
        a == b
      }
      case _ => dataA == dataB
    }
  }

  /**
   * for quick tests, we use literal scala nodes. However, the underlying
   * infrastructure wants to be all file centric for diagnostic-message
   * reasons (line numbers for errors)
   */
  def convertNodeToTempFile(xml: Node, tmpDir: File, nameHint: String = "daffodil_tmp_") = {
    // Create temp file
    // note that the prefix has a minimum length of 3.
    val prefix = nameHint.length match {
      case 0 => "daffodil_tmp_"
      case 1 => nameHint + "__"
      case 2 => nameHint + "_"
      case _ => nameHint
    }
    val tmpSchemaFile = File.createTempFile(prefix, ".dfdl.xsd", tmpDir)
    // Delete temp file when program exits
    tmpSchemaFile.deleteOnExit
    //
    // Note: we use our own pretty printer here because
    // Scala library one doesn't preserve/print CDATA properly.
    //
    val pp = new org.apache.daffodil.xml.PrettyPrinter(2)
    val xmlString = pp.format(xml)
    val fos = new java.io.FileOutputStream(tmpSchemaFile)
    val fw = new java.io.OutputStreamWriter(fos, "utf-8")
    fw.write(xmlString)
    fw.close()
    tmpSchemaFile
  }

  def convertInputStreamToTempFile(
    is: java.io.InputStream,
    tmpDir: File,
    nameHint: String,
    suffix: String) = {
    // Create temp file
    // note that the prefix has a minimum length of 3.
    val prefix = nameHint.length match {
      case 0 => "daffodil_tmp_"
      case 1 => nameHint + "__"
      case 2 => nameHint + "_"
      case _ => nameHint
    }
    val tmpSchemaFile = File.createTempFile(prefix, suffix, tmpDir)
    // Delete temp file when program exits
    tmpSchemaFile.deleteOnExit

    val fos = new java.io.FileOutputStream(tmpSchemaFile)
    IOUtils.copy(is, fos)
    fos.close()
    tmpSchemaFile
  }

  /**
   * Strong escaping that never loses information, handles apos and CR right.
   *
   * Escapes apostrophe (single quote) as well as the other XML escaped chars.
   * Remaps CR and any other XML-illegals into PUA. Replaces whitespace with
   * numeric character entities for additional safety.
   *
   * This is needed since XML may be using single quotes to surround a string which
   * might contain single quotes.
   *
   * The reason basic scala.xml.Utility.escape doesn't escape single-quotes is
   * HTML compatibility. HTML doesn't define an "&apos;" entity.
   *
   * Furthermore, since some potentially illegal XML characters may be used here, we
   * are going to remap all the illegal XML characters to their corresponding PUA characters.
   *
   * Lastly, all whitespace chars are replaced by numeric character entities, and
   * anything above 0xFF that is not considered letter or digit, is also replaced
   * by a numeric character entity.
   *
   * The result is a string which can be displayed as an XML attribute value, is
   * invertible back to the original string.
   *
   * Finally, CRLF and CR will come through as &#xE00D;&#xA; that's because
   * if we used &#xD; for the CR, it might be converted to a LF by XML readers.
   * We have to use our own PUA remapping trick if we want to be sure to preserve
   * CR in XML.
   */
  def escape(str: String, sb: StringBuilder = new StringBuilder()): StringBuilder = {
    var i = 0
    while (i < str.length) {
      val x = str(i)
      val c = escapeMapper(x)
      i += 1
      c match {
        case '\'' => sb.append("&#x27;") // don't use "&apos;" because it's not universally accepted (HTML doesn't have it in early versions)
        case '"' => sb.append("&quot;")
        case '&' => sb.append("&amp;")
        case '<' => sb.append("&lt;")
        case '>' => sb.append("&gt;")
        case _ if (c.isLetterOrDigit) => sb.append(c)
        case _ if (c.isWhitespace || c.isControl) => toNumericCharacterEntity(c, sb)
        // A0 is the NBSP character - not considered whitespace, but no glyph, so we need it numeric
        case _ if (c.toInt == 0xA0) => toNumericCharacterEntity(c, sb)
        // Any other char < 256 is punctuation or other glyph char
        case _ if (c.toInt < 0xFF) => sb.append(c)
        case _ => toNumericCharacterEntity(c, sb)
      }
    }
    sb
  }

  private val escapeMapper =
    remapXMLIllegalCharToPUA(
      checkForExistingPUA = false,
      replaceCRWithLF = false) _

  def toNumericCharacterEntity(c: Char, sb: StringBuilder) = {
    val i = c.toInt
    Assert.usage(i > 0) // NUL cannot be represented at all in XML.
    val s = Integer.toHexString(i).toUpperCase()
    sb.append("&#x")
    sb.append(s)
    sb.append(";")
  }
}

trait GetAttributesMixin extends ThrowsSDE {
  def xml: Node

  /**
   * Use to retrieve things that are not format properties.
   */
  def getAttributeRequired(name: String) = {
    getAttributeOption(name) match {
      case None => schemaDefinitionError("The attribute '" + name + "' is required.")
      case Some(s) => s
    }
  }

  /**
   * Use to retrieve things that are not format properties.
   */
  def getAttributeOption(name: String): Option[String] = {
    val attrString = xml.attribute(name).map { _.text }
    attrString
  }

  def getAttributeOption(ns: NS, name: String): Option[String] = {
    //
    // Most annoying, but this doesn't work....
    // val res = xml.attribute(ns.toString, name).map{ _.text }
    val attr = (xml \ ("@{" + ns.toString + "}" + name))
    if (attr.length == 0) None
    else Some(attr.text)
  }

  /**
   * For picking off the short-form annotations.
   */
  def dfdlAttributes(n: Node) = XMLUtils.dfdlAttributes(n)
  def dfdlxAttributes(n: Node) = XMLUtils.dfdlxAttributes(n)
  def dafAttributes(n: Node) = XMLUtils.dafAttributes(n)

}

class QNamePrefixNotInScopeException(pre: String, loc: LookupLocation)
  extends Exception("Prefix " + pre + " not found in scope. Location: " + loc.toString)

// Commented out for now, but we may reactivate this to
// do more validation stuff in the TDMLRunner. So keeping in the
// source like this.
//
//object XMLSchemaUtils {
//  /**
//   * validate a DFDL schema.
//   *
//   * This validates the XML Schema language subset that DFDL uses, and also all the annotations
//   * hung off of it.
//   */
//  def validateDFDLSchema(doc: Node) = {
//    // TODO: should this do something other than throw an exception on a validation error?
//    //
//    // Users will write DFDL Schemas, using the xs or xsd prefix (usually) bound to the XML Schema namespace,
//    // and the dfdl prefix (usually) bound to the DFDL namespace.
//    //
//    // However, we don't want to validate using the XML Schema for XML Schema (which would be the usual interpretation
//    // of validating an XML Schema), instead we want to use the schema for the DFDL Subset of XML Schema.
//    //
//    // So, the hack here, is we're going to textually substitute the URIs, so that the validator doesn't have to be
//    // modified to do this switch, and we don't have to lie in the DFDL Subset schema, and claim it is realizing the
//    // XML Schema URI.
//    //
//    // However, we should consider whether there is a better way to do this involving either (a) lying and having the
//    // DFDL Subset Schema pretend it is the XSD schema, or we can play some catalog tricks perhaps.
//    //
//    // Also, the way this whole thing finds the necessary schemas is a bit daft. It should look in the jar or files,
//    // but it should be using an XML Catalog.
//    //
//    val docstring = doc.toString()
//    val xmlnsURI = "http://www.w3.org/2001/XMLSchema";
//    val xsdSubsetURI = "http://www.w3.org/2001/XMLSchema";
//    val docReplaced = docstring.replaceAll(xmlnsURI, xsdSubsetURI)
//    val docReader = new StringReader(docReplaced)
//    val schemaResource = Misc.getRequiredResource(Validator.dfdlSchemaFileName()).toURI()
//    val res =
//      try {
//        Validator.validateXMLStream(schemaResource, docReader)
//      } catch {
//        case e: ...  => {
//          val exc = e
//          // System.err.println(exc.getMessage())
//          // Really useful place for a breakpoint.
//          throw e
//        }
//      }
//    res
//  }
//}
