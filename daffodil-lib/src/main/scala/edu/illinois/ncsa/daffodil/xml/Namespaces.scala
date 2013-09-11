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

  // TODO: equality test for NS can do pointer comparison
  // because we're registering these things.

  private val nsCache = scala.collection.mutable.Map[String, NS]()

  def apply(nsString: String) = {
    // null means NoNamespace. So does ""
    val s = if (nsString == null) "" else nsString
    val haveIt = nsCache.get(s)
    haveIt match {
      case Some(ns) => ns
      case None => {
        val newNS = new NS(s)
        nsCache.put(s, newNS)
        newNS
      }
    }
  }

  nsCache.put("", NoNamespace)
}

object NoNamespace extends NS("") {
  override def isNoNamespace = true
  override def toString = "No_Namespace"
  override def toStringOrNullIfNoNS: String = null // most places in Java APIs, no namespace is represented by null.
}

class NS protected (s: String) { // protected constructor. Must use factory.
  override def toString = s
  def toStringOrNullIfNoNS = s
  lazy val uri = new URI(s)
  def isNoNamespace = false
  override def hashCode() = s.hashCode()
  lazy val toJDOM = org.jdom2.Namespace.getNamespace(s)
}

/**
 * Copyright (c) 2010 NCSA.  All rights reserved.
 * Developed by: NCSA Cyberenvironments and Technologies
 *               University of Illinois at Urbana-Champaign
 *               http://cet.ncsa.uiuc.edu/
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal with the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the names of NCSA, University of Illinois, nor the names of its
 *     contributors may be used to endorse or promote products derived from this
 *     Software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * WITH THE SOFTWARE.
 *
 */

/*
 * Created By: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu >
 * Date: 2010
 */
//import java.util.Iterator
//import javax.xml.XMLConstants
//import javax.xml.namespace.NamespaceContext
//import scala.collection.mutable.Map
//
//import org.jdom2.Element
//import org.jdom2.Namespace
//
//// import edu.illinois.ncsa.daffodil.exceptions.UnimplementedException
//import edu.illinois.ncsa.daffodil.schema._
//import java.io.{ ObjectInputStream, ObjectOutputStream, Serializable }
//import edu.illinois.ncsa.daffodil.exceptions.Assert

//@SerialVersionUID(1)
//class Namespaces extends NamespaceContext with Serializable with Diffable {
//
//
//  
//  @transient
//  var prefixes = Map[String,Namespace]()
//  
//  @transient
//  var uris = Map[String,Namespace]()
//
//  private var namespaces:List[(String,String)] = Nil
//
//
//  def getNamespaceByURI(uri:String):Option[Namespace] =
//    uris get(uri)
//
//  def getNamespaceByPrefix(prefix:String):Option[Namespace] =
//    prefixes get(prefix)
//
//  def addNamespace(uri:String,prefix:String):Namespace = {
//    val namespace = Namespace getNamespace(prefix,uri)
//    prefixes(namespace getPrefix) = namespace
//    uris(uri) = namespace
//    //namespaces = (prefix,uri) :: namespaces
//    namespace
//  }
//
//  def addNamespace(namespace:Namespace) = {
//    prefixes(namespace getPrefix) = namespace
//    uris(namespace getURI) = namespace
//    //namespaces = (namespace getPrefix,namespace getURI) :: namespaces
//  }
//
//  def addNamespaces(namespaces:java.util.List[Namespace]) = {
//    for(i <- 0 until namespaces.size)
//      addNamespace(namespaces.get(i))
//  }
//
//  def addNamespaces(node:Element):Unit = {
//    addNamespace(node getNamespace)
//    addNamespaces(node.getAdditionalNamespaces.asInstanceOf[java.util.List[Namespace]])
//    node getParent match {
//      case e:Element => addNamespaces(e)
//      case _ =>
//    }
//  }
//
//  def getNotNullNamespaces = prefixes.values.filter { _.getPrefix != "" }
//
//  override def getNamespaceURI(prefix:String):String = {
//    prefix match {
//      case XMLConstants.DEFAULT_NS_PREFIX => getU(prefix)
//      case XMLConstants.XML_NS_PREFIX => XMLConstants.XML_NS_URI
//      case XMLConstants.XMLNS_ATTRIBUTE => XMLConstants.XMLNS_ATTRIBUTE_NS_URI
//      case null => throw new IllegalArgumentException("null prefix")
//      case p:String => getU(p)
//    }
//  }
//
//  private def getU(prefix:String):String =
//    prefixes get(prefix) match {
//      case Some(s) =>  s.getURI
//      case None => XMLConstants.NULL_NS_URI
//    }
//
//  private def getP(uri:String):String =
//    uris get(uri) match {
//      case Some(s) => s.getPrefix
//      case None => null
//    }
//
//  override def getPrefix(namespaceURI:String):String = {
//    namespaceURI match {
//      case XMLConstants.XML_NS_URI => XMLConstants.XML_NS_PREFIX
//      case XMLConstants.XMLNS_ATTRIBUTE_NS_URI => XMLConstants.XMLNS_ATTRIBUTE
//      case null => throw new IllegalArgumentException("null uri")
//      case u:String => getP(u)
//    }
//  }
//
//  override def getPrefixes(namespaceURI:String):Iterator[String] = {
//    new Iterator[String]{
//      var item = 0
//      override def hasNext = item==0
//      override def next = {
//        item += 1
//        getPrefix(namespaceURI)
//      }
//      override def remove = Assert.notYetImplemented() // throw new UnimplementedException("remove")
//    }
//  }
//
//  private def writeObject(out:ObjectOutputStream):Unit = {
//    namespaces = (for((_,namespace) <- prefixes) yield (namespace getPrefix,namespace getURI)).toList
//    out.defaultWriteObject
//  }
//
//  private def readObject(in:ObjectInputStream):Unit = {
//    in.defaultReadObject
//    prefixes = Map[String,Namespace]()
//    uris = Map[String,Namespace]()
//    val tempNamespaces = namespaces
//    namespaces = Nil
//    for((prefix,uri) <- tempNamespaces)
//      addNamespace(uri,prefix)    
//  }
//  
//  override def diff(o:Any) : Similarity = {
//    o match {
//      case x:Namespaces => {
//        for (entry <- namespaces) {
//         if (!x.namespaces.contains(entry)) return Different(entry, null) 
//        }
//        for (entry <- x.namespaces) {
//          if (!namespaces.contains(entry)) return Different(null, entry)
//        }
//        Same
//      }
//      case _ => DifferentType
//    }
//  }
//  
//}
