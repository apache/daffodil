package daffodil.xml

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
 * User: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu > 
 * Date: 2010
 */
import java.util.ArrayList
import scala.collection.mutable.Queue

import org.jdom.filter.Filter
import org.jdom.Content
import org.jdom.Element
import org.jdom.Namespace
import org.jdom.Parent
import org.jdom.Text


import daffodil.exceptions.UnimplementedException
import java.lang.String

class CompressableElement(name:String) extends Element(name) with CompressableNode {

  private var compressed = false

  private var myContent:ArrayList[Content] = _

  def this(name:String,namespace:Namespace) = {
    this(name)
    setNamespace(namespace)
  }

  def this(name:String,namespace:String) = {
    this(name)
    setNamespace(Namespace getNamespace(namespace))
  }

  //////////////////////////////////
  // ADD

  def addContent(newContent:Iterable[Content]):Element = {
    for(c <- newContent)
      addContent(c.asInstanceOf[Content])
    this
  }

  override def addContent(child:Content):Element = {
    if (compressed)
      decompress
    if (myContent == null)
      myContent = new ArrayList()
    myContent add(child)
    father(child)
//    child match {
//      case n:CompressableNode =>
//        if (compressed){
//          father(child)
//          n setPosition (DBUtil getNumberChildren (this))
//          DBUtil add(this,n)
//        } else {
//          n setPosition (myContent size)
//          myContent add(child)
//          father(child)
//        }
//    }
    this
  }

  def addContent(index:Int,newContent:Iterable[Content]) =
    throw new UnimplementedException("Element.addContent(index,content")

  override def addContent(index:Int,child:Content) = {
    throw new UnimplementedException("Element.addContent(index,content")
  }

  override def addContent(str:String) :Element= {
    addContent(new CompressableText(str))
    this
  }


  //////////////////////////////////
  // GET

  override def getChild(name:String):Element = {
    if(compressed)
      decompress
    if (myContent == null)
      null
    else{
      for(i <- 0 until myContent.size)
        myContent get(i) match { case e:Element => if(e.getName == name) return e; case _ => }
      null
    }
//    if (compressed){
//      throw new IllegalStateException("no child for name in compressed state")
//    } else{
//      for(i <- 0 until myContent.size)
//        myContent get(i) match { case e:Element => if(e.getName == name) return e; case _ => }
//      null
//    }
  }

  override def getChild(name:String,ns:Namespace):Element = {
    if (compressed)
      decompress
    val qName = ns.getURI+name
    if (myContent == null)
      null
    else {
      for(i <- 0 until myContent.size)
        myContent get(i) match { case e:Element => if(e.getNamespaceURI+e.getName == qName) return e; case _ => }
      null
    }
//    if (compressed){
//      throw new IllegalStateException("no child for name in compressed state")
//    } else{
//      val qName = ns.getURI+name
//      for(i <- 0 until myContent.size)
//        myContent get(i) match { case e:Element => if(e.getNamespaceURI+e.getName == qName) return e; case _ => }
//      null
//    }
  }

  override def getChildren:java.util.List[_ <:Content] = {
    if (compressed)
      decompress

    if (myContent == null)
      new ArrayList()
    else{
      var result = new ArrayList[Content](myContent.size)
      //System
      for(i <- 0 until myContent.size)
        myContent get(i) match { case e:Element => result add(e); case _ => }
      result
    }
//
//
//    if (compressed){
//      DBUtil getChildren (this) //TODO remove text elements
//    } else {
//      var result = new ArrayList[Content]
//      for(i <- 0 until myContent.size)
//        myContent get(i) match { case e:Element => result add(e); case _ => }
//      result
//    }
  }

  override def getChildren(name:String):java.util.List[Content] = {
    if (compressed)
      decompress
    var result = new ArrayList[Content]
    if (myContent == null)
      myContent = new ArrayList()
    for(i <- 0 until myContent.size)
      myContent get(i) match { case e:Element => if(e.getName == name) result add(e); case _ => }
    result

//    if (compressed){
//      throw new IllegalStateException("no child for name in compressed state")
//    }else {
//      var result = new ArrayList[Content]
//      for(i <- 0 until myContent.size)
//        myContent get(i) match { case e:Element => if(e.getName == name) result add(e); case _ => }
//      result
//    }
  }

  override def getChildren(name:String,ns:Namespace):java.util.List[Content] = {
    if (compressed)
      decompress
    val qName = ns.getURI+name
    var result = new ArrayList[Content]
    if (myContent == null)
      myContent = new ArrayList()
    for(i <- 0 until myContent.size)
      myContent get(i) match { case e:Element => if(e.getNamespaceURI+e.getName == qName) result add(e); case _ => }
    result

//    if (compressed){
//      throw new IllegalStateException("no child for name in compressed state")
//    }else {
//      val qName = ns.getURI+name
//      var result = new ArrayList[Content]
//      for(i <- 0 until myContent.size)
//        myContent get(i) match { case e:Element => if(e.getNamespaceURI+e.getName == qName) result add(e); case _ => }
//      result
//    }
  }

  override def getChildText(name:String) =
    getChild(name) getText

  override def getChildText(name:String,ns:Namespace) =
    getChild(name,ns) getText

  override def getChildTextNormalize(name:String) =
    getChild(name) getTextNormalize

  override def getChildTextNormalize(name:String,ns:Namespace) =
    getChild(name,ns) getTextNormalize

  override def getChildTextTrim(name:String) =
     getChild(name) getTextTrim

  override def getChildTextTrim(name:String,ns:Namespace) =
    getChild(name,ns) getTextTrim

  override def getContent = {
    if (compressed)
      decompress

    if (myContent == null)
      new ArrayList()
    else
      myContent.clone.asInstanceOf[java.util.List[Content]]
//    val result = new ArrayList[Content]
////
////    if (compressed){
////      val iter = DBUtil getChildrenIterator(this)
////
////      while(iter hasNext)
////	      result add(iter next)
////
////    }else
//    result.addAll(myContent)
//    result
  }


  override def getContent(filter:Filter) = {
    if (compressed)
      decompress
    var result = new ArrayList[Content]
    if (myContent == null)
      myContent = new ArrayList()
    for(i <- 0 until myContent.size)
      if (filter matches(myContent get(i)))
        result add(myContent get(i))

//    if (compressed){
//      val size = DBUtil getNumberChildren (this)
//      val children = DBUtil getChildren (this)
//      for(i <- 0 until size) {
//        val z = children get(i)
//        if (filter matches(z))
//          result add(z)
//      }
//    } else {
//      for(i <- 0 until myContent.size)
//      if (filter matches(myContent get(i)))
//        result add(myContent get(i))
//    }

    result
  }

  override def getContent(index:Int) = {
    if (compressed)
      decompress
    if (myContent == null)
      myContent = new ArrayList()
    myContent get(index)
  }

//    if (compressed)
//      DBUtil getChild (this,index)
//    else
//      myContent get(index)

  override def getContentSize = {
    if (compressed)
      decompress
    if (myContent == null)
      0
    else
      myContent size
  }
//    if (compressed)
//      DBUtil getNumberChildren (this)
//    else
//      myContent size


  override def getDescendants = {
    if (compressed)
      decompress
    if (myContent == null)
      myContent = new ArrayList()
    myContent iterator
  }
//    if (compressed)
//      DBUtil getChildrenIterator (this)
//    else
//      myContent iterator

  override def getDescendants(filter:Filter) =
    getContent(filter) iterator

  override def getText:String = {
    val contentSize = getContentSize
    if (contentSize == 0)
      return "";

    // If we hold only a Text or CDATA, return it directly
    if (contentSize == 1) {
      val obj = getContent(0);
      obj match {
        case o:CompressableText => o.getText
        case _ => ""
      }
    }else{
      // Else build String up
      val textContent = new StringBuffer
      var hasText = false

      for ( i <- 0 until contentSize) {
        val obj = getContent(i);
        if (obj.isInstanceOf[Text]) {
          textContent.append(obj.asInstanceOf[Text].getText());
          hasText = true;
        }
      }

      if (!hasText)
        ""
      else
        textContent.toString()
    }
  }


  /////////////////////////////////
  // SET

  def setContent(newContent:java.util.Collection[Content]) = {
    if (compressed)
      decompress

    if (myContent == null)
      myContent = new ArrayList()
    myContent clear ()
    myContent addAll(newContent)
    this
  }


  override def setAttribute(name: String, value: String) = {
    super.setAttribute(name,value)
//    if (isCompressed){
//      if (name=="type")
//        DBUtil setType(this,value)
//      else
//        throw new IllegalStateException("Can't set attributes in compressable nodes")
//    }
    this
  }

  override def setText(text:String) = {
//    if (isCompressed){
//      println("Setting text for already compressed node "+this.getID)
//      println("text = "+text)
//    }
    setContent(new CompressableText(text))
    
  }


  override def setContent(child:Content) = {
    removeContent
    addContent (child)
    this
  }

  def setContent(index:Int,newContent:java.util.Collection[Content]) = {
    if (compressed)
      decompress

    if (myContent == null)
      myContent = new ArrayList()

    myContent remove(index)
    myContent addAll(index,newContent)
    val it = newContent iterator;
    var pos = index
    while(it.hasNext)  {
      val child = it.next
      child match {
        case n:CompressableNode =>
          father(n)
          n setPosition pos
          pos += 1
      }
    }
    this
  }


  override def setContent(index:Int,child:Content) = {
    if (compressed)
      decompress

    if (myContent == null)
      myContent = new ArrayList()
    
    myContent set(index,child)
    child match {
      case node:CompressableNode =>
        father(node)
        node setPosition index
    }
    this
  }

  /////////////////////////////////////
  // REMOVE

  override def removeChild(name:String) ={
    if (compressed)
      decompress
    val child = getChild(name)
    if (child!=null && myContent!=null){
      myContent remove(child)
      true
    }else
      false
  }

  override def removeChild(name:String,ns:Namespace) = {
    if (compressed)
      decompress
    val child = getChild(name,ns)
    if (child!=null && myContent!=null){
      myContent remove(child)
      true
    }else
      false
  }

  override def removeChildren(name:String) = {
     if (compressed)
      decompress
    val children = getChildren(name);
    if (myContent != null)
      for(i <- 0 until children.size)
        myContent remove (children get(i))
    children.size > 0
  }

  override def removeChildren(name:String,ns:Namespace) = {
    if (compressed)
      decompress
    val children = getChildren(name,ns);
    if (myContent != null)
      for(i <- 0 until children.size)
        myContent remove (children get(i))
    children.size > 0
  }

  override def removeContent() = {
    val list = new ArrayList[Content]()
    if (compressed)
      decompress
    if (myContent != null)
      list addAll(myContent)
    myContent = null
    list
  }

  override def removeContent(child:Content) = {
    if (compressed)
      decompress
    if (myContent == null)
      false;
    else {
      val index = myContent indexOf (child)
      myContent remove(index)
  //    for(i <- index until myContent.size)
  //      myContent.get(i).asInstanceOf[CompressableNode].setPosition (i)
      true
    }
  }

  override def removeContent(filter:Filter) = {
    var children = getContent(filter)
    for(i <- 0 until children.size)
      removeContent(children get(i))
    children
  }

  override def removeContent(index:Int) = {
    if (compressed)
      decompress

    val ele = myContent remove(index)

    if (myContent.size == 0)
      myContent = null
//    for(i <- index until myContent.size)
//      myContent.get(i).asInstanceOf[CompressableNode].setPosition (i)
    ele
  }



  //MISC

  override def setImmediateAncestor(parent:Parent) = setParent(parent)

  def father(content:Content) = content match {
    case c:CompressableText => c setImmediateAncestor(this)
    case c:CompressableElement => c setImmediateAncestor(this)
  }

  override def isCompressed = compressed

  override def compress = {
    //println("Compressing "+getName)
    if (!compressed){
      id = DBUtil store(this)
      compressed = true
    }
    myContent = null
    //myContent = new ArrayList(0)
    

    if (getID<0)
      throw new AssertionError("Compressed but got no id")

//    if (!compressed){
//      getParent match {
//        case e:CompressableNode =>
//          id = if (e isCompressed){
//            setParent(null)
//            DBUtil store(this,e getID,nodePosition)
//          }else
//            DBUtil store(this,-1,nodePosition)
//        case _ => id = DBUtil store(this,-1,0)
//      }
//      compressed = true
//      myContent clear;
//    }
  }
  
  override def decompress = {
    compressed = false
    DBUtil restore this
  }

//  override def setCompressed(compressed:Boolean) = this.compressed = compressed

}
