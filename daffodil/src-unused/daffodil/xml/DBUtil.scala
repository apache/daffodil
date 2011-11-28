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
 * Created By: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu > 
 * Date: 2010
 */

import scala.collection.JavaConversions.asBuffer

import org.jdom.Content
import org.jdom.Namespace

import java.sql.Connection
import java.sql.DriverManager
import java.io.StringReader
import org.jdom.input.SAXBuilder

/**
 * Utility object to partially and temporally serialize a DOM tree to a database.
 *
 * The database is stored at a temporal file and live for as long as the java virtual machine running this instance
 * of the object.
 *
 *
 * @version 1
 * @author Alejandro Rodriguez
 */
object DBUtil {

  private val CREATE_ELEMENT = """CREATE TABLE ELEMENT (ID INT,
                                  URI VARCHAR(1024), PREF VARCHAR(1024), NAME VARCHAR(1024) NOT NULL,
                                  TYPE VARCHAR(1024), NUMBER_CHILDREN INT DEFAULT 0,
                                  PARENT_OF_TEXT SMALLINT DEFAULT 0, PARENT INT, POSITION INT,
                                  PRIMARY KEY(ID))"""

  private val CREATE_ELEMENT_INDEX = """CREATE INDEX PARENT_INDEX ON ELEMENT (PARENT)"""

  private val CREATE_TEXT = """CREATE TABLE TEXT (PARENT INT NOT NULL PRIMARY KEY, TEXT VARCHAR(1024))"""

  private val CREATE_XML = """CREATE TABLE DOM_XML (ID INT, TEXT CLOB NOT NULL, PRIMARY KEY(ID))"""



  private val INSERT_ELEMENT = """INSERT INTO ELEMENT(ID,URI,PREF,NAME,TYPE,NUMBER_CHILDREN,PARENT_OF_TEXT,
                                  PARENT,POSITION)
                                   VALUES (?,?,?,?,?,?,?,?,?)"""

  private val INSERT_TEXT = """INSERT INTO TEXT(PARENT,TEXT) VALUES(?,?)"""

  private val INSERT_XML = """INSERT INTO DOM_XML(ID,TEXT) VALUES(?,?)"""



  private val SET_XML = """UPDATE DOM_XML SET TEXT = ? WHERE ID = ?"""

  private val SET_TEXT_CHILD = """UPDATE ELEMENT SET NUMBER_CHILDREN = 1, PARENT_OF_TEXT = 1 WHERE ID = ?"""

  private val INCREASE_CHILDREN = """UPDATE ELEMENT SET NUMBER_CHILDREN = NUMBER_CHILDREN+1 WHERE ID = ?"""

  private val UPDATE_TYPE = """UPDATE ELEMENT SET TYPE = ? WHERE ID = ?"""

  private val DECREASE_CHILDREN = """UPDATE ELEMENT SET NUMBER_CHILDREN = NUMBER_CHILDREN - 1,
                                     PARENT_OF_TEXT = 0 WHERE ID = ?"""

  private val DELETE_TEXT = """DELETE FROM TEXT WHERE PARENT = ?"""

  private val DELETE_ELEMENT = """DELETE FROM ELEMENT WHERE ID = ?"""

  private val UPDATE_POSITIONS = """UPDATE ELEMENT SET POSITION = POSITION - 1 WHERE PARENT = ? AND POSITION > ?"""

  private val CHILDREN_TO_0 = """UPDATE ELEMENT SET NUMBER_CHILDREN = 0, PARENT_OF_TEXT = 0 WHERE ID = ?"""

  private val DELETE_ALL_CHILDREN = """DELETE FROM ELEMENT WHERE PARENT = ?"""



  private val NUMBER_CHILDREN = """SELECT NUMBER_CHILDREN, PARENT_OF_TEXT FROM ELEMENT WHERE ID = ? """

  private val TEXT_INFO = """SELECT TEXT FROM TEXT WHERE PARENT = ?"""

  private val NODE_INFO = """SELECT ID, URI, PREF, NAME, TYPE FROM ELEMENT WHERE PARENT = ? AND POSITION = ?"""

  private val POSITION = """SELECT POSITION FROM ELEMENT WHERE PARENT = ? AND ID = ?"""

  private val ALL_CHILDREN = """SELECT ID, URI, PREF, NAME, TYPE, POSITION FROM ELEMENT WHERE PARENT = ? ORDER BY POSITION"""

  private val GET_XML = """SELECT TEXT FROM DOM_XML WHERE ID = ?"""



  private val protocol = "jdbc:derby:"

  private var connection:Connection = _

  private lazy val insertElement = getConnection.prepareStatement(INSERT_ELEMENT)

  private lazy val insertText = getConnection.prepareStatement(INSERT_TEXT)

  private lazy val increaseChildren = getConnection.prepareStatement(INCREASE_CHILDREN)

  private lazy val setTextChild = getConnection.prepareStatement(SET_TEXT_CHILD)

  private lazy val numberChildren = getConnection.prepareStatement(NUMBER_CHILDREN)

  private lazy val textInfo = getConnection.prepareStatement(TEXT_INFO)

  private lazy val nodeInfo = getConnection.prepareStatement(NODE_INFO)

  private lazy val decreaseChildren = getConnection.prepareStatement(DECREASE_CHILDREN)

  private lazy val deleteText = getConnection.prepareStatement(DELETE_TEXT)

  private lazy val position = getConnection.prepareStatement(POSITION)

  private lazy val deleteElement = getConnection.prepareStatement(DELETE_ELEMENT)

  private lazy val updatePositions = getConnection.prepareStatement(UPDATE_POSITIONS)

  private lazy val childrenTo0 = getConnection.prepareStatement(CHILDREN_TO_0)

  private lazy val deleteAllChildren = getConnection.prepareStatement (DELETE_ALL_CHILDREN)

  private lazy val allChildren = getConnection.prepareStatement (ALL_CHILDREN)

  private lazy val updateType = getConnection.prepareStatement (UPDATE_TYPE)

  private lazy val getXML = getConnection.prepareStatement(GET_XML)

  private lazy val insertXML = getConnection.prepareStatement(INSERT_XML)

  private lazy val setXML = getConnection.prepareStatement(SET_XML)

  private var nextID = 0

  private var lastRestoreNode:CompressableNode = _

  private var printing = false



  private def getConnection = {
    if (connection == null){
      Class.forName("org.apache.derby.jdbc.EmbeddedDriver")
      val tempFile = java.io.File.createTempFile("daffodil","DB")
      val path = tempFile.getAbsolutePath
      tempFile.delete

      connection = DriverManager.getConnection(protocol + path+";create=true", null);
      init
    }
    connection
  }


  private def init = {
    val statement = connection.createStatement;
    statement execute(CREATE_ELEMENT)
    statement execute(CREATE_TEXT)
    statement execute(CREATE_ELEMENT_INDEX)
    statement execute(CREATE_XML)
    statement.close
  }

  /**
   * Serializes a node to the database
   */
  def store(node:CompressableNode):Int =
    node match {
      case t:CompressableText =>
        if (node.getID>=0)
          node.getID
        else{
          val id = nextID
          nextID += 1
          id
        }
      case e:CompressableElement =>
        val reader = new StringReader(XMLUtil serialize(e))
        if (node.getID>=0){
          setXML setClob(1,reader)
          setXML setInt(2,e getID)
          setXML execute;
          e.getID
        }else{
          val id = nextID
          nextID += 1

          insertXML setInt(1,id)
          insertXML setClob(2,reader)
          insertXML execute;
          id
        }
    }


  /**
   * Restores a node content from the database
   */
  def restore(node:CompressableNode):Unit = {

    if (node != lastRestoreNode && lastRestoreNode != null)
      if (printing){
          lastRestoreNode match {
            case e:CompressableElement => e removeContent
            case _ =>
          }
      }
      else
        lastRestoreNode compress;

    node match {
      case t:CompressableText =>
      case e:CompressableElement =>
        getXML setInt (1,node getID)
        val rs = getXML executeQuery ;
        if (!rs.next)
          throw new AssertionError("No element found with id "+node.getID)

        val builder = new SAXBuilder()
        builder setFactory (new JDOMFactory)
        val ele = builder.build(rs.getClob(1).getCharacterStream).getRootElement

        e.removeContent
//        if (ele.getText.length>0)
//          e.setText(ele getText)
//        else
        for(child <- ele.removeContent){
//          ele.removeContent(child)
          child match {
//            case x:Content => e addContent(x)
//          }
            case x:CompressableNode => e addContent(x)
          }
//          e addContent(child)
          //e addContent (ele.getContent)
        }
        lastRestoreNode = e
    }
  }


  def store(node:CompressableNode,parent:Int,position:Int): Int =
    node match {
      case t:CompressableText => storeText(t,parent)
      case e:CompressableElement => storeElement(e,parent,position)
    }


  def add(parent:CompressableNode,child:CompressableNode):Unit = {
    child match {
      case _:CompressableText => setTextChild setInt(1,parent getID); setTextChild executeUpdate
      case _:CompressableElement => increaseChildren setInt (1,parent getID); increaseChildren execute      
    }    
    child compress
  }

  def removeAll(parent:CompressableElement):Unit = {
    childrenTo0 setInt(1,parent getID)
    childrenTo0 executeUpdate;
    deleteAllChildren setInt(1,parent getID)
    deleteAllChildren executeUpdate;
    deleteText setInt(1,parent getID)
    deleteText executeUpdate
  }

  def remove(parent:CompressableElement,child:CompressableNode):Unit = {
    
    child match {
      case _:CompressableElement =>
        position setInt (1, parent getID)
        position setInt(2,child getID)
        val rs = position executeQuery;

        if(rs next){
          val pos = rs getInt(1)

          deleteElement setInt(1,child getID)
          deleteElement executeUpdate

          updatePositions setInt(1,parent getID)
          updatePositions setInt(2,pos)

          updatePositions executeUpdate

          decreaseChildren setInt(1,parent getID)
          decreaseChildren executeUpdate
	}
      case _:CompressableText => 
	deleteText setInt(1,child getID); 
        deleteText executeUpdate; 
        decreaseChildren setInt(1,parent getID)
        decreaseChildren executeUpdate
    }
    
  }

  def getChildren(parent:CompressableNode):java.util.List[CompressableNode] = {
    parent match {
      case t:CompressableText => java.util.Collections.emptyList[CompressableNode]
      case e:CompressableElement => new CompressedList(e)
    }
  }

  def setType(node:CompressableNode,value:String) = {
    updateType setString(1,value)
    updateType setInt(2, node getID)
    updateType executeUpdate
  }

  private def storeElement(node:CompressableElement,parent:Int,position:Int):Int = {
    //println("storing Element "+node.getName+"["+node.getText+"]")

    val storeText = node.getText.length>0

    insertElement setInt(1,nextID)
    insertElement setString(2,node.getNamespaceURI)
    insertElement setString(3,node.getNamespacePrefix)
    insertElement setString(4,node.getName)
    insertElement setString(5,node.getAttributeValue("type"))
    insertElement setInt(6,if (storeText) 1 else node.getChildren.size)
    insertElement setShort(7, if (storeText) 1 else 0)
    insertElement setInt(8,parent)
    insertElement setInt(9,position)

    insertElement executeUpdate;
    //insertElement clearParameters;
    val id = nextID
    nextID += 1

    if (storeText){
      val text = new CompressableText(node getText)
      store(text,id,0)
    }else{
      val children:java.util.List[_ <: Content] = node getChildren;
      for(i <- 0 until children.size)
        store(children.get(i).asInstanceOf[CompressableNode],id,i)
    }
    
    id
  }

  private def storeText(node:CompressableText,parent:Int):Int = {
    //println("storing Text "+node.getText)

    insertText setInt(1,parent)
    insertText setString(2,node getText)

    insertText executeUpdate

    insertText clearParameters;
    -1
  }

//  def getChild(node:CompressableElement,index:Int):CompressableNode = {
//
//    numberChildren setInt(1,node getID)
//    val rs = numberChildren executeQuery;
//    if (rs next){
//      val nChildren = rs getInt(1)
//      val parentOfText = rs getShort(2)
//
//      if (index<nChildren)
//	if (parentOfText>0){
//          textInfo setInt(1,node getID)
//          val rs = textInfo executeQuery;
//	  if (rs next){
//            val text = rs getString(1)
//            val child = new CompressableText(text)
//            child setImmediateAncestor(node)
//            child setCompressed(true)
//
//            textInfo clearParameters;
//            child
//	  }else{
//	    //DBUtil printAll;
//	    println("Looking for text child in "+node.getID)
//	    //println("It has this text '"+node.getText+"'")
//	    throw new IllegalStateException("Looking for "+index+"th child of "+node+" with "+nChildren+". It doesn't have a text child")
//	  }
//	}else{
//          nodeInfo setInt (1,node getID)
//          nodeInfo setInt(2,index)
//          val rs = nodeInfo executeQuery;
//	        rs next
//          val id = rs getInt(1)
//          val uri = rs getString(2)
//          val pref = rs getString(3)
//          val name = rs getString(4)
//          val typeName = rs getString(5)
//
//          val child = new CompressableElement(name)
//          if (typeName!=null)
//            child setAttribute ("type",typeName)
//          child setNamespace (Namespace.getNamespace(pref,uri))
//
//          child setImmediateAncestor(node)
//          child setCompressed(true)
//          child setID(id)
//          nodeInfo clearParameters;
//          child
//	}
//	else
//	  throw new IndexOutOfBoundsException(index + ">=" + nChildren)
//    }else
//      throw new IllegalStateException("node not found")
//  }

  def getNumberChildren(parent:CompressableElement) = {
    numberChildren setInt(1,parent getID)
    val rs = numberChildren executeQuery;
    numberChildren clearParameters;
    rs next;
    rs getInt(1)
  }


//  def getChildrenIterator(parent:CompressableElement):java.util.Iterator[CompressableNode] = {
//
//    if(parent.getID == -1){
//      return new java.util.Iterator[CompressableNode](){
//
//        override def hasNext = false
//
//        override def next = throw new IndexOutOfBoundsException
//
//        override def remove = throw new UnsupportedOperationException
//      }
//    }
//
//
//    numberChildren setInt(1,parent getID)
//    val rs1 = numberChildren executeQuery
//
//    rs1 next;
//
//    val nChildren = rs1 getInt(1)
//    val parentOfText = rs1 getShort(2)
//
//    if (parentOfText > 0){
//      textInfo setInt(1,parent getID)
//      val rs = textInfo executeQuery
//
//      new java.util.Iterator[CompressableNode](){
//        var hs = rs next
//
//        override def hasNext = hs
//
//        override def next = {
//          val text = rs getString(1)
//          val child = new CompressableText(text)
//          child setImmediateAncestor(parent)
//          child setCompressed(true)
//
//          hs = rs next;
//          child
//        }
//
//        override def remove = throw new UnsupportedOperationException
//      }
//    }else{
//      allChildren setInt (1,parent getID)
//      val rs = allChildren executeQuery
//
//      new java.util.Iterator[CompressableNode](){
//        var hs = rs next
//
//        override def hasNext = hs
//
//        override def next = {
//          val id = rs.getInt(1)
//          val uri = rs getString(2)
//          val prefix = rs getString(3)
//          val name = rs getString(4)
//          val typeName = rs getString(5)
//          val position = rs getInt(6)
//
//          val child = new CompressableElement(name)
//          if (typeName!=null)
//            child setAttribute ("type",typeName)
//          child setNamespace (Namespace.getNamespace(prefix,uri))
//
//          child setImmediateAncestor(parent)
//          child setCompressed(true)
//          child setID(id)
//          hs = rs next;
//
//          child
//        }
//
//        override def remove = throw new UnsupportedOperationException
//      }
//    }
//  }

  def printAll = {
    val rs = getConnection.createStatement.executeQuery("SELECT ID, PARENT, NAME FROM ELEMENT")
    
    println("ELEMENTS")
    while (rs next){
     println(rs.getInt(1)+" "+rs. getInt(2)+" "+rs.getString(3))
    }
    
    val rs2 = getConnection.createStatement.executeQuery("SELECT PARENT, TEXT FROM TEXT")
    
    println("TEXTS")
    while (rs2 next){
     println(rs2.getInt(1)+" "+rs2.getString(2))
    }
  }

  def printingPhase(b:Boolean) = printing = b

}
