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

package org.apache.daffodil.processor.tdml

import java.lang.{ Boolean => JBoolean }
import java.net.URI
import java.net.URISyntaxException
import scala.jdk.CollectionConverters._

import org.apache.daffodil.api.infoset.Infoset.InfosetInputterEventType
import org.apache.daffodil.api.infoset.{ InfosetInputter => JInfosetInputter }
import org.apache.daffodil.lib.util.Misc
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.infoset.JsonInfosetInputter
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.runtime1.infoset.XMLTextInfoset
import org.apache.daffodil.tdml.TDMLException

class TDMLInfosetInputter(
  val scalaInputter: ScalaXMLInfosetInputter,
  others: Seq[JInfosetInputter]
) extends JInfosetInputter {

  private def implString: String = "daffodil"

  override def getEventType(): InfosetInputterEventType = {
    val res = scalaInputter.getEventType()
    if (!others.forall(_.getEventType() == res))
      throw TDMLException("getEventType does not match", Some(implString))
    res
  }

  override def getLocalName(): String = {
    val res = scalaInputter.getLocalName()
    if (!others.forall(_.getLocalName() == res))
      throw TDMLException("getLocalName does not match", Some(implString))
    res
  }

  override def getNamespaceURI(): String = {
    val res = scalaInputter.getNamespaceURI()
    val resIsEmpty = res == null || res == ""
    val othersMatch = others.forall { i =>
      if (!i.getSupportsNamespaces) {
        true
      } else {
        val ns = i.getNamespaceURI()
        val nsIsEmpty = ns == null || ns == ""
        // some inputters return null for no namespace, some return empty
        // string, we consider those the same
        ns == res || (resIsEmpty && nsIsEmpty)
      }
    }
    if (!othersMatch)
      throw TDMLException("getNamespaceURI does not match", Some(implString))
    res
  }

  override def getSimpleText(primType: NodeInfo.Kind): String =
    scalaInputter.getSimpleText(
      primType,
      Map[String, String](XMLTextInfoset.stringAsXml -> "false").asJava
    )

  override def getSimpleText(
    primType: NodeInfo.Kind,
    runtimeProperties: java.util.Map[String, String]
  ): String = {
    val res = scalaInputter.getSimpleText(primType, runtimeProperties)
    val resIsEmpty = res == null || res == ""
    val otherStrings = others.map { i =>
      // Note in an unparserTestCase, there are no others (infoset inputters), because the input infoset is
      // coming from the TDML file, which is already XML.
      // Rather, this is used in a parserTestCase where after populating a TDMLInfosetOutputter
      // which contains every kind of infoset (jdom, dom, JSON, etc.),
      // the toInfosetInputter is called, which creates this kind of infoset inputter where this
      // method has each kind of infoset inputter (json, dom, JSON, etc.) so that it can verify they're
      // all equivalent.
      val firstVersion = i.getSimpleText(primType, runtimeProperties)
      val finalVersion = i match {
        case _ if (firstVersion eq null) => ""
        case jsonii: JsonInfosetInputter =>
          convertJSONInfosetStringToXMLEquivalent(firstVersion)
        case _ => firstVersion
      }
      finalVersion
    }
    val othersmatch = otherStrings.forall { case st: String =>
      val stIsEmpty = st == null || st == ""
      val areSame = res == st || (resIsEmpty && stIsEmpty)
      areSame
    }

    if (!othersmatch)
      throw TDMLException("getSimpleText does not match", Some(implString))

    if (primType.isInstanceOf[NodeInfo.AnyURI.Kind]) {
      try {
        val uri = new URI(res)
        if (!uri.getPath.startsWith("/")) {
          // TDML files must allow blob URI's to be relative, but Daffodil
          // requires them to be absolute with a scheme. So search for the file
          // using TDML semantics and convert to an absolute URI
          val abs = Misc.searchResourceOption(uri.getPath, None)
          if (abs.isEmpty) {
            throw TDMLException("Unable to find URI: " + res, Some(implString))
          }
          abs.get.toString
        } else {
          res
        }
      } catch {
        case uri: URISyntaxException => res
      }
    } else {
      res
    }
  }

  override def isNilled(): Option[JBoolean] = {
    val res = scalaInputter.isNilled()
    if (!others.forall(_.isNilled() == res))
      throw TDMLException("isNilled does not match", Some(implString))
    res
  }

  override def hasNext(): Boolean = {
    val res = scalaInputter.hasNext()
    if (!others.forall(_.hasNext() == res))
      throw TDMLException("hasNext does not match", Some(implString))
    res
  }

  override def next(): Unit = {
    scalaInputter.next()
    others.foreach(_.next())
  }

  override def fini(): Unit = {
    scalaInputter.fini()
    others.foreach(_.fini())
  }

  override def getSupportsNamespaces = true

  /**
   * Converts a JSON infoset string to its XML equivalent.
   *
   * This enables comparing a string parsed by DFDL into a JSON infoset string with the same string
   * parsed by DFDL into an XML infoset string.
   *
   * Unlike XML, JSON preserves CRLF (Carriage Return Line Feed) and CR (Carriage Return)
   * characters, and every Unicode character without the need for PUA (Private Use Area) remapping.
   * If the input string is in JSON format, this method converts CRLF and CR characters to LF (Line Feed).
   * Additionally, the JSON infoset inputter maintains Unicode PUA characters, while the XML infoset inputters remap
   * some characters from PUA back to XML-illegal characters. For consistent comparison with other XML infoset inputters,
   * this method remaps the characters as if they were XML.
   *
   * @param jsonString the JSON infoset string to be converted
   * @return the XML equivalent of the given JSON infoset string
   */
  private def convertJSONInfosetStringToXMLEquivalent(jsonString: String) = {
    val withLFString =
      jsonString.replaceAll("(\r\n|\r)", "\n") // because parsing into JSON didn't do this.
    val xmlString = XMLUtils.remapPUAToXMLIllegalCharacters(withLFString)
    xmlString
  }
}
