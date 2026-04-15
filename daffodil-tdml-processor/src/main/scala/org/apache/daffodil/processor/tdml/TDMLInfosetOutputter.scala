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

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import scala.xml.Node

import org.apache.daffodil.api
import org.apache.daffodil.runtime1.infoset.JDOMInfosetInputter
import org.apache.daffodil.runtime1.infoset.JDOMInfosetOutputter
import org.apache.daffodil.runtime1.infoset.JsonInfosetInputter
import org.apache.daffodil.runtime1.infoset.JsonInfosetOutputter
import org.apache.daffodil.runtime1.infoset.NullInfosetInputter
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetOutputter
import org.apache.daffodil.runtime1.infoset.TeeInfosetOutputter
import org.apache.daffodil.runtime1.infoset.W3CDOMInfosetInputter
import org.apache.daffodil.runtime1.infoset.W3CDOMInfosetOutputter
import org.apache.daffodil.runtime1.infoset.XMLTextInfosetInputter
import org.apache.daffodil.runtime1.infoset.XMLTextInfosetOutputter

object TDMLInfosetOutputterXML {
  def apply(): TDMLInfosetOutputterXML = {
    val baos = new ByteArrayOutputStream()
    val xmlOut = new XMLTextInfosetOutputter(baos, false)
    xmlOut.setIncludeDataType(true)
    new TDMLInfosetOutputterXML(baos, xmlOut)
  }
}

class TDMLInfosetOutputterXML(
  override val xmlStream: ByteArrayOutputStream,
  xmlOut: XMLTextInfosetOutputter
) extends TeeInfosetOutputter(Seq(xmlOut)*)
  with TDMLInfosetOutputter {

  override def getResult: Node =
    scala.xml.XML.load(new ByteArrayInputStream(xmlStream.toByteArray))

  override def toInfosetInputter: TDMLInfosetInputter = {
    val xmlIn = new XMLTextInfosetInputter(new ByteArrayInputStream(xmlStream.toByteArray))
    new TDMLInfosetInputter(xmlIn, Seq())
  }
}

class TDMLInfosetOutputterAll(
  jsonStream: ByteArrayOutputStream,
  override val xmlStream: ByteArrayOutputStream,
  scalaOut: ScalaXMLInfosetOutputter,
  jdomOut: JDOMInfosetOutputter,
  w3cdomOut: W3CDOMInfosetOutputter,
  jsonOut: JsonInfosetOutputter,
  xmlOut: XMLTextInfosetOutputter
) extends TeeInfosetOutputter(Seq(xmlOut, scalaOut, jdomOut, w3cdomOut, jsonOut)*)
  with TDMLInfosetOutputter {

  def getScalaResult: Node = scalaOut.getResult()
  override def getResult: Node =
    scala.xml.XML.load(new ByteArrayInputStream(xmlStream.toByteArray))

  override def toInfosetInputter: TDMLInfosetInputter = {
    val scalaIn = new ScalaXMLInfosetInputter(scalaOut.getResult())
    val jdomIn = new JDOMInfosetInputter(jdomOut.getResult())
    val w3cdomIn = new W3CDOMInfosetInputter(w3cdomOut.getResult())
    val jsonIn = new JsonInfosetInputter(new ByteArrayInputStream(jsonStream.toByteArray))
    val xmlIn = new XMLTextInfosetInputter(new ByteArrayInputStream(xmlStream.toByteArray))
    val nullIn = {
      val events = NullInfosetInputter.toEvents(
        new ByteArrayInputStream(
          scalaOut.getResult().toString().getBytes("UTF-8")
        )
      )
      new NullInfosetInputter(events)
    }
    new TDMLInfosetInputter(xmlIn, Seq(jdomIn, w3cdomIn, jsonIn, scalaIn, nullIn))
  }
}

object TDMLInfosetOutputterAll {
  def apply(): TDMLInfosetOutputterAll = {
    val jsonStream = new ByteArrayOutputStream()
    val xmlStream = new ByteArrayOutputStream()

    val scalaOut = new ScalaXMLInfosetOutputter()
    val jdomOut = new JDOMInfosetOutputter()
    val w3cdomOut = new W3CDOMInfosetOutputter()
    val jsonOut = new JsonInfosetOutputter(jsonStream, false)
    val xmlOut = new XMLTextInfosetOutputter(xmlStream, false)

    Seq(jdomOut, w3cdomOut, jsonOut, xmlOut).foreach { out =>
      out.setIncludeDataType(true)
    }

    new TDMLInfosetOutputterAll(
      jsonStream,
      xmlStream,
      scalaOut,
      jdomOut,
      w3cdomOut,
      jsonOut,
      xmlOut
    )
  }
}

trait TDMLInfosetOutputter extends api.infoset.InfosetOutputter {

  def xmlStream: ByteArrayOutputStream

  def getResult: Node

  def toInfosetInputter: TDMLInfosetInputter
}
