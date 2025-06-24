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
import java.nio.charset.Charset
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

class TDMLInfosetOutputterScala(scalaOut: ScalaXMLInfosetOutputter)
  extends TeeInfosetOutputter(Seq(scalaOut): _*)
  with TDMLInfosetOutputter {

  override def getResult: Node = scalaOut.getResult()

  override lazy val xmlStream: ByteArrayOutputStream = {
    val bos = new ByteArrayOutputStream()
    bos.write(getResult.toString().getBytes(Charset.defaultCharset()))
    bos
  }

  override def toInfosetInputter: TDMLInfosetInputter = {
    val scalaIn = new ScalaXMLInfosetInputter(scalaOut.getResult())
    new TDMLInfosetInputter(scalaIn, Seq())
  }
}

object TDMLInfosetOutputterScala {
  def apply(): TDMLInfosetOutputterScala = {
    val scalaOut = new ScalaXMLInfosetOutputter()
    new TDMLInfosetOutputterScala(scalaOut)
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
) extends TeeInfosetOutputter(Seq(xmlOut, scalaOut, jdomOut, w3cdomOut, jsonOut): _*)
  with TDMLInfosetOutputter {

  override def getResult: Node = scalaOut.getResult()

  override def toInfosetInputter: TDMLInfosetInputter = {
    val scalaIn = new ScalaXMLInfosetInputter(scalaOut.getResult())
    val jdomIn = new JDOMInfosetInputter(jdomOut.getResult())
    val w3cdomIn = new W3CDOMInfosetInputter(w3cdomOut.getResult())
    val jsonIn = new JsonInfosetInputter(new ByteArrayInputStream(jsonStream.toByteArray))
    val xmlIn = new XMLTextInfosetInputter(new ByteArrayInputStream(xmlStream.toByteArray))
    val nullIn = {
      val events = NullInfosetInputter.toEvents(new ByteArrayInputStream(xmlStream.toByteArray))
      new NullInfosetInputter(events)
    }
    new TDMLInfosetInputter(scalaIn, Seq(jdomIn, w3cdomIn, jsonIn, xmlIn, nullIn))
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
