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

package org.apache.daffodil.infoset

import com.siemens.ct.exi.core.helpers.DefaultEXIFactory
import javax.xml.transform.TransformerFactory
import com.siemens.ct.exi.main.api.sax.EXISource
import com.siemens.ct.exi.grammars.GrammarFactory
import com.siemens.ct.exi.core.FidelityOptions
import javax.xml.transform.sax.SAXSource
import org.xml.sax.InputSource
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.BufferedOutputStream
import javax.xml.transform.stream.StreamResult
import org.apache.daffodil.util.MaybeBoolean
import org.apache.daffodil.dpath.NodeInfo

object EXIInfosetInputter {
  def ConvertEXIToXMLWithExificient(input: java.io.InputStream): java.io.InputStream = {
    val xsdLocation = "daffodil-lib/src/main/resources/org/apache/daffodil/xsd/XMLSchema.xsd"
    lazy val exiFactory = DefaultEXIFactory.newInstance()
    val grammarFactory = GrammarFactory.newInstance()
    val g = grammarFactory.createGrammars(xsdLocation)
    exiFactory.setGrammars(g)
    exiFactory.getFidelityOptions().setFidelity(FidelityOptions.FEATURE_PREFIX, true)
    val saxSource = new EXISource(exiFactory)
    val xmlReader = saxSource.getXMLReader()

    val tf = TransformerFactory.newInstance()
    val transformer = tf.newTransformer()

    val exiSource = new SAXSource(new InputSource(input))
    exiSource.setXMLReader(xmlReader);

    val baos = new ByteArrayOutputStream()
    val bos = new BufferedOutputStream(baos)
    val sr = new StreamResult(bos)
    transformer.transform(exiSource, sr)
    val is = new ByteArrayInputStream(baos.toString.getBytes)
    // System.err.println(baos.toString)
    // System.err.println()
    // //Reader r = new Reader()
    // //r.read(Arrays.toString(baos.toString))
    // var cont = true
    // while(cont){
    //   var c = is.read
    //   if (c == -1){
    //     cont = false
    //   }
    //   else{
    //     System.err.print(c.asInstanceOf[Char])
    //   }
    // }
    // System.err.println()
    // System.err.println(is.toString)
    is
  }

  // def ConvertEXIToXMLWithNagasena(input: java.io.InputStream): java.io.InputStream = {

  // }

  // def ConvertEXIToXMLWithAgile(input: java.io.InputStream): java.io.InputStream = {

  // }
}

class EXIInfosetInputter (input: java.io.InputStream) extends InfosetInputter{

  lazy val xmlInputter = new XMLTextInfosetInputter(xmlInputStream)

  lazy val xmlInputStream = EXIInfosetInputter.ConvertEXIToXMLWithExificient(input)

  override val supportsNamespaces = true

  override def fini(): Unit = {
    xmlInputter.fini
  }

  override def hasNext(): Boolean = {
    xmlInputter.hasNext
  }

  override def next(): Unit = {
    xmlInputter.next
  }

  override def isNilled(): MaybeBoolean = {
    xmlInputter.isNilled
  }

  override def getSimpleText(primType: NodeInfo.Kind, runtimeProperties: java.util.Map[String, String]): String = {
    xmlInputter.getSimpleText(primType, runtimeProperties)
  }

  override def getNamespaceURI(): String = {
    xmlInputter.getNamespaceURI
  }

  override def getLocalName: String = {
    xmlInputter.getLocalName
  }

  override def getEventType(): InfosetInputterEventType = {
    xmlInputter.getEventType
  }

  def getInputStream(): java.io.InputStream = {
    xmlInputStream
  }
}
