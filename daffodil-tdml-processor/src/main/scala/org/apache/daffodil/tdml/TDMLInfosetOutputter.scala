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

package org.apache.daffodil.tdml

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream

import org.apache.daffodil.infoset.DIArray
import org.apache.daffodil.infoset.DIComplex
import org.apache.daffodil.infoset.DISimple
import org.apache.daffodil.infoset.InfosetOutputter
import org.apache.daffodil.infoset.JDOMInfosetInputter
import org.apache.daffodil.infoset.JDOMInfosetOutputter
import org.apache.daffodil.infoset.JsonInfosetInputter
import org.apache.daffodil.infoset.JsonInfosetOutputter
import org.apache.daffodil.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.infoset.ScalaXMLInfosetOutputter
import org.apache.daffodil.infoset.W3CDOMInfosetInputter
import org.apache.daffodil.infoset.W3CDOMInfosetOutputter
import org.apache.daffodil.infoset.XMLTextInfosetInputter
import org.apache.daffodil.infoset.XMLTextInfosetOutputter

class TDMLInfosetOutputter() extends InfosetOutputter {

  private def implString: String = "daffodil"

  private val jsonStream = new ByteArrayOutputStream()
  private val xmlStream = new ByteArrayOutputStream()

  private val scalaOut = new ScalaXMLInfosetOutputter()
  private val jdomOut = new JDOMInfosetOutputter()
  private val w3cdomOut = new W3CDOMInfosetOutputter()
  private val jsonOut = new JsonInfosetOutputter(jsonStream, false)
  private val xmlOut = new XMLTextInfosetOutputter(xmlStream, false)

  private val outputters = Seq(xmlOut, scalaOut, jdomOut, w3cdomOut, jsonOut)

  override def reset(): Unit = {
    outputters.foreach(_.reset())
  }

  override def startSimple(simple: DISimple): Boolean = {
    if (!outputters.forall(_.startSimple(simple)))
      throw TDMLException("startSimple failed", Some(implString))
    true
  }

  override def endSimple(simple: DISimple): Boolean = {
    if (!outputters.forall(_.endSimple(simple)))
      throw TDMLException("endSimple failed", Some(implString))
    true
  }

  override def startComplex(complex: DIComplex): Boolean = {
    if (!outputters.forall(_.startComplex(complex)))
      throw TDMLException("startComplex failed", Some(implString))
    true
  }

  override def endComplex(complex: DIComplex): Boolean = {
    if (!outputters.forall(_.endComplex(complex)))
      throw TDMLException("endComplex failed", Some(implString))
    true
  }

  override def startArray(array: DIArray): Boolean = {
    if (!outputters.forall(_.startArray(array)))
      throw TDMLException("startArray failed", Some(implString))
    true
  }

  override def endArray(array: DIArray): Boolean = {
    if (!outputters.forall(_.endArray(array)))
      throw TDMLException("endArray failed", Some(implString))
    true
  }

  override def startDocument(): Boolean = {
    if (!outputters.forall(_.startDocument()))
      throw TDMLException("startDocument failed", Some(implString))
    true
  }

  override def endDocument(): Boolean = {
    if (!outputters.forall(_.endDocument()))
      throw TDMLException("endDocument failed", Some(implString))
    true
  }

  def getResult() = scalaOut.getResult

  def toInfosetInputter() = {
    val scalaIn = new ScalaXMLInfosetInputter(scalaOut.getResult)
    val jdomIn = new JDOMInfosetInputter(jdomOut.getResult)
    val w3cdomIn = new W3CDOMInfosetInputter(w3cdomOut.getResult)
    val jsonIn = new JsonInfosetInputter(new ByteArrayInputStream(jsonStream.toByteArray))
    val xmlIn = new XMLTextInfosetInputter(new ByteArrayInputStream(xmlStream.toByteArray))
    new TDMLInfosetInputter(scalaIn, Seq(jdomIn, w3cdomIn, jsonIn, xmlIn))
  }
}
