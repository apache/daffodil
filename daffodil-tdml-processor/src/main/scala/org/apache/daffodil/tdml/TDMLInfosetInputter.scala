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

import java.net.URI
import java.net.URISyntaxException
import org.apache.daffodil.infoset.InfosetInputter
import org.apache.daffodil.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.infoset.InfosetInputterEventType
import org.apache.daffodil.infoset.JsonInfosetInputter
import org.apache.daffodil.util.MaybeBoolean
import org.apache.daffodil.util.Misc
import org.apache.daffodil.dpath.NodeInfo

class TDMLInfosetInputter(val scalaInputter: ScalaXMLInfosetInputter, others: Seq[InfosetInputter]) extends InfosetInputter {

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
      if (!i.supportsNamespaces) {
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

  override def getSimpleText(primType: NodeInfo.Kind, runtimeProperties: java.util.Map[String, String]): String = {
    val res = scalaInputter.getSimpleText(primType, runtimeProperties)
    val resIsEmpty = res == null || res == ""
    val otherStrings = others.map { i =>
      val firstVersion = i.getSimpleText(primType, runtimeProperties)
      val finalVersion = i match {
        case _ if (firstVersion eq null) => ""
        // the json infoset inputter maintains CRLF/CR, but XML converts CRLF/CR to
        // LF. So if this is Json, then we want the CRLF/CR converted to LF
        case jsonii: JsonInfosetInputter => firstVersion.replaceAll("(\r\n|\r)", "\n")
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

  override def isNilled(): MaybeBoolean = {
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
    scalaInputter.next
    others.foreach(_.next)
  }

  override def fini(): Unit = {
    scalaInputter.fini
    others.foreach(_.fini)
  }

  override val supportsNamespaces = true
}
