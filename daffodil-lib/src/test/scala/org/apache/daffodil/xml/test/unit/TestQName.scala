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

package org.apache.daffodil.xml.test.unit

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.Assert._
import org.apache.daffodil.xml.QNameRegex
import org.apache.daffodil.xml.QName
import org.apache.daffodil.xml.RefQName
import org.apache.daffodil.xml.NoNamespace
import scala.util.Success
import org.apache.daffodil.xml.UnspecifiedNamespace
import org.apache.daffodil.xml.ExtendedQNameSyntaxException
import scala.util.Failure
import java.net.URISyntaxException

class TestQName {

  @Test def testQNameLongPrefix() {
    val bigPrefix = ("a" * 6000)
    val data = <xs:element name="one" type={ bigPrefix + ":b" }/>
    val qntext = (data \ "@type").text

    // println("length of type attribute = " + qntext.length)
    qntext match {
      case QNameRegex.QName(pre, local) => {
        // println(pre)
        // println(local)
        assertEquals(6001, pre.length + local.length)
      }
    }
  }

  @Test def testExtQName1() {
    val tryrqn = QName.refQNameFromExtendedSyntax("local")
    tryrqn match {
      case Success(RefQName(None, "local", UnspecifiedNamespace)) => // ok
      case _ => fail(tryrqn.toString)
    }
  }

  @Test def testExtQName2() {
    val tryrqn = QName.refQNameFromExtendedSyntax("{}local")
    tryrqn match {
      case Success(RefQName(None, "local", NoNamespace)) => // ok
      case _ => fail(tryrqn.toString)
    }
  }

  @Test def testExtQName3() {
    val tryrqn = QName.refQNameFromExtendedSyntax("{uri}local")
    tryrqn match {
      case Success(RefQName(None, "local", ns)) => assertEquals("uri", ns.uri.toString)
      case _ => fail(tryrqn.toString)
    }
  }

  @Test def testExtQName4() {
    val tryrqn = QName.refQNameFromExtendedSyntax("pre:local")
    tryrqn match {
      case Success(RefQName(Some("pre"), "local", UnspecifiedNamespace)) => //ok
      case _ => fail(tryrqn.toString)
    }
  }

  @Test def testExtQName5() {
    val tryrqn = QName.refQNameFromExtendedSyntax("pre:{}local")
    tryrqn match {
      case Failure(exc: ExtendedQNameSyntaxException) => // ok
      case _ => fail(tryrqn.toString)
    }
  }

  @Test def testExtQName6() {
    val tryrqn = QName.refQNameFromExtendedSyntax("{http://<<notValidURI>>}local")
    tryrqn match {
      case Failure(exc: ExtendedQNameSyntaxException) => {
        val c = exc.getCause()
        c match {
          case usex: URISyntaxException => // ok
          case _ => fail("Expected a URISyntaxException, but got " + c)
        }
      }
      case Failure(exc) => fail(exc.getMessage())
      case _ => fail(tryrqn.toString)
    }
  }

}
