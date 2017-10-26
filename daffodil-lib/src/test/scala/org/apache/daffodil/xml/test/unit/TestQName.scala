/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.xml.test.unit

import junit.framework.Assert.assertEquals
import org.junit.Test
import org.junit.Assert._
import edu.illinois.ncsa.daffodil.xml.QNameRegex
import edu.illinois.ncsa.daffodil.xml.QName
import edu.illinois.ncsa.daffodil.xml.RefQName
import edu.illinois.ncsa.daffodil.xml.NoNamespace
import scala.util.Success
import edu.illinois.ncsa.daffodil.xml.UnspecifiedNamespace
import edu.illinois.ncsa.daffodil.xml.ExtendedQNameSyntaxException
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
