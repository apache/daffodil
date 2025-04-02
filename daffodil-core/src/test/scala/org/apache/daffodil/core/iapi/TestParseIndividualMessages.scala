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

package org.apache.daffodil.core.iapi

import java.io.InputStream
import java.io.OutputStream
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.xml.Node

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.io.SocketPairTestRig
import org.apache.daffodil.lib.Implicits.intercept
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.runtime1.iapi._

import org.junit.Assert._
import org.junit.Test

/**
 * Shows that we can parse exactly 1 message from a TCP network socket
 * without blocking for bytes past the end of the messsage.
 *
 * This only works for DFDL schemas of formats that are specified length.
 */
class TestParseIndividualMessages {

  //
  // DFDL schema for element e1 which occupies exactly 4 bytes.
  //
  val exactly4ByteSch = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format representation="binary" byteOrder="bigEndian" binaryNumberRep="binary" ref="tns:GeneralFormat"/>,
    <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="4"/>
  )

  /**
   * Test shows that at least for simple fixed-length data, Daffodil parse returns
   * without requiring more bytes to be read than the exact length required.
   */
  @Test def testDaffodilParseFromNetwork1(): Unit = {
    val sptr = new SocketPairTestRig {
      override def test(pos: OutputStream, cis: InputStream): Unit = {

        val dp = TestUtils.compileSchema(exactly4ByteSch)

        //
        // Write exactly 4 bytes to producer network stream
        //
        pos.write("1234".getBytes)
        pos.flush()

        //
        // Daffodil parse element e1 from consumer input stream
        //
        // If we need more than 4 bytes to successfully parse (we shouldn't for this schema)
        // then this will hang, because only 4 bytes are in fact available.
        val (pr: DFDL.ParseResult, xml: Node) =
          TestUtils.runDataProcessorOnInputStream(dp, cis, areTracing = false)

        assertFalse(pr.isError)
        assertEquals("1234", xml.text)
        assertEquals(33, pr.resultState.currentLocation.bitPos1b)
      }
    }
    sptr.run()
  }

  //
  // DFDL schema for delimited element.
  //
  private def delimitedSchema(term: String) = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format representation="text" ref="tns:GeneralFormat"/>,
    <xs:element name="e1" type="xs:string" dfdl:lengthKind="delimited"
                  dfdl:terminator={term} />
  )

  /**
   * Helper so we can test various delimiter-oriented scenarios.
   *
   * @param stringData          Data to write. Should be small enough that the parse will block.
   * @param terminator          String to use as terminating delimiter of element. Can be more than one delimiter.
   * @param followingDataString Data to write which should unblock the parse.
   */
  private def testHelperDaffodilParseDelimitedFromNetwork(
    data: String,
    terminator: String,
    followingDataString: String
  ) = {
    val sptr = new SocketPairTestRig {
      override def test(pos: OutputStream, cis: InputStream): Unit = {

        val dp = TestUtils.compileSchema(delimitedSchema(terminator))

        // Write the data. Should be too short to satisfy the parse.
        //
        pos.write(data.getBytes)
        pos.flush()

        val fut = Future {
          TestUtils.runDataProcessorOnInputStream(dp, cis, areTracing = false)
        }

        Thread.sleep(100)
        assertFalse(fut.isCompleted)
        //
        // Writing additional character(s) should unblock the parse.
        //
        pos.write(followingDataString.getBytes)
        pos.flush()
        val (pr, xml) = Await.result(fut, Duration.Inf)
        assertFalse(pr.isError)
        assertEquals("1234", xml.text)
      }
    }
    sptr.run()
  }

  /**
   * This test fails (and so is commented out) but *should* pass.
   *
   * Test (when it works) shows that for delimited data, we block seeking more data,
   * but once the need for a terminator with longest match is satisfied
   * the parse is unblocked.
   */
  // @Test // DAFFODIL-2504 - this fails looking for more data than is needed for terminator.
  def testDaffodilParseFromNetworkDelimited1(): Unit = {
    intercept[TimeoutException] {
      testHelperDaffodilParseDelimitedFromNetwork("1234", "$", "$")
    }
    fail(
      "if we get here, then we intercepted a TimeoutException, which means Daffodil was hung."
    )
  }

  /**
   * This test works, and it should work.
   *
   * It characterizes that currently we need to be able to read
   * not only the terminator, but 7 more characters, in order for the reads associated with the delimiter
   * scanning to be satisfied.
   *
   * We're not supposed to need 7 characters though. Only 1 character should do it.
   */
  @Test // DAFFODIL-2504
  def testDaffodilParseFromNetworkDelimited1b(): Unit = {
    testHelperDaffodilParseDelimitedFromNetwork("1234", "$", "$1234567")
  }

  /**
   * This test fails (and so is commented out) but *should* pass.
   *
   * Test (when it works) shows that for delimited data, we block seeking more data,
   * but once the need for a terminator with longest match is satisfied
   * the parse is unblocked.
   *
   * This variant has 2 options for terminator, $ or $$, and one is a prefix of the other
   * So this test insures that getting a shorter delimiter match doesn't unblock
   * when getting more data might match a longer delimiter.
   *
   * The test then subsequently provides the character for that longer delimiter
   * which should unblock daffodil.
   */
  // @Test // DAFFODIL-2504 - this fails looking for more data than is needed for terminator.
  def testDaffodilParseFromNetworkDelimited2(): Unit = {
    intercept[TimeoutException] {
      testHelperDaffodilParseDelimitedFromNetwork("1234$", "$ $$", "$")
    }
    fail("If we get here, we intercepted a TimeoutException, which means Daffodil is hung.")
  }

  /**
   * This test works, and it should work.
   *
   * It characterizes that currently we need to be able to read
   * not only the longest possible terminator, but 7 more characters,
   * in order for the reads associated with the delimiter
   * scanning to be satisfied.
   *
   * We're not supposed to need 7 characters though. Only 1 character should do it.
   */
  @Test // DAFFODIL-2504 - this shouldn't require 7 more characters.
  def testDaffodilParseFromNetworkDelimited2b(): Unit = {
    testHelperDaffodilParseDelimitedFromNetwork("1234$", "$ $$", "$1234567")
  }

}
