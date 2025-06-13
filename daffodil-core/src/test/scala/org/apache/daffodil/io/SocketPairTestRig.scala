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

package org.apache.daffodil.io

import java.io.InputStream
import java.io.OutputStream
import java.net.ServerSocket
import java.net.Socket
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.TimeoutException
import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

import org.apache.daffodil.io.SocketPairTestRig.timeLimit
import org.apache.daffodil.io.SocketPairTestRig.withTimeout
import org.apache.daffodil.lib.Implicits.intercept

import org.junit.Assert.assertEquals
import org.junit.Assert.fail
import org.junit.Test

/**
 * Test rig sets up TCP socket unidirectional between a producer
 * output stream, and a consumer input stream.
 */
abstract class SocketPairTestRig {

  /**
   * Override with test logic method. This logic can read and write from the argument
   * streams. The fact that these are socket connections means there is some buffering
   * there. But you cannot write an infinite amount of data to producer without causing
   * trouble with consuming all the operating system buffering, and once that happens
   * the write will block.
   *
   * @param producerOutputStream
   * @param consumerInputStream
   */
  protected def test(producerOutputStream: OutputStream, consumerInputStream: InputStream): Unit

  def run(): Unit = {
    val serverSocket = new ServerSocket(0) // 0 means to allocate an unused port
    val port = serverSocket.getLocalPort()
    val useLoopBack: String = null
    val producerSocket = new Socket(useLoopBack, port)
    val consumerSocket = serverSocket.accept()
    assert(producerSocket.isConnected)
    assert(consumerSocket.isConnected)
    try {
      val producerOutputStream = producerSocket.getOutputStream()
      val consumerInputStream = consumerSocket.getInputStream()

      // one way connection (no reverse flow control)
      // NOTE: Can't do this. It seems to cause the other direction
      // to also close.
      // producerSocket.getInputStream().close()
      // consumerSocket.getOutputStream().close()

      test(producerOutputStream, consumerInputStream)
    } catch {
      case th: Throwable =>
        throw th // good place for a breakpoint - but keep in mind tests use timeouts
    } finally {
      producerSocket.close()
      consumerSocket.close()
      serverSocket.close() // assumption is that this frees up the port
    }
  }
}

object SocketPairTestRig {

  val timeLimit = 1000.milliseconds

  /**
   * Runs test code with a timeout. Intended for use with tests that
   * could hang if there are bugs where Daffodil does a blocking read
   * that it shouldn't.
   *
   * @param whatTimedOutDescription Description of what we're testing. Ex: "Daffodil parse"
   * @param testThunk               The test code that could hang. Shouldn't, but could.
   * @tparam T Return type of the testThunk
   * @return the result of the testThunk
   */
  def withTimeout[T](whatTimedOutDescription: String)(testThunk: => T): T = {
    Try(withTimeout(testThunk)).recover { case e: TimeoutException =>
      fail(whatTimedOutDescription + " timed out.")
      ??? // ??? because scala doesn't know fail never returns.
    }.get
  }

  /**
   * Doesn't have the fail(...) built in, so we can
   * write tests that expect a timeout rather than fail on a timeout.
   * @param testThunk
   * @tparam T
   * @return
   */
  def withTimeout[T](testThunk: => T): T =
    withTimeout(timeLimit)(testThunk)

  /**
   * Gives control over the timeout limit
   */
  def withTimeout[T](limitMillis: FiniteDuration)(testThunk: => T): T =
    Await.result(Future(testThunk), limitMillis)

}

/**
 * Shows that we can read from a TCP network socket
 * without blocking for bytes past the end of what we read.
 */
class TestSocketPairTestRig {

  /**
   * Test the test rig. Make sure we can send a byte, and receive that byte,
   * and if we close the producer stream, we get EOD on the
   * consumer.
   */
  @Test def testSocketPairTestRig1(): Unit = {
    val sptr = new SocketPairTestRig {
      override def test(pos: OutputStream, cis: InputStream): Unit = {
        pos.write(0x31)
        pos.close()
        val i: Int = cis.read()
        assertEquals(0x31, i)
        val j = cis.read()
        assertEquals(-1, j)
      }
    }
    sptr.run()
  }

  /**
   * Test showing that when a blocking read on the consumer input stream
   * in fact blocks, that we can detect this via timeout.
   */
  @Test def testHangDetection1(): Unit = {
    val sptr = new SocketPairTestRig {
      override def test(pos: OutputStream, cis: InputStream): Unit = {

        //
        // Write exactly 4 bytes to producer network stream
        //
        pos.write("1234".getBytes)
        pos.flush()

        //
        // read 4 bytes. Should not hang.
        //
        val nRead = {
          val buf = new Array[Byte](4)
          cis.read(buf, 0, 4)
          assertEquals("1234".getBytes.toSeq, buf.toSeq)
        }
        //
        // read 1 more byte. This will hang, and timeout.
        //
        intercept[TimeoutException] {
          withTimeout(1000.milliseconds) {
            val buf = new Array[Byte](1)
            cis.read(buf, 0, 1)
          }
          fail("Failed to throw exception")
        }
      }
    }
    sptr.run()
  }

  /**
   * Test showing that a blocking read that hangs on the consumer stream
   * will eventually be satisfied by something else writing to the producer stream.
   */
  @Test def testHangDetection2(): Unit = {
    val sptr = new SocketPairTestRig {
      override def test(pos: OutputStream, cis: InputStream): Unit = {

        //
        // Write exactly 3 bytes to producer network stream
        //
        pos.write("123".getBytes)
        pos.flush()

        var buf = new Array[Byte](4)
        var nRead = {
          //
          // This won't hang
          //
          cis.read(buf, 0, 4)
        }
        assertEquals(3, nRead)
        assertEquals("123".getBytes.toSeq, buf.toSeq.slice(0, 3))

        //
        // In parallel, wait a bit, then write another byte.
        //
        Future {
          Thread.sleep(timeLimit.toMillis / 10) // wait 1/10 of the normal timeout
          pos.write("4".getBytes)
          pos.flush()
        }
        //
        // This won't hang. We'll wait long enough that our
        // blocking read will get satisfied when the write above
        // finally happens.
        //
        buf = new Array[Byte](1)
        nRead = {
          cis.read(buf, 0, 1)
        }
        assertEquals('4'.toByte, buf(0))
        assertEquals(1, nRead)
      }
    }
    sptr.run()
  }
}
