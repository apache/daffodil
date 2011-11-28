package daffodil.parser

/**
 * Copyright (c) 2010 NCSA.  All rights reserved.
 * Developed by: NCSA Cyberenvironments and Technologies
 *               University of Illinois at Urbana-Champaign
 *               http://cet.ncsa.uiuc.edu/
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal with the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the names of NCSA, University of Illinois, nor the names of its
 *     contributors may be used to endorse or promote products derived from this
 *     Software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * WITH THE SOFTWARE.
 *
 */

/*
 * Developer: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu >
 * Date: 2010
 */

import org.scalatest.matchers.ShouldMatchers

import java.io.{BufferedInputStream, ByteArrayInputStream}

import org.scalatest.junit.JUnit3Suite
import scala.collection.mutable.ListBuffer
import junit.framework.Assert._

class RollbackStreamUniTest extends JUnit3Suite with ShouldMatchers {

  def testReadingTenBytes () {
    val input = new ByteArrayInputStream(Array(0 toByte,
					       1 toByte,
					       2 toByte,
					       3 toByte,
					       4 toByte,
					       5 toByte,
					       6 toByte,
					       7 toByte,
					       8 toByte,
					       9 toByte,
					       10 toByte))
    val rollbackStream = new RollbackStream(new BufferedInputStream(input))

    rollbackStream.read should equal (0)
    rollbackStream.read should equal (1)
    rollbackStream.read should equal (2)
    rollbackStream.read should equal (3)
    rollbackStream.read should equal (4)
    rollbackStream.read should equal (5)
    rollbackStream.read should equal (6)
    rollbackStream.read should equal (7)
    rollbackStream.read should equal (8)
    rollbackStream.read should equal (9)
    rollbackStream.read should equal (10)
    
    rollbackStream close
  }

  def testReadingUpToEndOfFile() {
    val input = new ByteArrayInputStream(Array(0 toByte,
					       1 toByte,
					       2 toByte,
					       3 toByte,
					       4 toByte,
					       5 toByte,
					       6 toByte,
					       7 toByte,
					       8 toByte,
					       9 toByte,
					       10 toByte))
    val rollbackStream = new RollbackStream(new BufferedInputStream(input))

    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read should equal (-1)

    rollbackStream close
  }

  def testReadingPastEndOfFile () {
    val input = new ByteArrayInputStream(Array(0 toByte,
					       1 toByte,
					       2 toByte,
					       3 toByte,
					       4 toByte,
					       5 toByte,
					       6 toByte,
					       7 toByte,
					       8 toByte,
					       9 toByte,
					       10 toByte))
    val rollbackStream = new RollbackStream(new BufferedInputStream(input))

    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read
    rollbackStream.read should equal (-1)
    rollbackStream.read should equal (-1)
    rollbackStream.read should equal (-1)

    rollbackStream close
  }

  def testUsingOneCheckpointOneRollback () {
    val input = new ByteArrayInputStream(Array(0 toByte,
					       1 toByte,
					       2 toByte,
					       3 toByte,
					       4 toByte,
					       5 toByte,
					       6 toByte,
					       7 toByte,
					       8 toByte,
					       9 toByte,
					       10 toByte))
    val rollbackStream = new RollbackStream(new BufferedInputStream(input))

    rollbackStream.read should equal (0)
    rollbackStream.read should equal (1)
    rollbackStream.read should equal (2)
    rollbackStream.read should equal (3)
    rollbackStream.read should equal (4)
    
    rollbackStream checkpoint

    rollbackStream.read should equal (5)
    rollbackStream.read should equal (6)
    rollbackStream.read should equal (7)

    rollbackStream rollback

    rollbackStream.read should equal (5)
    rollbackStream.read should equal (6)
    rollbackStream.read should equal (7)

    rollbackStream close
  }

  def testUsingOneCheckpointPassingEOFRollback() {
    val input = new ByteArrayInputStream(Array(0 toByte,
					       1 toByte,
					       2 toByte,
					       3 toByte,
					       4 toByte,
					       5 toByte,
					       6 toByte,
					       7 toByte,
					       8 toByte,
					       9 toByte,
					       10 toByte))
    val rollbackStream = new RollbackStream(new BufferedInputStream(input))

    rollbackStream.read should equal (0)
    rollbackStream.read should equal (1)
    rollbackStream.read should equal (2)
    rollbackStream.read should equal (3)
    rollbackStream.read should equal (4)
    
    rollbackStream checkpoint

    rollbackStream.read should equal (5)
    rollbackStream.read should equal (6)
    rollbackStream.read should equal (7)
    rollbackStream.read should equal (8)
    rollbackStream.read should equal (9)
    rollbackStream.read should equal (10)
    rollbackStream.read should equal (-1)

    rollbackStream rollback

    rollbackStream.read should equal (5)
    rollbackStream.read should equal (6)
    rollbackStream.read should equal (7)

    rollbackStream close
  }
  
  def testUsingTwoCheckpointsOneRollback() {
    val input = new ByteArrayInputStream(Array(0 toByte,
					       1 toByte,
					       2 toByte,
					       3 toByte,
					       4 toByte,
					       5 toByte,
					       6 toByte,
					       7 toByte,
					       8 toByte,
					       9 toByte,
					       10 toByte))
    val rollbackStream = new RollbackStream(new BufferedInputStream(input))

    rollbackStream.read should equal (0)
    rollbackStream.read should equal (1)
    rollbackStream.read should equal (2)
    rollbackStream.read should equal (3)
    rollbackStream.read should equal (4)
    
    rollbackStream checkpoint

    rollbackStream.read should equal (5)
    rollbackStream.read should equal (6)
    rollbackStream.read should equal (7)

    rollbackStream checkpoint

    rollbackStream.read should equal (8)
    rollbackStream.read should equal (9)
    rollbackStream.read should equal (10)

    rollbackStream rollback

    rollbackStream.read should equal (8)
    rollbackStream.read should equal (9)
    rollbackStream.read should equal (10)

    rollbackStream close
  }


  def testUsingTwoCheckpointsTwoRollbacks() {
    val input = new ByteArrayInputStream(Array(0 toByte,
					       1 toByte,
					       2 toByte,
					       3 toByte,
					       4 toByte,
					       5 toByte,
					       6 toByte,
					       7 toByte,
					       8 toByte,
					       9 toByte,
					       10 toByte))
    val rollbackStream = new RollbackStream(new BufferedInputStream(input))

    rollbackStream.read should equal (0)
    rollbackStream.read should equal (1)
    rollbackStream.read should equal (2)
    rollbackStream.read should equal (3)
    rollbackStream.read should equal (4)
    
    rollbackStream checkpoint

    rollbackStream.read should equal (5)
    rollbackStream.read should equal (6)
    rollbackStream.read should equal (7)

    rollbackStream checkpoint

    rollbackStream.read should equal (8)
    rollbackStream.read should equal (9)
    rollbackStream.read should equal (10)

    rollbackStream rollback

    rollbackStream.read should equal (8)
    rollbackStream.read should equal (9)
    rollbackStream.read should equal (10)

    rollbackStream rollback

    rollbackStream.read should equal (5)
    rollbackStream.read should equal (6)
    rollbackStream.read should equal (7)
    rollbackStream.read should equal (8)
    rollbackStream.read should equal (9)
    rollbackStream.read should equal (10)

    rollbackStream close
  }

  def testUsingTwoCheckpointsUncheckRollback() {
    val input = new ByteArrayInputStream(Array(0 toByte,
					       1 toByte,
					       2 toByte,
					       3 toByte,
					       4 toByte,
					       5 toByte,
					       6 toByte,
					       7 toByte,
					       8 toByte,
					       9 toByte,
					       10 toByte))
    val rollbackStream = new RollbackStream(new BufferedInputStream(input))

    rollbackStream.read should equal (0)
    rollbackStream.read should equal (1)
    rollbackStream.read should equal (2)
    rollbackStream.read should equal (3)
    rollbackStream.read should equal (4)
    
    rollbackStream checkpoint

    rollbackStream.read should equal (5)
    rollbackStream.read should equal (6)
    rollbackStream.read should equal (7)

    rollbackStream checkpoint

    rollbackStream.read should equal (8)
    rollbackStream.read should equal (9)
    rollbackStream.read should equal (10)

    rollbackStream uncheck()
    rollbackStream rollback
    
    rollbackStream.read should equal (5)
    rollbackStream.read should equal (6)
    rollbackStream.read should equal (7)
    rollbackStream.read should equal (8)
    rollbackStream.read should equal (9)
    rollbackStream.read should equal (10)
    rollbackStream.read should equal (-1)

    rollbackStream close
  }

  def testReadingTenBytesPushingBack5Reading5Again() {
    val input = new ByteArrayInputStream(Array(0 toByte,
					       1 toByte,
					       2 toByte,
					       3 toByte,
					       4 toByte,
					       5 toByte,
					       6 toByte,
					       7 toByte,
					       8 toByte,
					       9 toByte,
					       10 toByte))
    val rollbackStream = new RollbackStream(new BufferedInputStream(input))

    rollbackStream.read should equal (0)
    rollbackStream.read should equal (1)
    rollbackStream.read should equal (2)
    rollbackStream.read should equal (3)
    rollbackStream.read should equal (4)
    rollbackStream.read should equal (5)
    rollbackStream.read should equal (6)
    rollbackStream.read should equal (7)
    rollbackStream.read should equal (8)
    rollbackStream.read should equal (9)
    rollbackStream.read should equal (10)

    rollbackStream.pushBack(5)
    rollbackStream.read should equal (6)
    rollbackStream.read should equal (7)
    rollbackStream.read should equal (8)
    rollbackStream.read should equal (9)
    rollbackStream.read should equal (10)

    rollbackStream close
  }
}