package edu.illinois.ncsa.daffodil.util

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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


import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import java.io.ByteArrayInputStream
import java.io.ObjectInputStream
import org.junit.Test

class ToSerialize extends Serializable {

  val v = 5
  var lazyValWasEvaluated = false
  lazy val x = {
    // println("v is " + v)
    lazyValWasEvaluated = true
    2 * v
  }

}

/**
 * This test shows that use of lazy val does not interfere with object serialization.
 * An unevaluated lazy val remains that way across serialization/deserialization.
 *
 * We're going to be very dependent on this not being broken by some Scala release patch, so leave
 * this test in just in case of that so we can detect it.
 */
class TestSerializationAndLazy {

  @Test
  def testSerializeBeforeLazyEval() {
    val instance = new ToSerialize
    val baos = new ByteArrayOutputStream
    val stream = new ObjectOutputStream(baos)
    stream.writeObject(instance)
    stream.flush()
    stream.close()
    assertFalse(instance.lazyValWasEvaluated)
    val ba = baos.toByteArray()
    val bais = new ByteArrayInputStream(ba)
    val istream = new ObjectInputStream(bais)
    val restoredInstance = istream.readObject()
    istream.close()
    assertTrue(restoredInstance.isInstanceOf[ToSerialize])
    val ts = restoredInstance.asInstanceOf[ToSerialize]
    assertFalse(ts.lazyValWasEvaluated)
    val z = ts.x
    assertTrue(ts.lazyValWasEvaluated)
  }

}