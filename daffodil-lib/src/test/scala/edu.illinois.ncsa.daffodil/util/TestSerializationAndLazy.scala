package edu.illinois.ncsa.daffodil.util

import junit.framework.Assert._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import java.io.ByteArrayInputStream
import java.io.ObjectInputStream
import org.scalatest.junit.JUnitSuite
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
class TestSerializationAndLazy extends JUnitSuite {

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