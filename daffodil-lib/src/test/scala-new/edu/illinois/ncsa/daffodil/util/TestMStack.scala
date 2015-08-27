package edu.illinois.ncsa.daffodil.util

import org.junit.Test
import scala.collection.mutable.ArrayStack
import Maybe._

/**
 * Compare MStack performance to ArrayStack. It should be faster for primitives
 */
class TestMStack {

  var junk: Long = 0

  /**
   * This test compares MStack.OfLong to ArrayStack[Long].
   *
   * It shows MStack.OfLong is faster, this is probably due to the
   * fact that it can avoid boxing/unboxing the numbers. However, I haven't
   * disassembled the byte code to verify that this is the difference.
   *
   * (On my laptop: 64%)
   */
  //  @Test def testMStackSpeed1 {
  //    val numOps: Long = 1000000000
  //    val longStack = new MStack.OfLong
  //    val arrayStack = new ArrayStack[Long]
  //
  //    // Warm up the JVM on MStack
  //    var i: Long = 0
  //    while (i < numOps) {
  //      i = i + 1
  //      longStack.push(0L)
  //      longStack.pop
  //    }
  //    // Warm up JVM on ArrayStack
  //    i = 0
  //    while (i < numOps) {
  //      i = i + 1
  //      arrayStack.push(0L)
  //      arrayStack.pop
  //    }
  //    val (nanosMStack, _) = Timer.getTimeResult {
  //      var i: Long = 0
  //      while (i < numOps) {
  //        i = i + 1
  //        longStack.push(0L)
  //        longStack.pop
  //      }
  //
  //    }
  //
  //    val (nanosAStack, _) = Timer.getTimeResult {
  //      var i: Long = 0
  //      while (i < numOps) {
  //        i = i + 1
  //        arrayStack.push(0L)
  //        arrayStack.pop
  //      }
  //    }
  //
  //    val result = (nanosMStack * 100.0) / nanosAStack.toDouble
  //
  //    println("MStack runs in " + result + "% of the time of ArrayStack.")
  //
  //  }
  //
  //  /**
  //   * Compare performance for MStack.OfMaybe[Thing] vs ArrayStack[Maybe[Thing]]
  //   *
  //   * Faster, probably due to lack of need to box/unbox the Maybe[Thing].
  //   *
  //   * (On my laptop: 17% of the time.)
  //   */
  //  @Test def testMStackMaybeSpeed2 {
  //    val numOps: Long = 100000000
  //    object Thing extends Thing
  //    class Thing {
  //      val n = 1
  //    }
  //    val mThing = Maybe(Thing)
  //    val mThingStack = new MStack.Of[Thing]
  //    val arrayStack = new ArrayStack[Maybe[Thing]]
  //
  //    // Warm up the JVM on MStack
  //    var i: Long = 0
  //    while (i < numOps) {
  //      i = i + 1
  //      mThingStack.push(mThing.get)
  //      val popped = Maybe(mThingStack.pop)
  //      junk += popped.get.n
  //    }
  //    // Warm up JVM on ArrayStack
  //    i = 0
  //    while (i < numOps) {
  //      i = i + 1
  //      arrayStack.push(One(Thing))
  //      junk += arrayStack.pop.get.n
  //    }
  //    val (nanosMStack, _) = Timer.getTimeResult {
  //      var i: Long = 0
  //      while (i < numOps) {
  //        i = i + 1
  //        mThingStack.push(mThing.get)
  //        val popped = Maybe(mThingStack.pop)
  //        junk += popped.get.n
  //      }
  //
  //    }
  //
  //    val (nanosAStack, _) = Timer.getTimeResult {
  //      var i: Long = 0
  //      while (i < numOps) {
  //        i = i + 1
  //        arrayStack.push(One(Thing))
  //        junk += arrayStack.pop.get.n
  //      }
  //    }
  //
  //    val result = (nanosMStack * 100.0) / nanosAStack.toDouble
  //
  //    println("MStack runs in " + result + "% of the time of ArrayStack.") // 63% under eclipse, no optimizers.
  //
  //  }

}