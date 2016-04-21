package edu.illinois.ncsa.daffodil.util


class TestMaybe {

  type JINT = java.lang.Long
  var e: JINT = 5

  var t: Long = 0

  def passOne(mi: Maybe[JINT]) = {
    if (mi.isDefined) t += mi.get
  }

  def returnOne(e: JINT) = Maybe(e)

  /**
   * This test can be used to verify that passing Maybe[T] objects
   * and returning Maybe[T] objects doesn't cause allocation to occur.
   *
   * Uncomment and run, then examine with jvisualvm or jprofiler or some
   * tool where you can watch allocation on the main thread.
   */
  //  @Test def testIfMaybeAllocatesOnPassOrReturn {
  //    var i: Int = 0
  //    while (i < 10000000000000L) {
  //      //
  //      // While this is running, watch on jvisualvm or jprofiler
  //      // to see if allocation is occurring on the main thread.
  //      //
  //      // slow allocation on other threads is to be expected.
  //      //
  //      i += 1
  //      passOne(Maybe(e))
  //      val mi = returnOne(e)
  //      if (mi.isDefined) t += mi.get
  //    }
  //    println(t)
  //  }

}
