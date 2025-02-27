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
package org.apache.daffodil.lib.util

/**
 * Below is a performance study of Long vs. MaybeULong vs. Maybe[JLong] vs. Option[Long]
 * vs. Option[JLong]
 *
 * This shows that MaybeULong is much faster than Maybe[JLong],
 * and that Maybe[T] is faster than Option[T].
 *
 * However, MaybeULong still has overhead, vs regular old scala Long.
 */
class TestMaybeTakPerf {
  import java.lang.{ Long => JLong }

  import Maybe._

  /**
   * Tak, but passing MaybeInt objects which should be unboxed.
   * They are represented as a Long, so this should be close to the
   * performance of the same function just passing Long.
   */
  def taki(sx: MaybeInt, sy: MaybeInt, sz: MaybeInt): MaybeInt = {
    val x = sx.get
    val y = sy.get
    val z = sz.get
    if (y < x)
      taki(
        taki(MaybeInt(x - 1), MaybeInt(y), MaybeInt(z)),
        taki(MaybeInt(y - 1), MaybeInt(z), MaybeInt(x)),
        taki(MaybeInt(z - 1), MaybeInt(x), MaybeInt(y))
      )
    else
      sz
  }

  /**
   *  Tak, but passing Scala Option type Some objects.
   */
  def taks(sx: Option[Long], sy: Option[Long], sz: Option[Long]): Option[Long] = {
    val x = sx.get
    val y = sy.get
    val z = sz.get
    if (y < x)
      taks(
        taks(Some(x - 1), Some(y), Some(z)),
        taks(Some(y - 1), Some(z), Some(x)),
        taks(Some(z - 1), Some(x), Some(y))
      )
    else
      sz
  }

  /**
   *  Tak, but passing MaybeLong objects which should be unboxed.
   */
  def takml(sx: MaybeULong, sy: MaybeULong, sz: MaybeULong): MaybeULong = {
    val x = sx.get
    val y = sy.get
    val z = sz.get
    if (y < x)
      takml(
        takml(MaybeULong(x - 1), MaybeULong(y), MaybeULong(z)),
        takml(MaybeULong(y - 1), MaybeULong(z), MaybeULong(x)),
        takml(MaybeULong(z - 1), MaybeULong(x), MaybeULong(y))
      )
    else
      sz
  }

  /**
   * Original Tak function modified to used boxed java.lang.Long
   */
  def takJ(x: JLong, y: JLong, z: JLong): JLong = {
    if (y < x)
      takJ(
        takJ(JLong.valueOf(x - 1), JLong.valueOf(y), JLong.valueOf(z)),
        takJ(JLong.valueOf(y - 1), JLong.valueOf(z), JLong.valueOf(x)),
        takJ(JLong.valueOf(z - 1), JLong.valueOf(x), JLong.valueOf(y))
      )
    else
      z
  }

  /**
   * Tak, but passing Scala Option.Some objects carrying boxed JLong values.
   */
  def takJs(sx: Option[JLong], sy: Option[JLong], sz: Option[JLong]): Option[JLong] = {
    val x = sx.get
    val y = sy.get
    val z = sz.get
    if (y < x)
      takJs(
        takJs(Some(JLong.valueOf(x - 1)), Some(JLong.valueOf(y)), Some(JLong.valueOf(z))),
        takJs(Some(JLong.valueOf(y - 1)), Some(JLong.valueOf(z)), Some(JLong.valueOf(x))),
        takJs(Some(JLong.valueOf(z - 1)), Some(JLong.valueOf(x)), Some(JLong.valueOf(y)))
      )
    else
      sz
  }

  /**
   * Tak, but passing Maybe.One AnyVal objects
   * which should be unboxed themselves, but are carrying boxed JLong values.
   */
  def takJm(sx: Maybe[JLong], sy: Maybe[JLong], sz: Maybe[JLong]): Maybe[JLong] = {
    val x = sx.get
    val y = sy.get
    val z = sz.get
    if (y < x)
      takJm(
        takJm(One(JLong.valueOf(x - 1)), One(JLong.valueOf(y)), One(JLong.valueOf(z))),
        takJm(One(JLong.valueOf(y - 1)), One(JLong.valueOf(z)), One(JLong.valueOf(x))),
        takJm(One(JLong.valueOf(z - 1)), One(JLong.valueOf(x)), One(JLong.valueOf(y)))
      )
    else
      sz
  }

  /**
   * Commented out as we don't need to wait for this multi-second perf test
   * every time we run regression testing.
   */
  // @Test // can run as Scala application using companion object App below.
  def testTak(): Unit = {

    /**
     * We're going to use the same Tak arguments that are used to
     * compute Takeon units. That way we can compare Tak variants that
     * introduce various overheads to basic Tak.
     */
    val x = TakTimer.x
    val y = TakTimer.y
    val z = TakTimer.z

    var slowdown = 0.0

    def takSlowdown(call: => Any, message: String = null) = {
      val takeons = TakTimer.timeInTakeons(call, message)
      takeons / TakTimer.callCount
    }

    println("Comparison of Performance vs Scala Long (Java long) as arg/result type.")

    println("A takeon is " + TakTimer.takeon + " nanoseconds on this platform.")

    slowdown =
      takSlowdown(taki(MaybeInt(x.toInt), MaybeInt(y.toInt), MaybeInt(z.toInt)), "MaybeInt")
    println("MaybeInt objects are %s times slower".format(slowdown))

    slowdown = takSlowdown(takml(MaybeULong(x), MaybeULong(y), MaybeULong(z)), "MaybeULong")
    println("MaybeULong objects are %s times slower".format(slowdown))

    slowdown = takSlowdown(
      takJ(JLong.valueOf(x), JLong.valueOf(y), JLong.valueOf(z)),
      "JLong aka java.lang.Long"
    )
    println("Boxed JLong numbers are %s times slower".format(slowdown))

    slowdown = takSlowdown(taks(Some(x), Some(y), Some(z)), "Option[Long]")
    println("Option[Long] objects are %s times slower".format(slowdown))

    slowdown = takSlowdown(takJm(One(x), One(y), One(z)), "Maybe[JLong]")
    println("Maybe[JLong] numbers are %s times slower".format(slowdown))

    slowdown = takSlowdown(takJs(Some(x), Some(y), Some(z)), "Option[JLong]")
    println("Option[JLong] numbers are %s times slower".format(slowdown))

    println("End Test.")

  }

}

/**
 * Rather than running above as JUnit you can run this as
 * a scala application instead. Then you don't have to edit the file.
 */
object TestMaybeTakPerf extends App {
  val testClass = new TestMaybeTakPerf
  testClass.testTak()
}
