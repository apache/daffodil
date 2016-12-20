/* Copyright (c) 2012-2014 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.util

import edu.illinois.ncsa.daffodil.exceptions.Assert

/**
 *  Using Scala 2.10's Value Classes to make a Some/None style option type
 *  which does not allocate a boxed object.
 *
 *  To tell the difference these two items are called One(v) and Nope.
 *
 *  One important difference between this and scala Option types is this:
 *  Option(null) = None
 *  Some(null) != None // you get a Some object with null as its value.
 *  but
 *  Maybe(null) = Nope
 *  One(null) == Nope // can't construct a Maybe type containing null.
 */
final class Maybe[+T <: AnyRef](val v: AnyRef) extends AnyVal with Serializable {
  @inline final def get: T = if (isDefined) value else noneGet
  @inline final def value: T = v.asInstanceOf[T]
  final def noneGet = throw new NoSuchElementException("Nope.get")

  @inline final def isEmpty: Boolean = NopeValue eq v
  @inline final def isDefined: Boolean = !isEmpty
  @inline final def nonEmpty = isDefined
  @inline final def contains[U >: T](elem: U): Boolean = !isEmpty && value == elem
  //  @inline final def exists(p: T => Boolean): Boolean = !isEmpty && p(get)
  //  @inline final def forall(p: T => Boolean): Boolean = isEmpty || p(get)
  //  @inline final def collect[U <: AnyRef](pf: PartialFunction[T, U]): Maybe[U] = if (!isEmpty && pf.isDefinedAt(get)) One(pf(get)) else Nope
  //  @inline final def iterator: Iterator[T] = if (isEmpty) collection.Iterator.empty else collection.Iterator.single(get)
  @inline final def toList: List[T] = if (isEmpty) List() else new ::(get, Nil)
  @inline final def toSeq: Seq[T] = toList
  @inline final def getOrElse[U >: T](default: U): U = if (isEmpty) default else get

  /**
   * This is the back convert
   * {{{
   * val thing: T = ......something that returns null
   * val maybeThing: Maybe[T] = Maybe(thing)
   * assert (maybeThing eq Maybe.Nope) // null becomes Nope (unlike scala Option types where Some(null) is possible. That's not possible with Maybe
   *
   * // to call an API that uses the object or null conventions....
   *
   * val thingy : T = maybeThing.orNull
   * assert (thingy eq null) // back to an object of type T or null if not present.
   * }}}
   */
  @inline final def orNull: T = if (isEmpty) null.asInstanceOf[T] else value
  //  @inline final def filter(p: T => Boolean): Maybe[T] = if (isEmpty || p(get)) this else Nope
  //  @inline final def filterNot(p: T => Boolean): Maybe[T] = if (isEmpty || !p(get)) this else Nope
  //  @inline final def withFilter(f: T => Boolean): Maybe[T] = filter(f)
  //  @inline final def map[U <: AnyRef](f: T => U): Maybe[U] = if (isEmpty) Nope else One(f(get))
  //
  // rewrite x.map{y => foo(y) } like this: if (x.isEmpty) Nope else One{ val y = x.value ; foo(y) }
  //
  //  @inline final def flatMap[U <: AnyRef](f: T => Maybe[U]): Maybe[U] = if (isEmpty) Nope else f(get)
  /**
   * For testing if the function object gets allocated or inlined away.
   */
  @inline private[util] final def _foreach[U](f: T => U): Unit = if (!isEmpty) f(get)
  //  @inline final def fold[U](ifEmpty: => U)(f: T => U): U = if (isEmpty) ifEmpty else f(get)
  //  @inline final def flatten[U <: AnyRef](implicit ev: T <:< Maybe[U]): Maybe[U] = if (isEmpty) Nope else ev(get)
  @inline final def toScalaOption: scala.Option[T] = if (isEmpty) scala.None else scala.Some(get)
  override final def toString = if (isEmpty) "Nope" else "One(" + get + ")"
}

/**
 * Outside of Maybe because it has to be public due to inlining, and
 * it doesn't want to get imported by 'import Maybe._' since it is
 * for internal use only.
 */
object NopeValue extends Serializable {
  override def toString = "Nope"
}

object Maybe {

  import scala.language.implicitConversions

  /**
   *  implicitly treat as iterator/sequence/list (Scala's Option type has this)
   */
  // implicit def toIterator[T <: AnyRef](m: Maybe[T]) = m.toList

  /**
   * implicitly convert Option type to Maybe type.
   *
   * The conversion the other way must be explicit by calling toScalaOption
   */

  implicit def toMaybe[T <: AnyRef](o: Option[T]): Maybe[T] = o match {
    case None => Nope
    case Some(x) => One(x)
  }

  @inline
  final def apply[T <: AnyRef](value: T) = if (value == null) Nope else new Maybe[T](value)

  val Nope = new Maybe[Nothing](NopeValue)

  type One[T <: AnyRef] = Maybe[T]

  object One {
    @inline
    final def apply[T <: AnyRef](value: T) = Maybe(value)

    // If the pattern matching is going to box an object then this is hardly
    // worth using.
    //
    // def unapply[T](value: Maybe[T]) = if (value.isDefined) scala.Some(value.get) else scala.None
  }

  /**
   * Use to do Maybe-like things using object vs. null.
   *
   * Done to avoid allocation of Maybe objects, for example if you
   * are storing them in a generic collection, or passing them polymorphically,
   * then a Maybe[T] will get
   * allocated because generic collection items are AnyRef, and polymorphism
   * works across AnyRefs only.
   *
   * Lets you use the same API as for Maybe objects. i.e., isDefined, get, etc.
   * or you can define an API that returns Maybe[T] by calling the toMaybe
   * function when returning a result, and thereby hide the fact that the
   * implementation is Object/null, but the API looks like Maybe[T] are being stored.
   *
   * The above is what I would call a "Conceptual Maybe" object being stored.
   */
  object WithNulls {

    @inline final def isDefined[T <: AnyRef](thing: T): Boolean = {
      if (thing eq null) false
      else if (thing eq NopeValue) Assert.usageError("Maybe.WithNulls.isDefined not for use on Maybe[T] objects, but T (or null) objects.")
      else true
    }

    @inline final def get[T <: AnyRef](thing: T): T = {
      if (!isDefined(thing)) throw new NoSuchElementException("get on undefined value: " + thing)
      else thing
    }

    final def toScalaOption[T <: AnyRef](thing: T): Option[T] = {
      if (!isDefined(thing)) None else Some(thing)
    }

    @inline final def toMaybe[T <: AnyRef](thing: AnyRef): Maybe[T] = {
      if (thing eq null) Nope else new Maybe(thing.asInstanceOf[T])
    }
  }
}

/**
 * Below is a performance study of Int vs. Maybe[Int] vs. Option[Int]
 *
 * Maybe is faster than Some/None
 */
//private object Tak extends App {
//  import Maybe._
//
//  def calibrate = {
//    if (takeons == 0.0) {
//      testTak
//    }
//  }
//
//  var takeons = 0.0 // value on Mike Beckerle's laptop
//
//  var callCount: Long = 0
//
//  // Original Tak function
//  def tak1(x: Long, y: Long, z: Long): Long = {
//    callCount += 1
//    if (y < x)
//      tak1(
//        tak1(x - 1, y, z),
//        tak1(y - 1, z, x),
//        tak1(z - 1, x, y))
//    else
//      z
//  }
//
//  // Tak, but passing Scala Option type Some objects.
//  def taks(sx: Option[Long], sy: Option[Long], sz: Option[Long]): Option[Long] = {
//    val x = sx.get
//    val y = sy.get
//    val z = sz.get
//    callCount += 1
//    if (y < x)
//      taks(
//        taks(Some(x - 1), Some(y), Some(z)),
//        taks(Some(y - 1), Some(z), Some(x)),
//        taks(Some(z - 1), Some(x), Some(y)))
//    else
//      sz
//  }
//
//  // Tak, but passing scala 2.10 maybe One "value class" objects
//  // which should be unboxed.
//  def tak(sx: Maybe[Long], sy: Maybe[Long], sz: Maybe[Long]): Maybe[Long] = {
//    val x = sx.value
//    val y = sy.value
//    val z = sz.value
//    callCount += 1
//    if (y < x)
//      tak(
//        tak(One(x - 1), One(y), One(z)),
//        tak(One(y - 1), One(z), One(x)),
//        tak(One(z - 1), One(x), One(y)))
//    else
//      sz
//  }
//
//  def testTak() {
//    println("Calibrating takeon units")
//    callCount = 0
//    val x = 20L
//    val y = 4L
//    val z = 20L
//    var nanos = Timer.getTimeNS { tak1(x, y, z) }
//    println("tak call count = " + callCount + " in " + nanos + "ns")
//    takeons = (1.0 * nanos) / callCount
//    println("Under current load, 1 CPU of this system executes " + takeons + " nanoseconds per tak call.")
//    println("So on this system, currently, 1 takeon = " + takeons + "ns")
//    println("Done calibrating")
//    var t0 = System.nanoTime
//    tak(One(x), One(y), One(z))
//    var t1 = System.nanoTime
//    val maybenanos = t1 - t0
//    println("maybe objects are %s times slower".format(maybenanos.toDouble / nanos))
//
//    t0 = System.nanoTime
//    taks(Some(x), Some(y), Some(z))
//    t1 = System.nanoTime
//    val somenanos = t1 - t0
//    println("Some objects are %s times slower".format(somenanos.toDouble / nanos))
//  }
//
//  val res = testTak()
// }
