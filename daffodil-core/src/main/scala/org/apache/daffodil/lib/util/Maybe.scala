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

import org.apache.daffodil.lib.exceptions.Assert

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
 *  One(null) = throws an exception
 */
final class Maybe[+T <: AnyRef](val v: AnyRef) extends AnyVal with Serializable {
  @inline final def get: T = if (isDefined) value else noneGet
  @inline final def value: T = v.asInstanceOf[T]
  final def noneGet =
    throw new NoSuchElementException("Nope.get") // good place for a breakpoint

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
  // @inline final def getOrElse[U >: T](default: U): U = if (isEmpty) default else get

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
  @inline final def toOption: scala.Option[T] =
    if (isEmpty) scala.None else scala.Some(get)
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
   * implicitly convert Option type to Maybe type.
   *
   * The conversion the other way must be explicit by calling toOption
   */

  implicit def toMaybe[T <: AnyRef](o: Option[T]): Maybe[T] = o match {
    case None => Nope
    case Some(x) => One(x)
  }

  /**
   * Maybe(null) returns Nope
   * Maybe(not-null) returns One(not-null)
   */
  @inline
  final def apply[T <: AnyRef](value: T) = if (value == null) Nope else new Maybe[T](value)

  @inline
  final def fromMaybeAnyRef[T <: AnyRef](anyref: Maybe[AnyRef]) = Maybe(
    anyref.v.asInstanceOf[T]
  )

  val Nope = new Maybe[Nothing](NopeValue)

  /**
   * Because Scala doesn't allow derivation from value types, One[T] is not
   * a value class that is a subtype of Maybe[T]. If that were allowed then
   * One could use
   * {{{
   *    def foo(arg: One[String]) = { ...
   *  }}}
   *  As a way of insisting that the argument is, by type-correctness, non-null
   *  and with no overhead. Alas we can't achieve that.
   *
   *  The best we can do is
   *  {{{
   *     def foo(arg: Maybe[String]) = { ...
   *  }}}
   *  This insures that when the argument being passed is constructed as a Maybe
   *  object, that you will get a One or Nope. But it doesn't prevent Nope from
   *  being the result.
   */
  type One[T <: AnyRef] = Maybe[T]

  object One {

    /**
     * One(null) throws an exception.
     */
    final def apply[T <: AnyRef](value: T) =
      if (value eq null) throw new NoSuchElementException("Cannot create One(..) for null")
      else new Maybe[T](value)

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
      else if (thing eq NopeValue)
        Assert.usageError(
          "Maybe.WithNulls.isDefined not for use on Maybe[T] objects, but T (or null) objects."
        )
      else true
    }

    @inline final def get[T <: AnyRef](thing: T): T = {
      if (!isDefined(thing))
        throw new NoSuchElementException("get on undefined value: " + thing)
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
