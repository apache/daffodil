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

package org.apache.daffodil.util

import org.apache.daffodil.exceptions.Assert

/**
 * Delay[T]
 *
 * This Delayed evaluation technique is an alternative to staggered
 * multi-stage factories, or currying. Turns out currying works fine in
 * Scala for functions, but not for class constructors, so it's easier to
 * make the class constructor take the usual bunch of arguments, but the
 * ones we might have wanted to supply later, we instead supply them, but
 * as Delay objects.
 *
 * For more info, this is used in the IncludeImport stuff in DSOM, and
 * in numerous other places where we construct, functionally, cyclic graphs.
 *
 * The idiom is to pass a Delay object to a constructor, and have the object
 * constructed have a public initialize method (lazy val really) which
 * forces the delays.
 *
 * Suppose we wanted a tuple-like pair to point at itself. It's a silly example,
 * but illustrates what this is achieving.
 * {{{
 *
 *   /** Like a Lisp "Cons Cell" with Car and "Cudder" values. */
 *   class Cons(carDelay: Delay[Cons], cdrDelay: Delay[Cons]) extends Serializable {
 *     lazy val car = carDelay.value
 *     lazy val cdr = cdrDelay.value
 *
 *     lazy val initialize: Unit = {
 *       car
 *       cdr
 *     }
 *   }
 *
 *   // in Lisp, Nil behaves like a Cons where car and cdr are both Nil also.
 *   lazy val LispNil = new Cons(Delay('car, this, LispNil), Delay('cdr, this, LispNil))
 *
 *
 *   // elsewhere, these must be initialized before serialization or
 *   // serialization will fail because Delay objects cause errors when
 *   // serializing unless their values have been previously forced.
 *
 *   requiredEvaluation(LispNil.initialize)
 *
 *   objectOutputStream.writeObject(LispNil)
 *
 * }}}
 */
final class Delay[T] private (private var box: Delay.Box[T], sym: Symbol)
  extends PreSerialization {
  //
  // This trick of taking another object on a var, and
  // then clobbering the var after we demand the value
  // eliminates holding onto a bunch of objects due
  // to closures for the pass-by-name arguments.
  //
  // The idea is no matter what the Scala implementation is doing in its implementation of
  // by-name args, that's on the constructor of the box object,
  // and we're going to explicitly null-out the reference to the box object
  // which guarantees we're not holding onto things we don't expect
  // once the Delay object has been evaluated.
  //
  lazy val value: T = {
    Assert.invariant(box ne null)
    val v = box.value
    box = null // throws away box, which allows GC of all closures, etc.
    v
  }

  def hasValue = box eq null

  /**
   * For creating a delay object purely to satisfy a type requirement
   * when you know the argument does not actually need to be delayed.
   */
  def force : Delay[T] = { value; this }

  /**
   * Create a string representation. Does not force the value to be computed.
   */
  override def toString = {
    val bodyString = if (hasValue) ", " + value.toString else ""
    val objString = if (hasValue) "" else {
      box
    }
    "Delay(" + sym + ", " + objString + bodyString + ")"
  }

  override def preSerialization = {
    if (!hasValue) {
      Assert.invariant(box ne null)
      val msg = s"No value for delay. Containing object not initialized? ID Symbol:$sym " +
      s"object ${box}"
      Assert.invariantFailed(msg)
    }
    super.preSerialization
  }

  @throws(classOf[java.io.IOException])
  final private def writeObject(out: java.io.ObjectOutputStream): Unit = serializeObject(out)
}

object Delay {
  /**
   * Create a delayed expression object.
   *
   * @param delayedExpression an argument expression which will not be evaluated until required
   * @tparam T type of the argument. (Usually inferred by Scala.)
   * @return the Delay object
   */
  def apply[T](sym: Symbol, obj: AnyRef, delayedExpression: => T) = {
    new Delay(new Box(sym, obj, delayedExpression), sym)
  }

  /**
   * Specifically, this is NOT serializable.
   * Serialization must force all Delay objects.
   */
  private class Box[T](val sym: Symbol, val obj: AnyRef, delayedExpression: => T) {
    lazy val value = delayedExpression

    override def toString() = {
      val desc = obj match {
        case d: NamedMixinBase => "(" + d.diagnosticDebugName + ")"
        case _ => ""
      }
      "box(" + obj.hashCode() + desc + ")"
    }
  }
}
