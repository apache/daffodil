/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil

import scala.annotation.implicitNotFound
import org.apache.daffodil.exceptions.Assert

/**
 * Strongly typed equality operators
 *
 * Use =:= to compare two things where they should be the same type, or subtypes.
 * We use a ":" in the operator because we use : for types e.g., x:T
 * E.g., List(1, 2, 3) =:= Seq(1, 2, 3)
 *
 * Use =#= to compare two things that must be convertible (commonly, numbers
 * hence the # symbol)
 * E.g., 5.toLong =#= 5.toByte
 *
 */
package object equality {

  def EqualitySuppressUnusedImportWarning() = {
    if (scala.math.random.isNaN) Assert.impossible()
  }

  // Convertible types - strongly typed equality

  implicit class ViewEqual[T](val left: T) extends AnyVal {
    @inline def =#=(right: T) = left == right
    @inline def !=#=(right: T) = left != right
  }
  //  implicit class ViewEqual[L](val left: L) extends AnyVal {
  //    def =#=[R](right: R)(implicit equality: ViewEquality[L, R]): Boolean =
  //      equality.areEqual(left, right)
  //    @inline def !=#=[R](right: R)(implicit equality: ViewEquality[L, R]): Boolean =
  //      !equality.areEqual(left, right)
  //  }
  //  @implicitNotFound("View equality requires ${L} and ${R} to be in an implicit conversion relationship, i.e. one can be viewed as the other!")
  //  private[equality] sealed trait ViewEquality[L, R] extends Any {
  //    def areEqual(left: L, right: R): Boolean
  //  }
  //
  //  private[equality] object ViewEquality extends LowPriorityViewEqualityImplicits {
  //    implicit def rightToLeftEquality[L, R](implicit view: R => L): ViewEquality[L, R] =
  //      new RightToLeftViewEquality(view)
  //  }
  //  private[equality] trait LowPriorityViewEqualityImplicits extends Any {
  //    implicit def leftToRightEquality[L, R](implicit view: L => R): ViewEquality[L, R] =
  //      new LeftToRightViewEquality(view)
  //  }
  //
  //  /**
  //   * At least in theory this is a value class and so has no representation.
  //   *
  //   * Unfortunately it does seem that if you use =#= then if you examine
  //   * the byte code there is a call to NEW. But it might be for some closure.
  //   * Associated with the implicit view converter being passed.
  //   * It doesn't seem to correspond directly to the calls to new in the ViewEquality
  //   * object's methods.
  //   */
  //  private[equality] class LeftToRightViewEquality[L, R](val view: L => R) extends AnyVal with ViewEquality[L, R] {
  //    override def areEqual(left: L, right: R): Boolean =
  //      view(left) == right
  //  }
  //  private[equality] class RightToLeftViewEquality[L, R](val view: R => L) extends AnyVal with ViewEquality[L, R] {
  //    override def areEqual(left: L, right: R): Boolean =
  //      left == view(right)
  //  }

  // Type wise - allows bi-directional subtypes, not just subtype on right.

  implicit class TypeEqual[L <: AnyRef](val left: L) extends AnyVal {
    @inline def =:=[R <: AnyRef](right: R)(implicit equality: TypeEquality[L, R]): Boolean =
      equality.areEqual(left, right)
    @inline def !=:=[R <: AnyRef](right: R)(implicit equality: TypeEquality[L, R]): Boolean =
      !equality.areEqual(left, right)
    @inline def _eq_[R <: AnyRef](right: R)(implicit equality: TypeEquality[L, R]): Boolean =
      equality.areEq(left, right)
    @inline def _ne_[R <: AnyRef](right: R)(implicit equality: TypeEquality[L, R]): Boolean =
      !equality.areEq(left, right)
  }

  @implicitNotFound("Typed equality requires ${L} and ${R} to be in a subtype relationship!")
  private[equality] sealed trait TypeEquality[L <: AnyRef, R <: AnyRef] {
    def areEqual(left: L, right: R): Boolean
    def areEq(left: L, right: R): Boolean
  }

  private[equality] object TypeEquality extends LowPriorityTypeEqualityImplicits {
    @inline implicit def rightSubtypeOfLeftEquality[L <: AnyRef, R <: L]: TypeEquality[L, R] =
      AnyTypeEquality.asInstanceOf[TypeEquality[L, R]]
  }
  private[equality] trait LowPriorityTypeEqualityImplicits {
    @inline implicit def leftSubtypeOfRightEquality[R <: AnyRef, L <: R]: TypeEquality[L, R] =
      AnyTypeEquality.asInstanceOf[TypeEquality[L, R]]
  }

  // must be public or scala compiler complains that it can't embed the
  // static reference.
  //
  object AnyTypeEquality extends TypeEquality[AnyRef, AnyRef] {
    @inline override def areEqual(left: AnyRef, right: AnyRef): Boolean =
      left == right
    @inline override def areEq(left: AnyRef, right: AnyRef): Boolean =
      left eq right
  }

  // exact type equality
  //
  //  implicit class ExactTypeEqual[L, R](val left: L) {
  //    def =!=[D >: L <: R, D2 >: R <: L](right: R): Boolean = left == right
  //  }

}
