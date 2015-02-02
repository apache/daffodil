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

package edu.illinois.ncsa.daffodil

import scala.annotation.implicitNotFound

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

  // Convertible types - strongly typed equality 

  implicit class ViewEqual[L](val left: L) extends AnyVal {
    def =#=[R](right: R)(implicit equality: ViewEquality[L, R]): Boolean =
      equality.areEqual(left, right)
  }
  @implicitNotFound("View equality requires ${L} and ${R} to be in an implicit conversion relationship, i.e. one can be viewed as the other!")
  private[equality] sealed trait ViewEquality[L, R] {
    def areEqual(left: L, right: R): Boolean
  }
  private[equality] object ViewEquality extends LowPriorityViewEqualityImplicits {
    implicit def rightToLeftEquality[L, R](implicit view: R => L): ViewEquality[L, R] =
      new RightToLeftViewEquality(view)
  }
  private[equality] trait LowPriorityViewEqualityImplicits {
    implicit def leftToRightEquality[L, R](implicit view: L => R): ViewEquality[L, R] =
      new LeftToRightViewEquality(view)
  }
  private class LeftToRightViewEquality[L, R](view: L => R) extends ViewEquality[L, R] {
    override def areEqual(left: L, right: R): Boolean =
      view(left) == right
  }
  private class RightToLeftViewEquality[L, R](view: R => L) extends ViewEquality[L, R] {
    override def areEqual(left: L, right: R): Boolean =
      left == view(right)
  }

  // Type wise - allows bi-directional subtypes, not just subtype on right.

  implicit class TypeEqual[L](val left: L) extends AnyVal {
    def =:=[R](right: R)(implicit equality: TypeEquality[L, R]): Boolean =
      equality.areEqual(left, right)
  }

  @implicitNotFound("Typed equality requires ${L} and ${R} to be in a subtype relationship!")
  private[equality] sealed trait TypeEquality[L, R] {
    def areEqual(left: L, right: R): Boolean
  }
  private[equality] object TypeEquality extends LowPriorityTypeEqualityImplicits {
    implicit def rightSubtypeOfLeftEquality[L, R <: L]: TypeEquality[L, R] =
      AnyTypeEquality.asInstanceOf[TypeEquality[L, R]]
  }
  private[equality] trait LowPriorityTypeEqualityImplicits {
    implicit def leftSubtypeOfRightEquality[R, L <: R]: TypeEquality[L, R] =
      AnyTypeEquality.asInstanceOf[TypeEquality[L, R]]
  }
  private object AnyTypeEquality extends TypeEquality[Any, Any] {
    override def areEqual(left: Any, right: Any): Boolean =
      left == right
  }

  // exact type equality
  //
  //  implicit class ExactTypeEqual[L, R](val left: L) {
  //    def =!=[D >: L <: R, D2 >: R <: L](right: R): Boolean = left == right
  //  }

}