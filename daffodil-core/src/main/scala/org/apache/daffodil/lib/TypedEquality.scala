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

package org.apache.daffodil.lib

import scala.annotation.implicitNotFound

import org.apache.daffodil.lib.exceptions.Assert

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
    // $COVERAGE-OFF$
    if (scala.math.random().isNaN) Assert.impossible()
    // $COVERAGE-ON$
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
