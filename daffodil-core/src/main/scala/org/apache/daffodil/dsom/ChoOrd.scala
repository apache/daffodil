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

package org.apache.daffodil.dsom

import org.apache.daffodil.exceptions.Assert

/**
 * ChoOrd = Choice or Ordered
 *
 * This is a tree representing elements as they appear in ordered
 * sequences or as alternatives to each other.
 *
 * This does not directly correspond to the xs:choice and xs:sequence
 * constructs of a DFDL Schema, though that can be the origin.
 *
 * The purpose of these ChoOrd trees is to establish some
 * robust invariants about the way these are nested.
 *
 * These invariants include:
 * * Elements are at the leaves.
 * * There is no unnecessary nesting. So an Ord only contains
 * leaves or Cho, and a Cho only contains leaves or Ord.
 * * There are no degenerate Cho or Ord with only 1 thing in them. Those get created
 * but squeezed out.
 */
sealed trait ChoOrd
case class Ord(s: Seq[ChoOrd]) extends ChoOrd {
  Assert.invariant(s.find { _.isInstanceOf[Ord] }.isEmpty)
}
case class Cho(s: Seq[ChoOrd]) extends ChoOrd {
  Assert.invariant(s.find { _.isInstanceOf[Cho] }.isEmpty)
}
case class Leaf(e: ElementBase) extends ChoOrd

object ChoOrd {

  object MT extends ChoOrd

  def mkCho(s: Seq[ChoOrd]) =
    if (s.isEmpty) MT
    else {
      val c = s.flatMap {
        case MT => Nil
        case Ord(Seq(x)) => Seq(x)
        case Cho(c) => c
        case x => Seq(x)
      }
      c match {
        case Seq(ch @ Cho(_)) => ch
        case _ => Cho(c)
      }
    }

  def mkOrd(s: Seq[ChoOrd]) =
    if (s.isEmpty) MT
    else {
      val c = s.flatMap {
        case MT => Nil
        case Cho(Seq(x)) => Seq(x)
        case Ord(c) => c
        case x => Seq(x)
      }
      c match {
        case Seq(ord @ Ord(_)) => ord
        case _ => Ord(c)
      }
    }

  /**
   * Leaf objects that are adjacent within an Ord, and which satisfy the test
   * are coalesced into an Ord.
   *
   * This is used to change the Ord from representing elements in schema order
   * to representing all possible next elements - by grouping together adjacent
   * optional elements.
   */
  def groupAdjacentSatisfying(t: ChoOrd, test: Leaf => Boolean): ChoOrd = t match {
    case ChoOrd.MT => t
    case l: Leaf => l
    case Cho(members) => mkCho(members.map { m => groupAdjacentSatisfying(m, test) })
    case Ord(members) => {
      val nestedRuns =
        //
        // This algorithm was surprisingly tricky - coalesce while preserving order,
        // the adjacent leaves satisfying a predicate.
        //
        members.foldLeft(
          Nil.asInstanceOf[Seq[Seq[ChoOrd]]]) {
            // cases are on (accumulatingRuns, nextChoOrd) and produce accumulatingRuns
            // where the first run is lengthened, or a new run is created
            case (Seq(), choOrd) => Seq(Seq(choOrd))
            case (Seq(priorRun @ Seq(prior: Leaf, restRun @ _*), restRuns @ _*), next: Leaf) if test(prior) && test(next) => {
              //
              // incorporate into current run
              //
              val thisRun = next +: prior +: restRun
              val allRuns = thisRun +: restRuns // lengthen current run
              allRuns
            }
            case (restRuns, next) => {
              //
              // start a new run
              //
              val thisRun = Seq(next)
              val allRuns = thisRun +: restRuns
              allRuns
            }
          }.map { _.reverse }.reverse
      val res = mkOrd(nestedRuns.map { seq =>
        seq match {
          case Seq(justOne) => justOne
          case s => mkCho(s)
        }
      })
      res
    }
  }

}
