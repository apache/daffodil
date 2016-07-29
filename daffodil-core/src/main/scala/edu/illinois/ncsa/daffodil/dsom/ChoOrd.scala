/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.exceptions.Assert

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
