package edu.illinois.ncsa.daffodil.schema

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
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

/**
 * Diffable is a facility for comparing objects for equality, and providing valuable diagnostic capability
 * when they are different by capturing why they are different.
 *
 * The theme here is "Design for Test" - unit tests in particular need to compare rich data structures.
 * Equality will tell you if the test passes or not, but be very little help in isolating why the structures
 * are not equal. Diff fixes this by saving the point at which the structures are different, and returning that
 * as the diagnostic of why they aren't equal.
 *
 * At a small cost in performance, it is better to implement diff than equals. Equals is realized in terms of diff
 * for classes that mix in trait Diffable.
 *
 * If a class does NOT have a diff method, and does not inherit one from a parent class, then it is compared via
 * equals to compute differences.
 */
//trait Diffable {
//  
//  def diff(o:Any) : Similarity
//  
//  /**
//   * We define equals in terms of diff so that we don't have to maintain two very analogous pieces of code.
//   */
//  override def equals(o:Any) : Boolean = {
//    o match {
//      case x:Diffable => {
//        val diff = Diffable.diff(this, x)
//        diff == Same
//      }
//      case _ => false
//    }
//  }
//}
//
//object Diffable {
//  def diffLists(a: List[Diffable], xa: List[Diffable]): Similarity = {
//    var subDiff: Similarity = Same
//    if (a.length != xa.length) return DifferentLength(a, xa)
//    (a, xa).zipped map { (a: Diffable, xa: Diffable) =>
//      subDiff = a.diff(xa)
//      if (subDiff != Same) return subDiff
//    }
//    subDiff
//  }
//  
//  def diff(a :Any, b:Any) : Similarity = {
//    a match {
//      case a1 : Diffable => a1.diff(b)
//      case _ => b match {
//        case b1 : Diffable => b1.diff(a)
//        case _ => {
//          if (a == b) Same // degenerates to equality checking. Danger! equality can be defined in terms of diff! It *should* be ok....
//          else Different(a, b)
//        }
//      }
//    }
//  }
//}
//
//sealed abstract class Similarity
//case object Same extends Similarity
//case object Different {
//  def apply(a : Any, b : Any) : Difference = {
//    System.err.println("can we stop here?")
//    new Difference(a, b, null, null) 
//  }
//  
//  val r = new scala.util.Random // try using random numbers to defeat any constant folding
//  
//  @volatile var dummy : Int = 0
//  
//  def breakpoint(arg : Int) : Int = {
//    sideEffect(arg)
//    return dummy // a line for a breakpoint
//  }
//  
//  def sideEffect(arg : Int) {
//    if (arg == 0) return
//    dummy = arg - 1
//    sideEffect(dummy) // maybe true recursion (not tail recursion) will defeat it.
//    // Scala not letting me breakpoint in these constructors. Wierd.
//    // Try to force it to give me someplace for a breakpoint.
//    System.err.println("breakpoint " + dummy)
//  }
//}
//
//class Difference(a : Any, b : Any, location : String, objectWithin : Any, 
//    dummy : Int // the purpose of this arg is that this 5 arg constructor is quasi-private. You are supposed to use the
//                   // case object as a factory. But we want a "real constructor" that we can put a breakpoint into.
//                   // TBD is there a better idiom for all this?
//    ) extends Similarity {
//  
//  val _dummy = dummy
// 
//  /**
//   * constructor is here just to allow us to call the breakpoint routine allowing us to debug when 
//   * differences are detected.
//   */
//  def this(a : Any, b : Any, location : String, objectWithin : Any) = 
//    this(a, b, location, objectWithin, Different.breakpoint(Different.r.nextInt(2)))
//
//  override def toString : String = {
//    val aStr = if (a == null) "null" else a.toString()
//    val bStr = if (b == null) "null" else b.toString()
//    "Difference(" + aStr + ", " + bStr + ")"
//  }
//}
//case class DifferentLength(a : Any, b : Any) extends Similarity
//case object DifferentType extends Similarity

