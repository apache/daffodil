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

package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.unparsers.UState

trait Dynamic {

  type CachedDynamic[A] = Either[CompiledExpression, A]

  // Returns an Either, with Right being the value of the constant, and the
  // Left being the a non-constant compiled expression. The conv variable is
  // used to convert the constant value to a more usable form, and perform and
  // SDE checks. This should be called during initialization/compile time. Not
  // during runtime.
  def cacheConstantExpression[A](e: CompiledExpression)(conv: (Any) => A): CachedDynamic[A] = {
    if (e.isConstant) {
      val v: Any = e.constant
      Right(conv(v))
    } else {
      Left(e)
    }
  }

  def cacheConstantExpression[A](oe: Maybe[CompiledExpression])(conv: (Any) => A): Maybe[CachedDynamic[A]] = {
    //oe.map { e => cacheConstantExpression[A](e)(conv) }
    if (oe.isDefined) One(cacheConstantExpression[A](oe.get)(conv))
    else Nope
  }

  def cacheConstantExpression[A](listOfE: List[CompiledExpression])(conv: (Any) => A): List[CachedDynamic[A]] = {
    listOfE.map { e => cacheConstantExpression[A](e)(conv) }
  }

  // For any expression that couldn't be evaluated in cacheConstantExpression,
  // this evaluates that. This is used to evaluate only runtime expressions.
  // This also carries along PState that is modified during expression
  // evaluation.
  def evalWithConversion[A](s: PState, e: CachedDynamic[A])(conv: (PState, Any) => A): A = {
    e match {
      case Right(r) => r
      case Left(l) => {
        val (aAsAny, newVMap) = l.evaluate(s)
        s.status match {
          case Success => // nothing
          case f: Failure => {
            // evaluation failed
            // we can't continue this code path
            // have to throw out of here
            throw f.cause
          }
        }
        val a: A = conv(s, aAsAny)
        s.setVariables(newVMap)
        a
      }
    }
  }
  // For any expression that couldn't be evaluated in cacheConstantExpression,
  // this evaluates that. This is used to evaluate only runtime expressions.
  // This also carries along PState that is modified during expression
  // evaluation.
  def evalWithConversion[A](s: UState, e: CachedDynamic[A])(conv: (UState, Any) => A): A = {
    e match {
      case Right(r) => r
      case Left(l) => {
        val (aAsAny, newVMap) = l.evaluate(s)
        s.status match {
          case Success => // nothing
          case f: Failure => {
            // evaluation failed
            // we can't continue this code path
            // have to throw out of here
            throw f.cause
          }
        }
        val a: A = conv(s, aAsAny)
        //
        // FIXME ?? Why not set the variables here?? This was commented out, but 
        // it seems like it should be setting them.
        //
        //s.setVariables(newVMap)
        a
      }
    }
  }

  def evalWithConversion[A](s: PState, oe: Maybe[CachedDynamic[A]])(conv: (PState, Any) => A): Maybe[A] = {
    if (oe.isDefined) {
      val a = evalWithConversion[A](s, oe.get)(conv)
      One(a)
    } else Nope
  }
  def evalWithConversion[A](s: UState, oe: Maybe[CachedDynamic[A]])(conv: (UState, Any) => A): Maybe[A] = {
    if (oe.isDefined) {
      val a = evalWithConversion[A](s, oe.get)(conv)
      One(a)
    } else Nope
  }

  def evalWithConversion[A](s: PState, oe: List[CachedDynamic[A]])(conv: (PState, Any) => A): List[A] = {
    var state = s
    val listE = oe.map(e => {
      val exp = evalWithConversion[A](state, e)(conv)
      exp
    })
    listE
  }
  def evalWithConversion[A](s: UState, oe: List[CachedDynamic[A]])(conv: (UState, Any) => A): List[A] = {
    var state = s
    val listE = oe.map(e => {
      val exp = evalWithConversion[A](state, e)(conv)
      exp
    })
    listE
  }

  // With an property that can potentially be compiled, this returns an Option,
  // which is either Some(s) if the value of the property is static, or None
  // otherwise

  def getStatic[A](e: CachedDynamic[A]): Maybe[A] = {
    e match {
      case Left(l) => Nope
      case Right(r) => One(r)
    }
  }

  def getStatic[A](oe: Maybe[CachedDynamic[A]]): Maybe[A] = {
    if (oe.isDefined) getStatic(oe.get)
    else Nope
  }
}