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

package edu.illinois.ncsa.daffodil.grammar

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.Implicits._; object INoWarn { ImplicitsSuppressUnusedImportWarning() }
import edu.illinois.ncsa.daffodil.processors.unparsers.SeqCompUnparser
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.grammar.primitives.Nada
import edu.illinois.ncsa.daffodil.processors.parsers.SeqCompParser
import edu.illinois.ncsa.daffodil.processors.parsers.AltCompParser
import edu.illinois.ncsa.daffodil.compiler.ForUnparser
import edu.illinois.ncsa.daffodil.compiler.ForParser
import edu.illinois.ncsa.daffodil.processors.unparsers.NadaUnparser
import edu.illinois.ncsa.daffodil.processors.parsers.NadaParser
import edu.illinois.ncsa.daffodil.processors.unparsers.EmptyGramUnparser

abstract class UnaryGram(context: Term, rr: => Gram) extends NamedGram(context) {
  private lazy val r = rr

  final override lazy val gram = {
    if (r.isEmpty) EmptyGram
    else this
  }
}

/**
 * BinaryGram isn't really 'binary' it's n-ary. It is called binary because it comes from
 * the binary grammar operations ~ and |, but in the abstract syntax tree we want
 * these flattened to lists of children so that a ~ b ~ c is ONE SeqComp with 3 children, not a tree
 * of two binary SeqComps.
 */
abstract class BinaryGram(context: SchemaComponent, childrenArg: Seq[Gram]) extends Gram(context) {

  requiredEvaluations(children)

  protected def op: String
  protected def open: String
  protected def close: String

  protected final lazy val children = {
    val c = childrenArg
    c
  }

  override def toString = {
    Assert.invariant(children != null)
    Assert.invariant(open != null)
    Assert.invariant(op != null)
    Assert.invariant(close != null)
    def toStringpq(p: String, q: String) =
      open + p + " " + op + " " + q + close
    val res = children.map { _.toString }.reduce { toStringpq _ }
    res
  }
}

/**
 * Sequential composition of grammar terms.
 *
 * Flattens nests of these into a flat list of terms.
 */
object SeqComp {
  def apply(context: SchemaComponent, p: => Gram, q: => Gram) = {
    val children = (p, q) match {
      case (ps: SeqComp, qs: SeqComp) => {
        ps.children ++ qs.children
      }
      case (ps: SeqComp, _) => ps.children ++ List(q)
      case (_, qs: SeqComp) => p +: qs.children
      case (_, _) => List(p, q)
    }
    val res = new SeqComp(context, children)
    res
  }
}

class SeqComp private (context: SchemaComponent, children: Seq[Gram]) extends BinaryGram(context, children) {
  protected final override def op = "~"
  protected final override def open = "("
  protected final override def close = ")"

  Assert.invariant(!children.exists { _.isInstanceOf[Nada] })

  lazy val parserChildren = children.filter(_.forWhat != ForUnparser).map { _.parser }.filterNot { _.isInstanceOf[NadaParser] }

  final override lazy val parser = {
    if (parserChildren.isEmpty) new NadaParser(context.runtimeData)
    else if (parserChildren.length == 1) parserChildren(0)
    else new SeqCompParser(context.runtimeData, parserChildren.toArray)
  }

  lazy val unparserChildren = children.filter(x => !x.isEmpty && (x.forWhat != ForParser)).map { _.unparser }.filterNot { _.isInstanceOf[NadaUnparser] }

  final override lazy val unparser = {
    if (unparserChildren.isEmpty) new NadaUnparser(context.runtimeData)
    else if (unparserChildren.length == 1) unparserChildren(0)
    else new SeqCompUnparser(context.runtimeData, unparserChildren.toArray)
  }
}

/**
 * Alternative composition of grammar terms.
 *
 * Flattens nests of these into a single flat list.
 */
object AltComp {
  def apply(context: SchemaComponent, p: => Gram, q: => Gram) = {
    val children = (p, q) match {
      case (ps: AltComp, qs: AltComp) => {
        ps.children ++ qs.children
      }
      case (ps: AltComp, _) => ps.children ++ List(q)
      case (_, qs: AltComp) => p +: qs.children
      case (_, _) => List(p, q)
    }
    val res = new AltComp(context, children)
    res
  }
}

class AltComp private (context: SchemaComponent, children: Seq[Gram]) extends BinaryGram(context, children)
  with HasNoUnparser {
  protected final override def op = "|"
  protected final override def open = "["
  protected final override def close = "]"

  final override lazy val parser = new AltCompParser(context.runtimeData, children.map { _.parser })
}

object EmptyGram extends Gram(null) {
  override def isEmpty = true
  override def toString = "Empty"

  override lazy val parser = new NadaParser(null)
  override lazy val unparser = // hasNoUnparser
    // we depend on this unparser being returned, even though it cannot be called to unparse anything.
    // As there are unit tests which test attributes where those attributes cause a DummyUnparser to be created.
    // That is later optimized out (or needs to be).
    //
    // FIXME: switch back to hasNoUnparser, find where this is being used and have it not ask for
    // this unparser.
    new EmptyGramUnparser(null) // DummyUnparser(Misc.getNameFromClass(this))

}

object ErrorGram extends Gram(null) with HasNoUnparser {
  override def isEmpty = false
  override def toString = "Error"

  override lazy val parser = hasNoParser // new ErrorParser
}

abstract class NamedGram(context: SchemaComponent) extends Gram(context) {
  // Note: keep the toString really simple.
  // It causes much grief if toString uses complicated things that can fail or
  // that end up needing the name of this NamedGram again.
  override def toString = name // + "(" + context.scPath.last + ")" //+ (if (isEmpty) "(Empty)" else "")

}

/**
 * Primitives will derive from this base
 */
abstract class Terminal(contextArg: SchemaComponent, guard: Boolean)
  extends NamedGram(contextArg) {

  override def isEmpty = !guard

  private lazy val realSC = context.asInstanceOf[SchemaComponent]
  final override lazy val path = realSC.path + "@@" + diagnosticDebugName

  override def toString = path // dangerous. What if realSC.path fails?

}
