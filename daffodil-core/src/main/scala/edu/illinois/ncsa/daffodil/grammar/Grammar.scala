package edu.illinois.ncsa.daffodil.grammar

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

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.util.Misc.getNameFromClass
import edu.illinois.ncsa.daffodil.Implicits._
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.util.Info
import edu.illinois.ncsa.daffodil.util.Compile
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.LengthUnits
import edu.illinois.ncsa.daffodil.dsom.DiagnosticUtils._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.OOLAGHost
import edu.illinois.ncsa.daffodil.util.Logging

abstract class Gram(contextArg: AnnotatedSchemaComponent)
  extends OOLAGHost(contextArg) {

  def rethrowAsDiagnostic(th: Throwable) = contextArg.rethrowAsDiagnostic(th)

  val context: AnnotatedSchemaComponent = contextArg

  def deref = this

  // override def addDiagnostic(diag: Diagnostic) = context.addDiagnostic(diag)

  def name: String = prettyName

  override def path = context.path + "%" + prettyName

  def isEmpty = false // they are by default not empty. Overridden in the cases where they could be.
  def ~(qq: => Gram) = {
    val q = qq.deref
    val self = this.deref
    //
    // The Nada terminal also behaves like empty for sequential composition
    // It is not empty for alternative composition though.
    //
    val res =
      if (q.isEmpty || q.isInstanceOf[Nada]) // Nada might get through to this point. Let's optimize it out.
        if (self.isEmpty || self.isInstanceOf[Nada]) EmptyGram
        else self
      else if (self.isEmpty || self.isInstanceOf[Nada]) q
      else {
        Assert.invariant(!self.isInstanceOf[Nada])
        Assert.invariant(!self.isEmpty)
        Assert.invariant(!q.isInstanceOf[Nada])
        Assert.invariant(!q.isEmpty)
        SeqComp(context, self, q)
      }
    res
  }

  def |(qq: => Gram) = {
    val q = qq.deref
    val self = this.deref
    if (q.isEmpty)
      if (self.isEmpty) EmptyGram
      else self
    else if (self.isEmpty) q
    else
      AltComp(context, self, q)
  }

  /**
   * Parser - a Gram can provide a parser, which... parses what the Gram describes
   */
  def parser: Parser

  def unparser: Unparser
}

abstract class UnaryGram(context: Term, rr: => Gram) extends NamedGram(context) {
  lazy val r = rr
  lazy val gram = {
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
abstract class BinaryGram(context: AnnotatedSchemaComponent, childrenArg: Seq[Gram]) extends Gram(context) {

  requiredEvaluations(children)

  def op: String
  def open: String
  def close: String

  lazy val children = {
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

object SeqComp {
  def apply(context: AnnotatedSchemaComponent, p: => Gram, q: => Gram) = {
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

class SeqComp private (context: AnnotatedSchemaComponent, children: Seq[Gram]) extends BinaryGram(context, children) {
  def op = "~"
  def open = "("
  def close = ")"

  Assert.invariant(!children.exists { _.isInstanceOf[Nada] })

  def parser = new SeqCompParser(context.runtimeData, children)
  def unparser = new SeqCompUnparser(context, children)
}

object AltComp {
  def apply(context: AnnotatedSchemaComponent, p: => Gram, q: => Gram) = {
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

class AltComp private (context: AnnotatedSchemaComponent, children: Seq[Gram]) extends BinaryGram(context, children) {
  def op = "|"
  def open = "["
  def close = "]"

  def parser = new AltCompParser(context.runtimeData, children)
  def unparser = new AltCompUnparser(context, children)
}

object EmptyGram extends Gram(null) {
  override def isEmpty = true
  override def toString = "Empty"

  def parser = new EmptyGramParser
  def unparser = new EmptyGramUnparser
}

object ErrorGram extends Gram(null) {
  override def isEmpty = false
  override def toString = "Error"

  def parser = new ErrorParser
  def unparser = new ErrorUnparser
}

abstract class NamedGram(context: AnnotatedSchemaComponent) extends Gram(context) {
  // Note: keep the toString really simple.
  // It causes much grief if toString uses complicated things that can fail or 
  // that end up needing the name of this NamedGram again.
  override def toString = name // + "(" + context.scPath.last + ")" //+ (if (isEmpty) "(Empty)" else "")

}

/**
 * Primitives will derive from this base
 */
abstract class Terminal(contextArg: AnnotatedSchemaComponent, guard: Boolean)
  extends NamedGram(contextArg) {
  override def isEmpty = !guard

  lazy val realSC = context.asInstanceOf[SchemaComponent]
  override lazy val path = realSC.path + "@@" + prettyName

  override def toString = path // dangerous. What if realSC.path fails?

  def SDE(str: String, args: Any*): Nothing = realSC.SDE(str, args: _*)

}

/**
 * Productions will derive from this base.
 *
 * Note the call by name on the GramArg. We don't evaluate the GramArg at all unless the guard is true.
 *
 * Guards are used so we can have grammars that include all possibilities,
 * but where examining the format properties specifically would indicate that some of those
 * possibilities are precluded. The guard causes that term to just splice itself out
 * of the grammar.
 *
 * Note that it is crucial that the guardArg is passed by value, and the gramArg is
 * passed by name, and not evaluated until the guard is known true.
 */
class Prod(nameArg: String, val sc: Term, guardArg: Boolean, gramArg: => Gram)
  extends NamedGram(sc) {

  override def deref = gram

  def SDE(str: String, args: Any*): Nothing = sc.SDE(str, args)

  override lazy val name = nameArg

  override lazy val path = sc.path + "@@Prod(" + prettyName + ")"

  lazy val containingClassName = getNameFromClass(sc)

  lazy val guard = guard_.value
  private val guard_ = LV('guard) {
    guardArg
  }

  lazy val gram = gram_.value
  private val gram_ = LV('gram) {
    guard match {
      case true => {
        gramArg
      }
      case false => {
        log(Compile("Prod %s removed.", name))
        EmptyGram
      }
    }
  }

  //  /**
  //   * Constructor overloads let you specify just guard (for stubbing things really), 
  //   * or just grammar production (which means no guard) or both.
  //   */
  //  def this(nameArg : String, sc : SchemaComponent, gram : Gram ) = this(nameArg, sc, true, gram)
  //  def this(nameArg : String, sc : SchemaComponent, guard : Boolean ) = this(nameArg, sc, { assert(guard == false); false}, EmptyGram)
  //  
  override lazy val isEmpty = gram.isEmpty

  lazy val parser = parser_.value
  private val parser_ = LV('parser) {
    gram.parser
  }

  lazy val unparser = unparser_.value
  private val unparser_ = LV('unparser) {
    gram.unparser
  }
}

object Prod {

  // This inner object is about allowing a curried form of Prod object creation
  // where the regular args come first, and the nested grammar part is a body section that comes 
  // after. This is a bit cleaner to use and is a tiny small step in cleanup of
  // the grammar part of Daffodil.
  object prod {
    def apply(nameArg: String, sc: Term)(gram: => Gram): Prod = new Prod(nameArg, sc, true, gram)
    def apply(nameArg: String, sc: Term, guard: Boolean)(gram: => Gram): Prod = new Prod(nameArg, sc, guard, gram)
  }

  // If there is a guard, then delay evaluating the gram until we know if 
  // the guard is satisfied. That's why we pass gram by-name.
  //@deprecated("use prod(nameArg, sc, guard) {...body...} form", "2013-10-01")
  def apply(nameArg: String, sc: Term, guard: Boolean, gram: => Gram): Prod = new Prod(nameArg, sc, guard, gram)

  // there is no guard in this apply signature
  // here, so in principle there is no reason to pass by-name the
  // gram. However, keeping it this way insures uniform behavior if you are say,
  // walking through productions in a debugger. 
  //@deprecated("use prod(nameArg, sc) {...body...} form", "2013-10-01")
  def apply(nameArg: String, sc: Term, gram: => Gram): Prod = new Prod(nameArg, sc, true, gram)

}

