package daffodil.grammar

import daffodil.exceptions.Assert
import daffodil.util.Debug
import daffodil.util.Misc.getNameFromClass
import daffodil.dsom.{ SchemaComponent, Term, DiagnosticsProviding}


abstract class Gram(val context: Term) extends DiagnosticsProviding {
  def deref = this

  val name = getNameFromClass(this)
  def prettyName = name
  def path = ""

  def isEmpty = false // they are by default not empty. Overridden in the cases where they could be.
  def ~(qq : => Gram) = {
    val q = qq.deref
    val self = this.deref
    //
    // The Nada terminal also behaves like empty for sequential composition
    // It is not empty for alternative composition though.
    //
    if (q.isEmpty || q.isInstanceOf[Nada]) // Nada might get through to this point. Let's optimize it out.
      if (self.isEmpty || self.isInstanceOf[Nada]) EmptyGram
      else self
    else if (self.isEmpty || self.isInstanceOf[Nada]) q
    else {
      Assert.invariant(!self.isInstanceOf[Nada])
      Assert.invariant(!self.isEmpty)
      Assert.invariant(!q.isInstanceOf[Nada])
      Assert.invariant(!q.isEmpty)
      new SeqComp(context, self, q)
    }
  }

  def |(qq : => Gram) = {
    val q = qq.deref
    val self = this.deref
    if (q.isEmpty)
      if (self.isEmpty) EmptyGram
      else self
    else if (self.isEmpty) q
    else
      new AltComp(context, self, q)
  }

  /**
   * Parser - a Gram can provide a parser, which... parses what the Gram describes
   */
  def parser: Parser

  def unparser: Unparser
}

abstract class UnaryGram(context: Term, rr: => Gram) extends NamedGram(context) {
  val r = rr
  val gram = {
    if (r.isEmpty) EmptyGram
    else this
  }

  override lazy val diagnosticChildren = if (r.isEmpty) Nil else List(r)
}

abstract class BinaryGram(context: Term, p: => Gram, q: => Gram) extends Gram(context) {
  def op: String
  def open: String
  def close: String
  override def toString = open + p + " " + op + " " + q + close

  override lazy val diagnosticChildren = List(p, q)
}

class SeqComp(context: Term, p: => Gram, q: => Gram) extends BinaryGram(context, p, q) {
  def op = "~"
  def open = ""
  def close = ""
    
  Assert.invariant(!p.isInstanceOf[Nada])

  def parser = new SeqCompParser(context, p, q)
  def unparser = new SeqCompUnparser(context, p, q)
}

class AltComp(context: Term, p: => Gram, q: => Gram) extends BinaryGram(context, p, q) {
  def op = "|"
  def open = "("
  def close = ")"

  def parser = new AltCompParser(context, p, q)
  def unparser = new AltCompUnparser(context, p, q)
}

class RepExactlyN(context: Term, n: Long, r: => Gram) extends UnaryGram(context, r) {
  Assert.invariant(n > 0)

  def parser = new RepExactlyNParser(context, n, r)
  def unparser = new RepExactlyNUnparser(context, n, r)
}

object RepExactlyN {
  def apply(context: Term, n: Long, r: => Gram) =
    if (n == 0) EmptyGram
    else new RepExactlyN(context, n, r)
}

class RepAtMostTotalN(context: Term, n: Long, r: => Gram) extends UnaryGram(context, r) {
  def parser = DummyParser(null) // stub
  def unparser = DummyUnparser(null) // stub
}
object RepAtMostTotalN {
  def apply(context: Term, n: Long, r: => Gram) = EmptyGram // new RepAtMostTotalN(n, r)
}

class RepUnbounded(context: Term, r: => Gram) extends UnaryGram(context, r) {
  Assert.invariant(!r.isEmpty)

  def parser = new RepUnboundedParser(context, r)
  def unparser = new RepUnboundedUnparser(context, r)
}

object RepUnbounded {
  def apply(context: Term, r: => Gram) = {
    val rr = r
    if (rr.isEmpty) EmptyGram
    else new RepUnbounded(context, r)
  }
}

class RepExactlyTotalN(context: Term, n: Long, r: => Gram) extends UnaryGram(context, r) {
  def parser = DummyParser(null) // stub
  def unparser = DummyUnparser(null) // stub
  override val gram = EmptyGram
  override def isEmpty = true
}

object RepExactlyTotalN {
  def apply(context: Term, n: Long, r: => Gram) = new RepExactlyTotalN(context, n, r)
}

object EmptyGram extends Gram(null) {
  override def isEmpty = true
  override def toString = "Empty"

  override lazy val diagnosticChildren = Nil

  def parser = new EmptyGramParser
  def unparser = new EmptyGramUnparser
}

object ErrorGram extends Gram(null) {
  override def isEmpty = false
  override def toString = "Error"

  override lazy val diagnosticChildren = Nil

  def parser = new ErrorParser
  def unparser = new ErrorUnparser
}

abstract class NamedGram(context: Term) extends Gram(context) {
  override def toString = name //+ (if (isEmpty) "(Empty)" else "")
}

/**
 * Primitives will derive from this base
 */
abstract class Terminal(context: Term, guard: Boolean) extends NamedGram(context) {
  override def isEmpty = !guard

  lazy val realSC = context.asInstanceOf[SchemaComponent]
  override lazy val path = realSC.path + "@@" + prettyName

  def SDE(str: String, args: Any*): Nothing = realSC.SDE(str, args)

  lazy val diagnosticChildren = Nil
}

/**
 * Productions will derive from this base.
 *
 * Note the call by name on the GramArg. We don't evaluate the GramArg at all unless the guard is true.
 *
 * Guards are used so we can Gramess grammars that include all possibilities,
 * but where examining the format properties specifically would indicate that some of those
 * possibilities are precluded. The guard causes that term to just splice itself out
 * of the grammar.
 */
class Prod(nameArg: String, val sc: Term, guardArg: => Boolean, gramArg: => Gram)
  extends NamedGram(sc) {
  
  override def deref = gram
  
  def SDE(str : String, args : Any *) : Nothing = sc.SDE(str, args)

  override val name = nameArg

  override lazy val path = sc.path + "@@Prod(" + prettyName + ")"

  lazy val containingClassName = getNameFromClass(sc)

  lazy val guard = LV {
    guardArg
  }

  lazy val gram = LV {
    guard.value match {
      case true => {
        //      System.err.println("Start Prod " + containingClassName + ".Prod." + name)
        gramArg
        //      System.err.print("End Prod " + containingClassName + ".Prod." + name)
        //      if (g.isEmpty)
        //        System.err.println(" empty.")
        //      else
        //        System.err.println(" ok:" + g)
      }
      case false => {
        log(Debug("Prod %s removed.", name))
        EmptyGram
      }
      // case None => ErrorGram
    }
  }

  lazy val diagnosticChildren = List(gram)

  //  /**
  //   * Constructor overloads let you specify just guard (for stubbing things really), 
  //   * or just grammar production (which means no guard) or both.
  //   */
  //  def this(nameArg : String, sc : SchemaComponent, gram : Gram ) = this(nameArg, sc, true, gram)
  //  def this(nameArg : String, sc : SchemaComponent, guard : Boolean ) = this(nameArg, sc, { assert(guard == false); false}, EmptyGram)
  //  
  override lazy val isEmpty = gram.isEmpty

  lazy val parser = parser_.value
  private lazy val parser_ = LV {
    gram.parser
  }

  lazy val unparser = unparser_.value
  private lazy val unparser_ = LV {
    gram.unparser
  }

  //  override def toString = {
  //    val body = if (!guard) EmptyGram.toString else gram.toString
  //    // name + "(" + body + ")"
  //    body
  //  }
}

object Prod {
  def apply(nameArg: String, sc: Term, gram: => Gram) = new Prod(nameArg, sc, true, gram)

  def apply(nameArg: String, sc: Term, guard: => Boolean, gram: => Gram) = new Prod(nameArg, sc, guard, gram)
}

