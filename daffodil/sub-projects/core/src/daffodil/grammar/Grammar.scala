package daffodil.grammar

import daffodil.exceptions.Assert
import daffodil.dsom.SchemaComponent
import daffodil.util.Misc._
import daffodil.dsom._
import daffodil.dsom.OOLAG._
import daffodil.util.Debug


trait Gram extends DiagnosticsProviding {

  val name = getNameFromClass(this)
  
  def prettyName = name
  def path = ""

  def isEmpty = false // they are by default not empty. Overridden in the cases where they could be.

  def ~(qq : => Gram) = {
    val q = qq
    if (q.isEmpty)
      if (this.isEmpty) EmptyGram
      else this
    else if (this.isEmpty) q
    else
      new SeqComp(this, q)
  }

  def |(qq : => Gram) = {
    val q = qq
    if (q.isEmpty)
      if (this.isEmpty) EmptyGram
      else this
    else if (this.isEmpty) q
    else
      new AltComp(this, q)
  }

  /**
   * Parser - a Gram can provide a parser, which... parses what the Gram describes
   */
  def parser : Parser

  def unparser : Unparser
}


abstract class UnaryGram(rr : => Gram) extends NamedGram {
  val r = rr
  val gram = {
    if (r.isEmpty) EmptyGram
    else this
  }

  override lazy val diagnosticChildren = if (r.isEmpty) Nil else List(r)
}

abstract class BinaryGram(p : => Gram, q : => Gram) extends Gram {
  def op : String
  def open : String
  def close : String
  override def toString = open + p + " " + op + " " + q + close

  override lazy val diagnosticChildren = List(p, q)
}

class SeqComp(p : => Gram, q : => Gram) extends BinaryGram(p, q) {
  def op = "~"
  def open = ""
  def close = ""

  def parser = new SeqCompParser(p, q)
  def unparser = new SeqCompUnparser(p, q)
}

class AltComp(p : => Gram, q : => Gram) extends BinaryGram(p, q) {
  def op = "|"
  def open = "("
  def close = ")"
  def parser = new AltCompParser(p, q)
  def unparser = new AltCompUnparser(p, q)
}

class RepExactlyN(n : Long, r : => Gram) extends UnaryGram(r) {
  Assert.invariant(n > 0)
  def parser = new RepExactlyNParser(n, r)
  def unparser = new RepExactlyNUnparser(n, r)
}

object RepExactlyN {
  def apply(n : Long, r : => Gram) =
    if (n == 0) EmptyGram
    else new RepExactlyN(n, r)
}

class RepAtMostTotalN(n : Long, r : => Gram) extends UnaryGram(r) {
  def parser = DummyParser(null) // stub
  def unparser = DummyUnparser(null) // stub
}
object RepAtMostTotalN {
  def apply(n : Long, r : => Gram) = EmptyGram // new RepAtMostTotalN(n, r)
}

class RepUnbounded(r : => Gram) extends UnaryGram(r) {
  Assert.invariant(!r.isEmpty)
  def parser = new RepUnboundedParser(r)
  def unparser = new RepUnboundedUnparser(r)
}

object RepUnbounded {
  def apply(r : => Gram) = {
    val rr = r
    if (rr.isEmpty) EmptyGram
    else new RepUnbounded(r)
  }
}

class RepExactlyTotalN(n : Long, r : => Gram) extends UnaryGram(r) {
  def parser = DummyParser(null) // stub
  def unparser = DummyUnparser(null) // stub
  override val gram = EmptyGram
  override def isEmpty = true
}

object RepExactlyTotalN {
  def apply(n : Long, r : => Gram) = new RepExactlyTotalN(n, r)
}

object EmptyGram extends Gram {
  override def isEmpty = true
  override def toString = "Empty"

  override lazy val diagnosticChildren = Nil

  def parser = new EmptyGramParser
  def unparser = new EmptyGramUnparser
}

object ErrorGram extends Gram {
  override def isEmpty = false
  override def toString = "Error"

  override lazy val diagnosticChildren = Nil

  def parser = new ErrorParser
  def unparser = new ErrorUnparser
}

trait NamedGram extends Gram {
  override def toString = name //+ (if (isEmpty) "(Empty)" else "")
}

/**
 * Primitives will derive from this base
 */
abstract class Terminal(sc : Any, guard : Boolean) extends NamedGram {
  override def isEmpty = !guard
  
  lazy val realSC = sc.asInstanceOf[SchemaComponent]
  override lazy val path = realSC.path + "@@" + prettyName
  
  def SDE(str : String, args : Any *) : Nothing = realSC.SDE(str, args)

  lazy val diagnosticChildren = Nil
}

/**
 * Productions will derive from this base.
 *
 * Note the call by name on the GramArg. We don't evaluate the GramArg at all unless the guard is true.
 *
 * Guards are used so we can Gramess grammars that include all possibilities,
 * but where examining the format properties specifically would indicate that some of those
 * possiblities are precluded. The guard causes that term to just splice itself out
 * of the grammar.
 */
class Prod(nameArg : String, val sc : SchemaComponent, guardArg : => Boolean, gramArg : => Gram)
  extends NamedGram {
  
  def SDE(str : String, args : Any *) : Nothing = sc.SDE(str, args)

  override val name = nameArg
  
  override lazy val path = sc.path + "@@Prod(" + prettyName + ")"

  lazy val containingClassName = getNameFromClass(sc)
 
  lazy val guard = LV{
    guardArg
  }
   
  lazy val gram = LV{
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
  private lazy val parser_ = LV{
    gram.parser
  }

  // TODO. Remove override 
  //override lazy val unparser = gram.unparser
  lazy val unparser = unparser_.value
  private lazy val unparser_ = LV{
    gram.unparser
  }
  
  
//  override def toString = {
//    val body = if (!guard) EmptyGram.toString else gram.toString
//    // name + "(" + body + ")"
//    body
//  }
}

object Prod {
  def apply(nameArg : String, sc : SchemaComponent, gram : => Gram) = new Prod(nameArg, sc, true, gram)
  
  def apply(nameArg : String, sc : SchemaComponent, guard : => Boolean, gram : => Gram) = new Prod(nameArg, sc, guard, gram)
}
