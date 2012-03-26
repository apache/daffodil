package daffodil.grammar

import daffodil.exceptions.Assert
import daffodil.dsom.SchemaComponent
import daffodil.util.Misc._

abstract class Gram(nameArg : String = "") {
    
  val name = if (nameArg == "") getNameFromClass(this) else nameArg
  
  def isEmpty = false // they are by default not empty Gramessions. Overridden in the cases where they could be.
  
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
   * Parser - an Gram can provide a parser, which... parses what the Gram describes
   */
  def parser : Parser
  
}

abstract class UnaryGram(rr : => Gram) extends NamedGram {
  
  val Gram = {
    val r = rr
    if (r.isEmpty) EmptyGram
    else this
  }

}

abstract class BinaryGram(p : => Gram, q : => Gram) extends Gram {
  def op : String
  def open : String
  def close : String 
  override def toString = open + p + " " + op + " " + q + close
}

class SeqComp(p : => Gram, q : => Gram) extends BinaryGram(p, q) {
  def op = "~"
  def open = ""
  def close = ""
    
  def parser = new SeqCompParser(p, q)
}

class AltComp(p : => Gram, q : => Gram) extends BinaryGram(p, q) {
  def op = "|"
  def open = "("
  def close = ")"
  def parser = new AltCompParser(p, q)
}

class RepExactlyN(n : Long, r : => Gram) extends UnaryGram(r) {
    def parser = new RepExactlyNParser(n, r)
}
object RepExactlyN {
  def apply(n : Long, r : => Gram) = new RepExactlyN(n, r)
}

class RepAtMostTotalN(n : Long, r : => Gram) extends UnaryGram(r) {
    def parser = DummyParser(null) // stub
}
object RepAtMostTotalN {
  def apply(n : Long, r : => Gram) = EmptyGram // new RepAtMostTotalN(n, r)
}

class RepUnbounded(r : => Gram) extends UnaryGram(r) {
    def parser = new RepUnboundedParser(r)
}
object RepUnbounded {
  def apply(r : => Gram) = new RepUnbounded(r)
}

class RepExactlyTotalN(n : Long, r : => Gram) extends UnaryGram(r) {
    def parser = DummyParser(null) // stub
    override val Gram = EmptyGram
    override def isEmpty = true
}

object RepExactlyTotalN {
  def apply(n : Long, r : => Gram) = new RepExactlyTotalN(n, r)
}

object EmptyGram extends Gram {
  override def isEmpty = true
  override def toString = "Empty"
    
  def parser = new EmptyGramParser
}



abstract class NamedGram(nameArg : String = "") extends Gram(nameArg) {
    override def toString = name + (if (isEmpty) "(Empty)" else "")
}

/**
 * Primitives will derive from this base
 */
abstract class Terminal(sc: Any, guard : Boolean) extends NamedGram {
  override def isEmpty = !guard
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
class Prod(nameArg : String, sc : SchemaComponent, guard : Boolean, gramArg : => Gram) extends NamedGram(nameArg) {
  
  val containingClassName = getNameFromClass(sc)

  val gram = {
    if (guard) {
      System.err.println("Start Prod " + containingClassName + ".Prod." + name)
      val e = gramArg
      System.err.print("End Prod " + containingClassName + ".Prod." + name)
      if (e.isEmpty)
        System.err.println(" empty.")
      else
        System.err.println(" ok:" + e)
      e
    } 
    else {
      System.err.print("Prod " + name)
      System.err.println(" empty.")
      EmptyGram
    }
  }
  
//  /**
//   * Constructor overloads let you specify just guard (for stubbing things really), 
//   * or just grammar production (which means no guard) or both.
//   */
//  def this(nameArg : String, sc : SchemaComponent, gram : Gram ) = this(nameArg, sc, true, gram)
//  def this(nameArg : String, sc : SchemaComponent, guard : Boolean ) = this(nameArg, sc, { assert(guard == false); false}, EmptyGram)
//  
  override def isEmpty = gram.isEmpty
  
  def parser = gram.parser
  
  override def toString = {
    val body = if (!guard) EmptyGram.toString else gram.toString
    // name + "(" + body + ")"
    body
  }
}

object Prod {
  def apply(nameArg : String, sc : SchemaComponent, gram : => Gram ) = new Prod(nameArg, sc, true, gram)
  def apply(nameArg : String, sc : SchemaComponent, guard : Boolean ) = {
    Assert.usage(guard == false)
    new Prod(nameArg, sc, guard, EmptyGram)
  }
  def apply(nameArg : String, sc : SchemaComponent, guard : Boolean, gram : => Gram) = new Prod(nameArg, sc, guard, gram)
}
