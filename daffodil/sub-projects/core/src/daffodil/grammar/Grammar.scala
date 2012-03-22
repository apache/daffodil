package daffodil.grammar

import daffodil.exceptions.Assert
import daffodil.dsom.SchemaComponent
import daffodil.util.Misc._

abstract class Expr(nameArg : String = "") {
  

  
  val name = if (nameArg == "") getNameFromClass(this) else nameArg
  
  def isEmpty = false // they are by default not empty expressions. Overridden in the cases where they could be.
  
  def ~(qq : => Expr) = {
    val q = qq
    if (q.isEmpty) 
    	if (this.isEmpty) EmptyExpr
    	else this
    else if (this.isEmpty) q
    else
    	new SeqComp(this, q)
  }
  
  def |(qq : => Expr) = {
    val q = qq
    if (q.isEmpty) 
    	if (this.isEmpty) EmptyExpr
    	else this
    else if (this.isEmpty) q
    else
    	new AltComp(this, q)
  }
  
  /**
   * Parser - an Expr can provide a parser, which... parses what the Expr describes
   */
  def parser : Parser
  
}

abstract class UnaryExpr(r : => Expr) extends Expr {
  override def toString = name + "(" + r + ")"
}

abstract class BinaryExpr(p : => Expr, q : => Expr) extends Expr {
  def op : String
  def open : String
  def close : String 
  override def toString = open + p + " " + op + " " + q + close
}

class SeqComp(p : => Expr, q : => Expr) extends BinaryExpr(p, q) {
  def op = "~"
  def open = ""
  def close = ""
    
  def parser = new SeqCompParser(p, q)
}

class AltComp(p : => Expr, q : => Expr) extends BinaryExpr(p, q) {
  def op = "|"
  def open = "("
  def close = ")"
  def parser = new AltCompParser(p, q)
}

class RepExactlyN(n : Long, r : => Expr) extends UnaryExpr(r) {
    def parser = new RepExactlyNParser
}
object RepExactlyN {
  def apply(n : Long, r : => Expr) = new RepExactlyN(n, r)
}

class RepAtMostTotalN(n : Long, r : => Expr) extends UnaryExpr(r) {
    def parser = DummyParser(null) // stub
}
object RepAtMostTotalN {
  def apply(n : Long, r : => Expr) = new RepAtMostTotalN(n, r)
}

class RepUnbounded(r : => Expr) extends UnaryExpr(r) {
    def parser = DummyParser(null) // stub
}
object RepUnbounded {
  def apply(r : => Expr) = new RepUnbounded(r)
}

class RepExactlyTotalN(n : Long, r : => Expr) extends UnaryExpr(r) {
    def parser = DummyParser(null) // stub
}
object RepExactlyTotalN {
  def apply(n : Long, r : => Expr) = new RepExactlyTotalN(n, r)
}

object EmptyExpr extends Expr {
  override def isEmpty = true
  override def toString = "Empty"
    
  def parser = new EmptyExprParser
}



abstract class NamedExpr(nameArg : String = "") extends Expr(nameArg) {
    override def toString = name + (if (isEmpty) "(Empty)" else "")
}

/**
 * Primitives will derive from this base
 */
abstract class Terminal(sc: Any, guard : Boolean) extends NamedExpr {
  override def isEmpty = !guard
}

/**
 * Productions will derive from this base.
 * 
 * Note the call by name on the exprArg. We don't evaluate the exprArg at all unless the guard is true.
 * 
 * Guards are used so we can express grammars that include all possibilities, 
 * but where examining the format properties specifically would indicate that some of those
 * possiblities are precluded. The guard causes that term to just splice itself out
 * of the grammar.
 */
class Prod(nameArg : String, sc : SchemaComponent, guard : Boolean, exprArg : => Expr) extends NamedExpr(nameArg) {
  
  val containingClassName = daffodil.util.Misc.getNameFromClass(sc)
  val expr = {

    if (guard) {
      System.err.print(containingClassName + ".Prod." + name)
      System.err.println(" ok.")
      exprArg
    } 
    else {
//      System.err.print("Production " + name)
//      System.err.println(" empty.")
      EmptyExpr
    }
  }
  
//  /**
//   * Constructor overloads let you specify just guard (for stubbing things really), 
//   * or just grammar production (which means no guard) or both.
//   */
//  def this(nameArg : String, sc : SchemaComponent, expr : Expr ) = this(nameArg, sc, true, expr)
//  def this(nameArg : String, sc : SchemaComponent, guard : Boolean ) = this(nameArg, sc, { assert(guard == false); false}, EmptyExpr)
//  
  override def isEmpty = expr.isEmpty
  
  def parser = expr.parser
  
  override def toString = {
    val body = if (!guard) EmptyExpr.toString else expr.toString
    // name + "(" + body + ")"
    body
  }
}

object Prod {
  def apply(nameArg : String, sc : SchemaComponent, expr : => Expr ) = new Prod(nameArg, sc, true, expr)
  def apply(nameArg : String, sc : SchemaComponent, guard : Boolean ) = {
    Assert.usage(guard == false)
    new Prod(nameArg, sc, guard, EmptyExpr)
  }
  def apply(nameArg : String, sc : SchemaComponent, guard : Boolean, expr : => Expr) = new Prod(nameArg, sc, guard, expr)
}
