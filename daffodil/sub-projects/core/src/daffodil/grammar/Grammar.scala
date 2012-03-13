package daffodil.grammar

import daffodil.exceptions.Assert
import daffodil.dsom.SchemaComponent

abstract class Expr {
  
  def getNameFromClass() = {
    val hexHash = this.hashCode.formatted("%x")
    getClass().getName().split("\\.").toList.last + "@" + hexHash
  }
  
  val name = getNameFromClass()
  
  def isEmpty = false // they are by default not empty expressions. Overridden in the cases where they could be.
  
  def ~(q : => Expr) = {
    if (q.isEmpty) this
    else if (this.isEmpty) q
    else
    	new SeqComp(this, q)
  }
  
  def |(q : => Expr) = {
    if (q.isEmpty) this
    else if (this.isEmpty) q
    else
    	new AltComp(this, q)
  }
}

abstract class UnaryExpr(r : Expr) extends Expr {
  override def toString = name + "(" + r + ")"
}

abstract class BinaryExpr(p : Expr, q : Expr) extends Expr {
  def op : String
  override def toString = "(" + p + " " + op + " " + q + ")"
}

class SeqComp(p : Expr, q : Expr) extends BinaryExpr(p, q) {
  def op = "~"
}

class AltComp(p : Expr, q : Expr) extends BinaryExpr(p, q) {
  def op = "|"
}

class RepExactlyN(r : Expr) extends UnaryExpr(r)
object RepExactlyN {
  def apply(r : Expr) = new RepExactlyN(r)
}

object EmptyExpr extends Expr {
  override def isEmpty = true
  override def toString = "Empty"
}



abstract class NamedExpr extends Expr {
    override def toString = name + (if (isEmpty) "(Empty)" else "")
}

/**
 * Primitives will derive from this base
 */
abstract class Terminal(sc: SchemaComponent, guard : Boolean) extends NamedExpr {
  override def isEmpty = !guard
}

/**
 * Productions will derive from this base.
 * 
 * Guards are used so we can express grammars that include all possibilities, 
 * but where examining the format properties specifically would indicate that some of those
 * possiblities are precluded. The guard causes that term to just splice itself out
 * of the grammar.
 */
class Production (sc : SchemaComponent, guard : Boolean, exprArg : => Expr) extends NamedExpr {
  val expr = if (guard) exprArg else EmptyExpr
  /**
   * Constructor overloads let you specify just guard (for stubbing things really), 
   * or just grammar production (which means no guard) or both.
   */
  def this(sc : SchemaComponent, expr : Expr ) = this(sc, true, expr)
  def this(sc : SchemaComponent, guard : Boolean ) = this(sc, { assert(guard == false); false}, EmptyExpr)
  override def isEmpty = expr.isEmpty
}
