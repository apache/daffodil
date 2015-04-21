package edu.illinois.ncsa.daffodil.grammar

import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.Parser
import edu.illinois.ncsa.daffodil.processors.unparsers.DummyUnparser
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser
import edu.illinois.ncsa.daffodil.processors.Nada
import edu.illinois.ncsa.daffodil.dsom.SchemaComponent
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG.OOLAGHost
import edu.illinois.ncsa.daffodil.compiler.ParserOrUnparser
import edu.illinois.ncsa.daffodil.util.Misc

/**
 * Gram - short for "Grammar Term"
 *
 * These are the objects in the grammar. The grammar is supposed to be
 * roughly the grammar in the DFDL specification, but some differences are expected
 * because this one has to actually be operationalized.
 */
abstract class Gram(contextArg: SchemaComponent)
  extends OOLAGHost(contextArg) {

  final val context: SchemaComponent = contextArg

  /**
   * Some grammar terms (productions specifically) have an indirection
   * to the grammar term that is their definition.
   *
   * This would be protected, except it is used in unit testing.
   */
  private[grammar] def gram: Gram = this

  private[grammar] def deref = this // override in Prod

  def name: String = prettyName

  override lazy val path = context.path + "%" + prettyName

  def isEmpty = false // they are by default not empty. Overridden in the cases where they could be.

  /**
   * Sequential composition operator in the grammar.
   *
   * Note: This should not evaluate the argument unless it has to.
   */
  def ~(qq: => Gram) = {
    lazy val q = qq.deref
    val self = this.deref
    //
    // The Nada terminal also behaves like empty for sequential composition
    // It is not empty for alternative composition though.
    //
    val res =
      if (self.isEmpty || self.isInstanceOf[Nada]) {
        if (q.isEmpty || q.isInstanceOf[Nada]) // Nada might get through to this point. Let's optimize it out.
          EmptyGram
        else q
      } else if (q.isEmpty || q.isInstanceOf[Nada]) self
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
    lazy val q = qq.deref
    val self = this.deref
    if (self.isEmpty)
      if (q.isEmpty) EmptyGram
      else q
    else if (q.isEmpty) self
    else
      AltComp(context, self, q)
  }

  /**
   * This || operator means one of the operands ONLY. I.e., statically
   * they are supposed to be mutually exclusive, so only one (or none) of them should
   * ever survive.
   */
  def ||(qq: => Gram) = {
    lazy val q = qq.deref
    val self = this.deref
    if (self.isEmpty)
      if (q.isEmpty) EmptyGram
      else q
    else if (q.isEmpty) self
    else
      Assert.invariantFailed("More than one alternative for || survived in the grammar")
  }

  /**
   * Parser - a Gram can provide a parser, which... parses what the Gram describes
   */
  def parser: Parser

  def unparser: Unparser = DummyUnparser(Misc.getNameFromClass(this)) // context.runtimeData

}
