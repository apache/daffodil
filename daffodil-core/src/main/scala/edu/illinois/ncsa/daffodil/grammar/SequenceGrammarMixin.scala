package edu.illinois.ncsa.daffodil.grammar
import edu.illinois.ncsa.daffodil.grammar._
import edu.illinois.ncsa.daffodil.compiler._
import edu.illinois.ncsa.daffodil.processors._
import edu.illinois.ncsa.daffodil.schema.annotation.props._
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen._
import edu.illinois.ncsa.daffodil.dsom.oolag.OOLAG._
import edu.illinois.ncsa.daffodil.util._
import edu.illinois.ncsa.daffodil.dsom.Sequence

trait SequenceGrammarMixin { self: Sequence =>

  lazy val groupContent = {
    self.sequenceKind match {
      case SequenceKind.Ordered => orderedSequenceContent
      case SequenceKind.Unordered => subsetError("Unordered sequences are not supported.") // unorderedSequenceContent
    }
  }

  lazy val orderedSequenceContent = Prod("sequenceContent", this, SequenceCombinator(this, terms.foldRight(mt)(folder)))
  lazy val unorderedSequenceContent = {
    val uoseq = self.unorderedSeq.get
    Prod("unorderedSequenceContent", this, SequenceCombinator(this, UnorderedSequence(this, uoseq.terms.foldRight(mt)(folder))))
  }

  def folder(p: Gram, q: Gram): Gram = p ~ q

  lazy val terms = groupMembers.map { _.asTermInSequence }

  /**
   * These are static properties even though the delimiters can have runtime-computed values.
   * The existence of an expression to compute a delimiter is assumed to imply a non-zero-length, aka a real delimiter.
   */
  lazy val hasPrefixSep = sepExpr(SeparatorPosition.Prefix)

  lazy val hasInfixSep = sepExpr(SeparatorPosition.Infix)

  lazy val hasPostfixSep = sepExpr(SeparatorPosition.Postfix)

  // note use of pass by value. We don't want to even need the SeparatorPosition property unless there is a separator.
  def sepExpr(pos: => SeparatorPosition): Boolean = {
    if (hasSeparator) if (separatorPosition eq pos) true else false
    else false
  }

  lazy val hasSeparator = separator.isKnownNonEmpty
}

