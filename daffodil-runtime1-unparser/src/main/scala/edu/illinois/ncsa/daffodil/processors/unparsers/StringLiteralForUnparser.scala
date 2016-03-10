package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.dsom.EntityReplacer
import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.processors.OutputNewLineEv

/**
 * Handles the situation where a string literal may contain %NL;, which must
 * be replaced by the value of dfdl:outputNewLine which may be an expression
 * that has to be evaluated at runtime.
 *
 * Does everything that can be done at compile time at compile time.
 */
object StringLiteralForUnparser {

  /**
   * Factory for these takes outputNewLine property by name so that it can
   * avoid demanding it in the cases where it actually doesn't need to force
   * the existence of this property in the schema.
   */
  def apply(context: TermRuntimeData, outputNewLineByName: => OutputNewLineEv, stringLiteralRaw: String): StringLiteralForUnparser = {
    lazy val outputNL = outputNewLineByName
    val endMarker = "__daffodil_stringLiteralForUnparser_endMarker__"
    Assert.invariant(!stringLiteralRaw.endsWith(endMarker))
    val rawWithEndMark = stringLiteralRaw + endMarker
    EntityReplacer { er =>
      val cookedWithEndMark = er.replaceForUnparse(rawWithEndMark)
      val chunksSeparatedByNL = cookedWithEndMark.split(er.markerForNL).toSeq
      val last = chunksSeparatedByNL.last
      val butLast = chunksSeparatedByNL.take(chunksSeparatedByNL.length - 1)
      val chunks =
        if (last == endMarker) {
          // this means the original string ended with a %NL;
          // because the endMarker is all by itself.
          butLast :+ "" // replace it by an empty string
        } else {
          // the original string did not end with a %NL;
          // so that means whatever the last token is, it has our endMarker
          // glued onto it, and we must remove that.
          Assert.invariant(last.endsWith(endMarker))
          butLast :+ last.replace(endMarker, "")
        }

      if (chunks.length == 1) {
        // there are no NL entities. There is only a single chunk.
        ConstantStringLiteralForUnparser(chunks.head)
      } else if (outputNL.isConstant) {
        // the outputNL isn't an expression. We have its value directly.
        val sl = chunks.mkString(outputNL.optConstant.get)
        ConstantStringLiteralForUnparser(sl)
      } else {
        // There are multiple chunks separated by %NL; and there
        // is a runtime expression for outputNL
        RuntimeStringLiteralForUnparser(outputNL, chunks)
      }
    }
  }
}

sealed trait StringLiteralForUnparser {
  def evaluate(ustate: UState): String
}

case class ConstantStringLiteralForUnparser(str: String) extends StringLiteralForUnparser {
  def evaluate(ustate: UState): String = str
}

case class RuntimeStringLiteralForUnparser(outputNL: OutputNewLineEv, chunks: Seq[String]) extends StringLiteralForUnparser {
  def evaluate(ustate: UState): String = {
    val nlAny = outputNL.evaluate(ustate)
    val nl = nlAny.asInstanceOf[String]
    val sl = chunks.mkString(nl)
    sl
  }
}
