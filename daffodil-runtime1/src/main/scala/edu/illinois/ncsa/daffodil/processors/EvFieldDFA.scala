package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.processors.dfa.DFAField
import edu.illinois.ncsa.daffodil.processors.dfa.CreateFieldDFA


class FieldDFAParseEv(val escapeSchemeEv: Maybe[EscapeSchemeParseEv], rd: RuntimeData)
  extends Evaluatable[DFAField](rd)
  with InfosetCachedEvaluatable[DFAField] {

  override lazy val runtimeDependencies = escapeSchemeEv.toList

  def compute(state: ParseOrUnparseState) = {

    val fieldDFA =
      if (escapeSchemeEv.isDefined) {
        val es = escapeSchemeEv.get.evaluate(state)
        es match {
          case s: EscapeSchemeBlockParserHelper => CreateFieldDFA()
          case s: EscapeSchemeCharParserHelper => CreateFieldDFA(s.ec, s.eec)
        }
      } else {
        CreateFieldDFA()
      }
    fieldDFA
  }
}
