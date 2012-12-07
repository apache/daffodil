package daffodil.processors
import daffodil.dsom.DFDLAssert
import daffodil.dsom.DFDLDiscriminator
import daffodil.dsom.DFDLSetVariable
import daffodil.dsom.ElementBase
import daffodil.grammar.Gram
import daffodil.grammar.NamedGram
import daffodil.schema.annotation.props.gen.TestKind
import daffodil.exceptions.Assert

abstract class SpecifiedLengthCombinatorBase(e: ElementBase, eGram: => Gram)
  extends NamedGram(e) {

  val eParser = eGram.parser
  val diagnosticChildren = List(eGram)

  def kind: String

  def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<SpecifiedLength" + kind + ">" +
        eParser.toBriefXML(depthLimit - 1) +
        "</SpecifiedLength" + kind + ">"
  }

}

class SpecifiedLengthPattern(e: ElementBase, eGram: => Gram)
  extends SpecifiedLengthCombinatorBase(e, eGram) {

  val kind = "Pattern"

  def parser: Parser = new SpecifiedLengthPatternParser(this, e)
  def unparser: Unparser = new DummyUnparser(e)

}

abstract class SpecifiedLengthParserBase(combinator: SpecifiedLengthCombinatorBase,
                                         e: ElementBase)
  extends PrimParser(combinator, e)
  with WithParseErrorThrowing {

  def toBriefXML = combinator.toBriefXML _

  def parse(pstate: PState, endBitPos: Long, e: ElementBase) = {
    val postState1 = pstate.withEndBitLimit(endBitPos)
    val postState2 = combinator.eParser.parse1(postState1, e)
    val postState3 = postState2.withEndBitLimit(pstate.bitLimit)
    postState3
  }

}

class SpecifiedLengthPatternParser(combinator: SpecifiedLengthCombinatorBase, e: ElementBase)
  extends SpecifiedLengthParserBase(combinator, e) {

  val charset = e.knownEncodingCharset
  val pattern = e.lengthPattern

  def parse(start: PState): PState = withParseErrorThrowing(start) {
    val in = start.inStream

    val reader = in.getCharReader(charset, start.bitPos)
    val d = new delimsearch.DelimParser(e)
    val result = d.parseInputPatterned(pattern, reader)
    val endBitPos =
      if (!result.isSuccess) start.bitPos + 0 // no match == length is zero!
      else start.bitPos + result.numBits
    val postEState = parse(start, endBitPos, e)
    postEState
  }
}

