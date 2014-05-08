package edu.illinois.ncsa.daffodil.processors.dfa

import edu.illinois.ncsa.daffodil.processors.DFDLCharReader

class TextPaddingParser(val padChar: Char, knownEncFunc: String => Int)
  extends Parser {

  val paddingDFA = CreatePaddingDFA(padChar)

  def parse(input: DFDLCharReader): Option[ParseResult] = {

    val paddingReg: Registers = new Registers

    paddingReg.reset(input, 0)

    val dfaStatus = paddingDFA.run(0, paddingReg, 0) // Will always succeed.

    val paddingValue = Some(paddingReg.resultString.toString)
    val totalNumCharsRead = paddingReg.numCharsReadUntilDelim
    val numBits: Int = knownEncFunc(paddingReg.charsReadUntilDelim.toString)
    val nextReader: DFDLCharReader = input.drop(totalNumCharsRead).asInstanceOf[DFDLCharReader]
    Some(new ParseResult(paddingValue, None, "", totalNumCharsRead, numBits, nextReader))
  }

}