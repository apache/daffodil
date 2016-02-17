package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.TermRuntimeData
import edu.illinois.ncsa.daffodil.processors.parsers.DelimiterTextType
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.util.LogLevel
import java.nio.charset.MalformedInputException
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.dsom.CompiledExpression

class DelimiterTextUnparser(erd: TermRuntimeData, delimExpr: CompiledExpression[String],
  delimiterType: DelimiterTextType.Type)
  extends PrimUnparserObject(erd) with TextUnparserRuntimeMixin {

  override lazy val nom = {
    if (delimiterType == DelimiterTextType.Initiator) "InitiatorUnparser"
    else if (delimiterType == DelimiterTextType.Separator) "SeparatorUnparser"
    else "TerminatorUnparser"
  }

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..."
    else "<" + nom + ">" + delimExpr + "</" + nom + ">"
  }

  def unparse(state: UState): Unit = {
    setupEncoding(state, erd)

    log(LogLevel.Debug, "Unparsing starting at bit position: %s", state.bitPos1b)

    val localDelimNode = state.localDelimiters

    val (delimDFAOpt, _ /* location */ ) = {
      if (delimiterType == DelimiterTextType.Initiator) (localDelimNode.initiator, localDelimNode.initiatorLoc)
      else if (delimiterType == DelimiterTextType.Separator) (localDelimNode.separator, localDelimNode.separatorLoc)
      else (localDelimNode.terminator, localDelimNode.terminatorLoc)
    }

    if (!delimDFAOpt.isDefined) Assert.invariantFailed("Expected a delimiter of type " + delimiterType + " on the stack, but was not found.")

    val delimDFA = delimDFAOpt.get

    try {
      val valueString = delimDFA.unparseValue

      val outStream = state.dataOutputStream
      val nCharsWritten = outStream.putString(valueString)
      if (nCharsWritten != valueString.length)
        UE(state, "%s - Too many bits in delimiter: IndexOutOfBounds. Insufficient space to write delimiter '%s'.",
          nom, Misc.remapStringToVisibleGlyphs(valueString))
    } catch {
      // Characters in infoset element cannot be encoded without error.
      //
      // This won't actually be thrown until encodingErrorPolicy='error' is
      // implemented.
      //
      case m: MalformedInputException => { UnparseError(One(erd.schemaFileLocation), One(state.currentLocation), "%s - MalformedInputException: \n%s", nom, m.getMessage()) }
    }
  }

}
