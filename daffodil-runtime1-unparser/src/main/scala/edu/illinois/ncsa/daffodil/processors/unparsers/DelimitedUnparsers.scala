package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.processors.PrimUnparser
import edu.illinois.ncsa.daffodil.processors.TextJustificationType
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.PrimUnparser
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import java.nio.charset.MalformedInputException
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.util.LogLevel
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.dfa.CreateFieldDFA
import edu.illinois.ncsa.daffodil.processors.DFDLStringReader
import edu.illinois.ncsa.daffodil.processors.dfa.TextDelimitedUnparser
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.GenerateEscape
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeCharUnparserHelper
import edu.illinois.ncsa.daffodil.processors.EscapeSchemeBlockUnparserHelper

class StringDelimitedUnparser(erd: ElementRuntimeData,
  justificationPad: TextJustificationType.Type,
  override val pad: Maybe[Char],
  isDelimRequired: Boolean)
  extends PrimUnparser(erd) with PaddingRuntimeMixin {

  val fieldDFA = CreateFieldDFA()
  val textUnparser = new TextDelimitedUnparser(erd)

  protected def theString(state: UState) = state.currentInfosetNode.get.asSimple.dataValueAsString

  override val padToLength: Int = {
    // TODO: Should this be BigDecimal or Int?
    if (erd.minLength.isDefined) { erd.minLength.get.intValue() } else { 0 }
  }

  def padOrTruncateByJustification(str: String): String = {
    val result = justificationPad match {
      case TextJustificationType.None => str
      case TextJustificationType.Right => addLeftPadding(str)
      case TextJustificationType.Left => addRightPadding(str)
      case TextJustificationType.Center => addPadding(str)
    }
    result
  }

  def unparse(state: UState): Unit = {

    log(LogLevel.Debug, "Parsing starting at bit position: %s", state.bitPos1b)

    val schemeOpt = state.currentEscapeScheme

    try {
      val valueString = theString(state)

      val escapedValue = {
        if (schemeOpt.isDefined) {

          val localDelimNode = state.localDelimiters
          val inscopeDelimiters = {
            val sep = if (localDelimNode.separator.isDefined) Seq(localDelimNode.separator.get)
            else Seq.empty
            val term = if (localDelimNode.terminator.isDefined) Seq(localDelimNode.terminator.get)
            else Seq.empty
            sep ++ term
          }
          val rdr = new DFDLStringReader(valueString)
          val scheme = schemeOpt.get

          val (result, _) = {
            if (scheme.isInstanceOf[EscapeSchemeCharUnparserHelper]) {
              val theScheme = scheme.asInstanceOf[EscapeSchemeCharUnparserHelper]
              val thingsToEscape = inscopeDelimiters ++ scheme.lookingFor

              textUnparser.escape(rdr, fieldDFA, thingsToEscape, theScheme.ec.get, theScheme.eec, state)
            } else {
              val theScheme = scheme.asInstanceOf[EscapeSchemeBlockUnparserHelper]

              def hasInscopeTerminatingDelimiters(): Boolean = {
                // Need to do this so we can 'break' the loop early
                //
                for (d <- inscopeDelimiters) {
                  if (valueString.contains(d.lookingFor)) return true
                }
                false
              }

              val generateEscapeBlock = (theScheme.generateEscapeBlock == GenerateEscape.Always) ||
                valueString.startsWith(theScheme.blockStart) || hasInscopeTerminatingDelimiters()

              val thingsToEscape = theScheme.lookingFor // blockEnd and extraEscapedCharacters

              textUnparser.escape(rdr, fieldDFA, thingsToEscape,
                theScheme.eec.get, theScheme.blockStart, theScheme.blockEnd,
                generateEscapeBlock, state)
            }
          }

          result
        } else valueString // No EscapeScheme
      }

      val paddedValue = padOrTruncateByJustification(escapedValue)

      val outStream = state.outStream
      outStream.encode(erd.encodingInfo.knownEncodingCharset.charset, paddedValue)
      log(LogLevel.Debug, "Ended at bit position " + outStream.bitPos1b)
    } catch {
      // Characters in infoset element cannot be encoded without error.
      //
      // This won't actually be thrown until encodingErrorPolicy='error' is
      // implemented. 
      //
      case m: MalformedInputException => { UnparseError(One(erd.schemaFileLocation), One(state), "%s - MalformedInputException: \n%s", nom, m.getMessage()) }
      //
      // Thrown if the length is explicit but are too many bytes/bits to
      // fit within the length.
      //
      case e: IndexOutOfBoundsException => {
        UnparseError(One(erd.schemaFileLocation), One(state), "%s - Too many bits in field: IndexOutOfBounds: \n%s", nom, e.getMessage())
      }
    }
  }

}

class LiteralNilDelimitedEndOfDataUnparser(
  erd: ElementRuntimeData,
  nilValue: String,
  justPad: TextJustificationType.Type,
  padChar: Maybe[Char],
  isDelimRequired: Boolean)
  extends StringDelimitedUnparser(erd, justPad, padChar, isDelimRequired) {

  final override def theString(ignored: UState) = nilValue
}

