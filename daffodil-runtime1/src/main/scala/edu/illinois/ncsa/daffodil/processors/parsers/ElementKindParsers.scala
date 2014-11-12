package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.processors.Success
import edu.illinois.ncsa.daffodil.processors.RuntimeData
import edu.illinois.ncsa.daffodil.processors.ElementRuntimeData
import edu.illinois.ncsa.daffodil.processors.Parser

class ComplexTypeParser(rd: RuntimeData, bodyParser: Parser)
  extends PrimParser(rd) {
  override def toString = "ComplexType"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<ComplexType>" +
        bodyParser.toBriefXML(depthLimit - 1) +
        "</ComplexType>"
  }

  def parse(start: PState): PState = {
    start.mpstate.childIndexStack.push(1L) // one-based indexing
    val parseState = bodyParser.parse1(start, rd)
    start.mpstate.childIndexStack.pop()
    parseState
  }
}

class SequenceCombinatorParser(rd: RuntimeData, bodyParser: Parser)
  extends PrimParser(rd) {
  override def toString = "Sequence"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<Sequence>" +
        bodyParser.toBriefXML(depthLimit - 1) +
        "</Sequence>"
  }

  def parse(start: PState): PState = {
    start.mpstate.groupIndexStack.push(1L) // one-based indexing
    val parseState = bodyParser.parse1(start, rd)
    start.mpstate.groupIndexStack.pop()
    start.mpstate.moveOverOneGroupIndexOnly()
    parseState
  }
}

class ArrayCombinatorParser(erd: ElementRuntimeData, bodyParser: Parser) extends PrimParser(erd) {
  override def toString = "Array"

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<Array>" +
        bodyParser.toBriefXML(depthLimit - 1) +
        "</Array>"
  }

  def parse(start: PState): PState = {

    start.mpstate.arrayIndexStack.push(1L) // one-based indexing
    start.mpstate.occursBoundsStack.push(DaffodilTunableParameters.maxOccursBounds)

    val parseState = bodyParser.parse1(start, erd)
    if (parseState.status != Success) return parseState

    val shouldValidate = start.mpstate.dataProc.getValidationMode != ValidationMode.Off

    val actualOccurs = start.mpstate.arrayIndexStack.pop()
    start.mpstate.occursBoundsStack.pop()

    val finalState = {
      (erd.minOccurs, erd.maxOccurs) match {
        case (Some(minOccurs), Some(maxOccurs)) if shouldValidate =>
          val isUnbounded = maxOccurs == -1
          val occurrence = actualOccurs - 1
          val postValidationState = {
            if (isUnbounded && occurrence < minOccurs)
              parseState.withValidationError("%s occurred '%s' times when it was expected to be a " +
                "minimum of '%s' and a maximum of 'UNBOUNDED' times.", erd.prettyName,
                occurrence, minOccurs)
            else if (!isUnbounded && (occurrence < minOccurs || occurrence > maxOccurs))
              parseState.withValidationError("%s occurred '%s' times when it was expected to be a " +
                "minimum of '%s' and a maximum of '%s' times.", erd.prettyName,
                occurrence, minOccurs, maxOccurs)
            else
              parseState
          }
          postValidationState
        case _ => parseState
      }
    }
    finalState
  }
}
