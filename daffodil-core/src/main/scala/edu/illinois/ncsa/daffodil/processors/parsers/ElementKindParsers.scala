package edu.illinois.ncsa.daffodil.processors.parsers

import edu.illinois.ncsa.daffodil.grammar.Gram
import edu.illinois.ncsa.daffodil.processors.PrimParser
import edu.illinois.ncsa.daffodil.dsom.ElementBase
import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.dsom.ComplexTypeBase
import edu.illinois.ncsa.daffodil.dsom.SchemaComponent
import edu.illinois.ncsa.daffodil.dsom.Sequence
import edu.illinois.ncsa.daffodil.dsom.LocalElementDecl
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.processors.Success

class ComplexTypeParser(ct: ComplexTypeBase, body: Gram)
  extends PrimParser(body: Gram, ct.element) {
  override def toString = "ComplexType"

  val bodyParser = body.parser

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<ComplexType>" +
        bodyParser.toBriefXML(depthLimit - 1) +
        "</ComplexType>"
  }

  def parse(start: PState): PState = {
    start.mpstate.childIndexStack.push(1L) // one-based indexing
    val parseState = bodyParser.parse1(start, ct)
    start.mpstate.childIndexStack.pop()
    parseState
  }
}

class SequenceCombinatorParser(body: Gram, sq: Sequence)
  extends PrimParser(body, sq) {
  override def toString = "Sequence"

  val bodyParser = body.parser

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<Sequence>" +
        bodyParser.toBriefXML(depthLimit - 1) +
        "</Sequence>"
  }

  def parse(start: PState): PState = {
    start.mpstate.groupIndexStack.push(1L) // one-based indexing
    val parseState = bodyParser.parse1(start, context)
    start.mpstate.groupIndexStack.pop()
    start.mpstate.moveOverOneGroupIndexOnly()
    parseState
  }
}

class ArrayCombinatorParser(body: Gram, e: ElementBase) extends PrimParser(body, e) {
  override def toString = "Array"

  val bodyParser = body.parser

  override def toBriefXML(depthLimit: Int = -1): String = {
    if (depthLimit == 0) "..." else
      "<Array>" +
        bodyParser.toBriefXML(depthLimit - 1) +
        "</Array>"
  }

  def parse(start: PState): PState = {

    start.mpstate.arrayIndexStack.push(1L) // one-based indexing
    start.mpstate.occursBoundsStack.push(DaffodilTunableParameters.maxOccursBounds)

    val parseState = bodyParser.parse1(start, e)
    if (parseState.status != Success) return parseState

    val shouldValidate = start.mpstate.dataProc.getValidationMode != ValidationMode.Off

    val actualOccurs = start.mpstate.arrayIndexStack.pop()
    start.mpstate.occursBoundsStack.pop()

    val finalState = {
      if (shouldValidate) {
        e match {
          case led: LocalElementDecl => {
            val expectedMinOccurs = led.minOccurs
            val expectedMaxOccurs = led.maxOccurs
            val isUnbounded = expectedMaxOccurs == -1
            val occurrence = actualOccurs - 1
            val postValidationState = {
              if (isUnbounded && occurrence < expectedMinOccurs)
                parseState.withValidationError("%s occurred '%s' times when it was expected to be a " +
                  "minimum of '%s' and a maximum of 'UNBOUNDED' times.", e,
                  occurrence, expectedMinOccurs)
              else if (!isUnbounded && (occurrence < expectedMinOccurs || occurrence > expectedMaxOccurs))
                parseState.withValidationError("%s occurred '%s' times when it was expected to be a " +
                  "minimum of '%s' and a maximum of '%s' times.", e,
                  occurrence, expectedMinOccurs, expectedMaxOccurs)
              else
                parseState
            }
            postValidationState
          }
          case _ => parseState
        }
      } else {
        parseState
      }
    }
    finalState
  }
}
