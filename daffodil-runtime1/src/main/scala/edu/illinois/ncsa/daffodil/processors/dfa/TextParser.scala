package edu.illinois.ncsa.daffodil.processors.dfa

import scala.collection.mutable.ArrayBuffer
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.dsom.RuntimeEncodingMixin
import edu.illinois.ncsa.daffodil.processors.EncodingInfo
import edu.illinois.ncsa.daffodil.processors.RuntimeData

class TextParser(
  override val context: RuntimeData,
  override val encodingInfo: EncodingInfo)
  extends DelimitedParser with RuntimeEncodingMixin with Serializable {

  var delims: Seq[DFADelimiter] = Seq.empty

  def parse(input: DFDLCharReader, isDelimRequired: Boolean): Maybe[ParseResult] = {
    val successes: ArrayBuffer[(DFADelimiter, Registers)] = ArrayBuffer.empty
    val initialCharPos = input.characterPos

    delims.foreach(d => {
      val reg = new Registers()
      reg.reset(input, 0)

      val dfaStatus = d.run(0, reg)
      dfaStatus.status match {
        case StateKind.Failed => // Continue
        case StateKind.Succeeded => successes += (d -> reg)
        case _ => // Continue
      }
    })

    val lm = longestMatch(successes)
    val result = {
      if (!lm.isDefined) {
        if (isDelimRequired) Nope
        else {
          val totalNumCharsRead = 0
          val numBits: Int = 0
          val nextReader: DFDLCharReader = input.drop(totalNumCharsRead).asInstanceOf[DFDLCharReader]
          One(new ParseResult(Nope, Nope, "", totalNumCharsRead, numBits, nextReader))
        }
      } else {
        val (dfa, r) = lm.get
        val delim: Maybe[String] = {
          One(r.delimString.toString)
        }
        val lookingFor = dfa.lookingFor
        val totalNumCharsRead = r.numCharsRead
        val numBits: Int = knownEncodingStringBitLength(r.delimString.toString)
        val nextReader: DFDLCharReader = input.drop(totalNumCharsRead).asInstanceOf[DFDLCharReader]

        One(new ParseResult(Nope, delim, lookingFor, totalNumCharsRead, numBits, nextReader))
      }
    }
    result
  }
}
