package edu.illinois.ncsa.daffodil.processors.dfa

import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._

class TextParser(knownEncFunc: String => Int) extends DelimitedParser {

  var delims: Seq[DFADelimiter] = Seq.empty

  def parse(input: DFDLCharReader, isDelimRequired: Boolean): Maybe[ParseResult] = {
    val successes: Queue[(DFADelimiter, Registers)] = Queue.empty
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
        val numBits: Int = knownEncFunc(r.delimString.toString)
        val nextReader: DFDLCharReader = input.drop(totalNumCharsRead).asInstanceOf[DFDLCharReader]

        One(new ParseResult(Nope, delim, lookingFor, totalNumCharsRead, numBits, nextReader))
      }
    }
    result
  }
}