package edu.illinois.ncsa.daffodil.processors.dfa

import scala.collection.mutable.Queue
import edu.illinois.ncsa.daffodil.processors.DFDLCharReader

class TextParser(knownEncFunc: String => Int) extends Parser with HasLongestMatch {
  
  var delims: Seq[DFADelimiter] = Seq.empty

  def parse(input: DFDLCharReader, isDelimRequired: Boolean): Option[ParseResult] = {
    val successes: Queue[(DFADelimiter, Registers)] = Queue.empty
    val initialCharPos = input.characterPos

    delims.foreach(d => {
      val reg = new Registers()
      reg.reset(input, 0)
      d.run(0, reg) match {
        case Right(stateNum) => // Shouldn't happen
        case Left(status) => {
          status.status match {
            case StateKind.Failed => // Continue
            case StateKind.Succeeded => successes += (d -> reg)
            case _ => // Continue
          }
        }
      }
    })

    val result = longestMatch(successes) match {
      case None if isDelimRequired => None
      case None => {
        val totalNumCharsRead = 0
        val numBits: Int = 0
        val nextReader: DFDLCharReader = input.drop(totalNumCharsRead).asInstanceOf[DFDLCharReader]
        Some(new ParseResult(None, None, "", totalNumCharsRead, numBits, nextReader))
      }
      case Some((dfa, r)) => {
        val delim: Option[String] = {
          Some(r.delimString.toString)
        }
        val lookingFor = dfa.lookingFor
        val totalNumCharsRead = r.numCharsRead
        val numBits: Int = knownEncFunc(r.delimString.toString)
        val nextReader: DFDLCharReader = input.drop(totalNumCharsRead).asInstanceOf[DFDLCharReader]

        Some(new ParseResult(None, delim, lookingFor, totalNumCharsRead, numBits, nextReader))
      }
    }
    result
  }
}