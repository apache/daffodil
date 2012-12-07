package daffodil.debugger
import daffodil.processors.PState
import daffodil.dsom._
import daffodil.schema.annotation.props.gen.Representation
import daffodil.processors.Success
import daffodil.processors.Parser
import daffodil.xml.XMLUtils
import daffodil.processors.EndSequence
import daffodil.processors.EndArray
import daffodil.processors.StartSequence
import daffodil.processors.StartArray
import daffodil.processors.StartChildren
import daffodil.processors.EndChildren

/**
 * Really simplistic debugger.
 */
object Debugger {

  /**
   * Wrap things to debug with this rather than just calling setDebugging(true).
   * That way it doesn't get turned on for every subsequent test after when
   * batches of tests are being run.
   */
  def withDebugger[T](body: => T) {
    try {
      setDebugging(true)
      body
    } finally {
      setDebugging(false)
    }
  }

  private var areDebugging = false
  var pause = true

  def setDebugging(flag: Boolean) {
    areDebugging = flag
  }

  def dontTrace(parser: Parser)(body: => Unit) = {
    parser.toString match {
      case "EndSequence" => body
      // case "EndArray" => body
      case "StartSequence" => body
      // case "StartArray" => body
      case "StartChildren" => body
      case "EndChildren" => body
      case _ => // nothing
    }
  }

  def before(pstate: PState, parser: Parser) {
    if (!areDebugging) return

    dontTrace(parser) { return }

    printState("!!!Before!!!", pstate, parser)

    parser.context match {
      case e: ElementBase => {
        if (e.representation == Representation.Text) {

        } else {
          // print data in hex form
        }
      }
      case _ =>

    }

    //    def readCommand() {
    //      do {
    //        println("Press space to continue.")
    //        Console.readChar match {
    //        case ' ' => true
    //        case _ => false
    //      }
    //     }

  }

  def printState(ba: String, pstate: PState, parser: Parser) {
    println("%s %s %s".format(ba, parser.context, parser))
    println("%s position (bytes) = %d".format(ba, pstate.bytePos))
    if (pstate.bitLimit != -1) println("%s limit (bytes) = %d".format(ba, pstate.bitLimit / 8))
    if (pstate.discriminator == true) println("%s discriminator true".format(ba))
    if (pstate.arrayPos != -1) println("%s array index = %d".format(ba, pstate.arrayPos))
    if (pstate.groupPos != -1) println("%s group index = %d".format(ba, pstate.groupPos))
    if (pstate.childPos != -1) println("%s child index = %d".format(ba, pstate.childPos))
    println("%s Infoset node = '%s'".format(ba, pstate.infoset.toBriefXML))
    val loc = pstate.currentLocation
    println(loc)
  }

  def printStateDelta(ba: String, before: PState, after: PState, parser: Parser) {
    println("%s %s %s".format(ba, parser.context, parser))
    if (after.status != Success) {
      println(after.status)
      return
    }
    var hasDelta = false

    if (before.bytePos != after.bytePos) {
      hasDelta = true
      println("%s position (bytes) = %d".format(ba, after.bytePos))
    }
    if (before.bitLimit != after.bitLimit) println("%s limit (bytes) = %d".format(ba, after.bitLimit / 8))
    if (before.discriminator != after.discriminator)
      println("%s discriminator changed to %s".format(ba, after.discriminator))
    else if (after.discriminator == true)
      println("%s discriminator %s".format(ba, after.discriminator))

    if (before.arrayPos != after.arrayPos) println("%s array index = %d".format(ba, after.arrayPos))
    if (before.groupPos != after.arrayPos) println("%s group index = %d".format(ba, after.groupPos))
    if (before.childPos != after.arrayPos) println("%s child index = %d".format(ba, after.childPos))
    println("%s Infoset node = '%s'".format(ba, after.infoset.toBriefXML))

  }

  def after(beforePState: PState, afterPState: PState, parser: Parser) {
    if (!areDebugging) return
    dontTrace(parser) { return }
    printStateDelta("!!!After!!!", beforePState, afterPState, parser)
  }

  def beforeRepetition(pstate: PState, parser: Parser) {
    if (!areDebugging) return
    println("!!! BeforeIteration !!!" + parser)
  }

  def afterRepetition(beforePState: PState, afterPState: PState, parser: Parser) {
    if (!areDebugging) return
    println("!!! AfterIteration !!! " + parser)
  }

}