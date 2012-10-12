package daffodil.debugger
import daffodil.processors.PState
import daffodil.dsom._
import daffodil.schema.annotation.props.gen.Representation
import daffodil.processors.Success
import daffodil.processors.Parser
import daffodil.xml.XMLUtils

/**
 * Really simplistic debugger.
 */
object Debugger {

  /**
   * Wrap things to debug with this rather than just calling setDebugging(true).
   * That way it doesn't get turned on for every subsequent test after when
   * batches of tests are being run.
   */
  def withDebugger[T](body : => T) {
    try {
      setDebugging(true)
      body
    } finally {
      setDebugging(false)
    }
  }

  private var areDebugging = false
  var pause = true

  def setDebugging(flag : Boolean) {
    areDebugging = flag
  }

  def before(pstate : PState, parser : Parser) {
    if (!areDebugging) return

    printState("!!!Before!!!", pstate, parser)

    parser.context match {
      case e : ElementBase => {
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

  def printState(ba : String, pstate : PState, parser : Parser) {
    println("%s %s %s".format(ba, parser.context, parser))
    println("%s position (bytes) = %d".format(ba, pstate.bytePos))
    if (pstate.bitLimit != -1) println("%s limit (bytes) = %d".format(ba, pstate.bitLimit / 8))
    if (pstate.discriminator == true) println("%s discriminator true".format(ba))
    if (pstate.arrayPos != -1) println("%s array index = %d".format(ba, pstate.arrayPos))
    if (pstate.groupPos != -1) println("%s group index = %d".format(ba, pstate.groupPos))
    if (pstate.childPos != -1) println("%s child index = %d".format(ba, pstate.childPos))
    val etext = pstate.parent match {
      case e : org.jdom.Element => {
        val xmlVerbose = XMLUtils.element2Elem(e)
        val xml = XMLUtils.removeAttributes(xmlVerbose)
        println("%s Infoset node = '%s'".format(ba, xml))
      }
      case _ =>
    }
    val loc = pstate.currentLocation
    println(loc)
  }

  def printStateDelta(ba : String, before : PState, after : PState, parser : Parser) {
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
    val etext = after.parent match {
      case e : org.jdom.Element => {
        val xmlVerbose = XMLUtils.element2Elem(e)
        val xml = XMLUtils.removeAttributes(xmlVerbose)
        println("%s Infoset node = '%s'".format(ba, xml))
      }
      case _ =>
    }

  }

  def after(beforePState : PState, afterPState : PState, parser : Parser) {
    if (!areDebugging) return
    printStateDelta("!!!After!!!", beforePState, afterPState, parser)
  }

}