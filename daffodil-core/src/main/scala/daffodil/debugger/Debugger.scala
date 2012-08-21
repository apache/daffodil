package daffodil.debugger
import daffodil.processors.PState
import daffodil.dsom._
import daffodil.schema.annotation.props.gen.Representation

/**
 * Really simplistic debugger.
 */
object Debugger {

  var areDebugging = false
  var pause = true
  
  def setDebugging(flag : Boolean) {
    areDebugging = flag
  }
  
  def before(pstate : PState, context : SchemaComponent) {
    if (!areDebugging) return

    
    printState("Before", pstate, context)
   
    context match {
      case e : ElementBase => {
    	  if (e.representation == Representation.Text) {
            
    	  } else {
    	    // print data in hex form
    	  }
      }
        
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
  
  def printState(ba : String, pstate : PState, context : SchemaComponent) {
    println("%s %s".format(ba, context))
    println("%s position (bytes) = %d".format(ba, pstate.bytePos))
    if (pstate.bitLimit != -1) println("%s limit (bytes) = %d".format(ba, pstate.bitLimit / 8))
    println("%s array index = %d".format(ba, pstate.arrayPos))
    println("%s group index = %d".format(ba, pstate.groupPos))
    println("%s child index = %d".format(ba, pstate.childPos))
  }
  
  def after (beforePState : PState, afterPState : PState, context : SchemaComponent) {
    if (!areDebugging) return
    printState("After", afterPState, context)
  }
  
}