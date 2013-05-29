package edu.illinois.ncsa.daffodil.japi.debugger

import edu.illinois.ncsa.daffodil.debugger.{ InteractiveDebugger => SInteractiveDebugger }
import edu.illinois.ncsa.daffodil.debugger.{ InteractiveDebuggerRunner => SInteractiveDebuggerRunner }

abstract class DebuggerRunner {
  def init(): Unit
  def getCommand(): String
  def lineOutput(line: String): Unit
  def fini(): Unit
}

abstract class TraceRunner extends DebuggerRunner {
  val traceIter = Seq("display info parser",
                      "display info data",
                      "display info infoset",
                      "display info diff",
                      "trace").iterator

  def init(): Unit = {}

  def getCommand(): String = { 
    if (traceIter.hasNext) {
      traceIter.next
    } else {
      // If the traceItr commands are good this should never happen. The only
      // time this would ever get hit is if something caused the debugger to
      // break. So if this does happen, just keep running trace. We should
      // eventually finish parsing.
      "trace"
    }   
  }
 
  def fini(): Unit = {}
}

private[japi] class JavaInteractiveDebuggerRunner(dr: DebuggerRunner)
  extends SInteractiveDebuggerRunner {
  def init(id: SInteractiveDebugger): Unit = dr.init
  def getCommand(): String = dr.getCommand
  def lineOutput(line: String): Unit = dr.lineOutput(line)
  def fini(): Unit = dr.fini
}

