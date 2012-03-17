package daffodil.grammar

import org.jdom._
import daffodil.xml.Namespaces
import daffodil.processors._
import daffodil.exceptions.Assert

import daffodil.processors._
import java.io._

/**
 * Encapsulates lower-level parsing with a uniform interface
 */
trait Parser {
  
	def parse(pstate : PState) : PState
	
    // TODO: other methods for things like asking for the ending position of something
	// which would enable fixed-length formats to skip over data and not parse it at all.
	
}

class EmptyExprParser extends Parser {
  def parse(pstate : PState) = pstate
}

class SeqCompParser(p : Expr, q : Expr) extends Parser {
  Assert.invariant(!p.isEmpty && !q.isEmpty)
  val pParser = p.parser
  val qParser = q.parser
  def parse(pstate : PState) = {
    val pResult = pParser.parse(pstate)
    if (pResult.status == Success) {
      val qResult = qParser.parse(pResult)
      qResult
    } 
    else pResult
  }
}

class AltCompParser(p : Expr, q : Expr) extends Parser {
  Assert.invariant(!p.isEmpty && !q.isEmpty)
  val pParser = p.parser
  val qParser = q.parser
  def parse(pstate : PState) = {
    //
    // TBD: better Scala idiom for try/catch than this var stuff
    // Use of var makes this code non-thread-safe. If you can avoid var, then you
    // don't have to worry about multiple threads at all.
    //
    var pResult : PState = null
    try {
      pResult = pParser.parse(pstate)
    }
    catch {
      case e : Exception => 
    }
    if (pResult != null && pResult.status == Success) pResult
    else {
      //
      // TODO: check for discriminator evaluated to true.
      // If so, then we don't run the next alternative, we
      // consume this discriminator status result (so it doesn't ripple upward)
      // and return the failed state.
      //
      val qResult = qParser.parse(pResult)
      qResult
    } 
  }
}

class RepExactlyNParser extends Parser {
    def parse(pstate : PState) = Assert.notYetImplemented()
}

object DummyParser extends Parser { 
    def parse(pstate : PState) : PState = Assert.notYetImplemented()
}

/**
 * A parser takes a state, and returns an updated state
 * 
 * The fact that there are side-effects/mutations on parts of the state
 * enables us to reuse low-level java primitives that mutate streams.
 * 
 * The goal however, is to hide that fact so that the only places that have to 
 * know are the places doing the mutation, and the places rolling them back
 * which should be isolated to the alternative parser.
 */
class PState (
  val parent : Parent,
  val variableMap : VariableMap,
  val target : String,
  val namespaces : Namespaces,
  val status : ProcessorResult
  ) {
  /**
   * Convenience functions for creating a new state, changing only
   * one of the state components to a new one.
   */
  def withParent (newParent : Parent) = 
    new PState(newParent, variableMap, target, namespaces, status)
  def withVariables (newVariableMap : VariableMap) = 
    new PState(parent, newVariableMap, target, namespaces, status)
  
  def checkpoint() : CheckPoint = Assert.notYetImplemented()
  def rollback(checkpoint : CheckPoint) : PState = Assert.notYetImplemented()
  
}
object PState {
  def createInitialState(in : InputStream) : PState = {
    val newState = Assert.notYetImplemented()
    newState
  }
}

class CheckPoint {
  
}

