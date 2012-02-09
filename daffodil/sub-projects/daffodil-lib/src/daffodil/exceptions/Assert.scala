package daffodil.exceptions

// Copyright (C) 2012, Michael J. Beckerle. All Rights Reserved.

abstract class AnException(m : String) extends Exception(m) {
  def this() = this("") // no arg constructor also.
}
class UsageException(m : String) extends AnException(m)
class NotYetImplementedException extends AnException("Not yet implemented.")
class Abort(m : String) extends AnException(m)

abstract class DFDLException(m : String) extends AnException(m) {
  def this() = this("") // no arg constructor also.
}
class SDE(m : String) extends DFDLException(m) // schema definition error

class PE(m : String) extends DFDLException(m) // parsing error (processing error)

class VE(m : String) extends DFDLException(m) // validation error (used only when validation is turned on.

class Assert  {
  def toss(x : Throwable) = {
    throw x
  }
}

object Assert extends Assert {
  
  
  def usage(test: => Boolean, message: => String = "Usage error.") = {
    val r = test
    if (!r)
      abort(message)
  }
  
  def usageError( message: => String = "Usage error.") = {
    abort(message)
  }
  
  def notYetImplemented() = {
    toss(new NotYetImplementedException)
  }
  
  def abort(message : => String = "") = {
    toss(new Abort(message))
  }
  
  def impossible(message: String = "impossible! this code path is supposed to be unreachable.") = {
    abort(message)
  }
  
  /**
   * use when a match/case has exhausted all possibles.
   * 
   * Sometimes, if you are just dispatching on an enum, scala can prove you've exhausted all
   * possibles. In other cases, use this. Eg., exhaustive case analysis on unsealed case classes (so
   * the compiler can't assure you, but you still believe you are being exhaustive). Under program
   * maintenance people break these things. Hence, use this to catch those kinds of fall throughs.
   */
  def impossibleCase() = impossible("should be no fall through to this case")
  
  /**
   * test for something that the program is supposed to be insuring. 
   * 
   * This is for more complex invariants than the simple 'impossible' case. 
   */
  def invariant(test : => Boolean) = {
    val r = test
    if (!r) abort("Invariant broken.")
  }
  
  /**
   * Use when a case or if/then analysis has fallen through to a situation that 
   * a program invariant should be assuring doesn't happen. That is, where 
   * the case analysis has exhaused all the situations that are consistent with
   * the invariant.
   * 
   * This is different from an impossible - those are for situations which 
   * are simpler to show are impossible. 
   */
  def invariantFailed(msg : => String = "") = {
    abort("Invariant broken. " + msg )
  }

    
  /**
   * Use for checks for Schema Definition Errors (per DFDL spec)
   */
  def schemaDefinition(test : => Boolean, message : => String) = {
    // note use above of by-name arguments. This lets us turn off the evaluation of the tests,
    // and also the construction of the string only happens IF the test passes.
    if (!test) toss(new SDE(message))
  }

  def schemaDefinitionError(message : => String) = {
    // note use above of by-name arguments. This lets us turn off the evaluation of the tests,
    // and also the construction of the string only happens IF the test passes.
    toss(new SDE(message))
  }
  
  def SDE(message : => String) = schemaDefinitionError(message)
    
  /**
   * Use for checks about currently implemented subset of DFDL.
   */
  def subset(test : => Boolean, message : => String) = schemaDefinition(test, "SUBSET: " + message)
    
  def unknownPropertyValue(propName : String, propValue : String) =
    schemaDefinitionError("For property " + propName + " unrecognized value " + propValue)
    
  /**
   * Conditional behavior for NYIs
   */
  def notYetImplemented(test : => Boolean) : Unit = {
    if (test) notYetImplemented()
  }
}