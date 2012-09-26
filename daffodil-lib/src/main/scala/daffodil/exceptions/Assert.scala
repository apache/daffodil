package daffodil.exceptions
import daffodil.dsom.OOLAG.OOLAGException

// Copyright (C) 2012, Michael J. Beckerle. All Rights Reserved.

abstract class UnsuppressableException(m : String) extends Exception(m) {
  def this() = this("") // no arg constructor also.
}
class UsageException(m : String) extends UnsuppressableException(m)
class NotYetImplementedException(m : String) extends UnsuppressableException("Not yet implemented.\n" + m)
class Abort(m : String) extends UnsuppressableException(m)

class Assert  {
  def shortBacktrace = {
    val frames = Thread.currentThread().getStackTrace().toList.take(6).tail.tail
    frames.map{ _.toString }.mkString("\n")
  }
  
  def toss(x : Throwable) = {
    throw x
  }
}

object Assert extends Assert {
  
  
  def usage(testAbortsIfFalse: => Boolean, message: => String = "Usage error.") = {
    usageErrorUnless(testAbortsIfFalse, message)
  }
  
  /**
   * Verbose name helps you get the sense of the predicate right.
   */
  def usageErrorUnless(testAbortsIfFalse: => Boolean, message: => String = "Usage error.") = {
    val r = testAbortsIfFalse
    if (!r)
      abort(message)
  }
  
  def usageError( message: => String = "Usage error.") = {
    abort(message)
  }
 
  def notYetImplemented(info : String) = {
    toss(new NotYetImplementedException(info + "\n" + shortBacktrace))
  }
  
  def notYetImplemented() = {
    toss(new NotYetImplementedException(shortBacktrace))
  }
  
  def abort(message : => String = "") = {
    toss(new Abort(message + "\n" + shortBacktrace))
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
  def invariant(testThatWillAbortIfFalse : => Boolean) = {
    val r = testThatWillAbortIfFalse
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
   * Conditional behavior for NYIs
   */
  def notYetImplemented(testThatWillThrowIfTrue : => Boolean) : Unit = {
    if (testThatWillThrowIfTrue) notYetImplemented()
  }
}