package daffodil.dsom
import daffodil.api.Diagnostic
import daffodil.exceptions.Assert
import daffodil.api.WithDiagnostics

/*
 * This software Copyright (c) 2012 Tresys
 * Technology LLC, Columbia, Maryland, USA
 *
 * This software was developed by Tresys Technology LLC
 * with U.S. Government sponsorship.
 *
 * Tresys Technology LLC grants the U.S. Government and others
 * acting on its behalf, a paid-up non-exclusive, irrevocable
 * worldwide license in this computer software to reproduce,
 * prepare derivative works, and perform publicly and display
 * publicly, by or on behalf of the U.S. Government.
 *
 * All other rights are reserved by Tresys Technology LLC.
 *
 * The copyright notice above and this notice of U.S. Government
 * rights must be included with any such authorized reproductions,
 * derivative works, public displays, and public performances
 * of the computer software.
 */

/**
 * An extended result is a pairing of a result of type T, and a collection of diagnostics
 * that extend it and inform a caller about the result. It's possible for the result to have no
 * value of type T, but only diagnostics that provide reasons why.
 *
 * Three of four combinations of result value and diagnostic collection are possible. That is,
 * one can have a result with no diagnostics, diagnostics with no result, or both.
 * The fourth case, of no result value, and no diagnostic information represents a "generic" 
 * failure to produce a value. This is useful for a performance mode where diagnostic
 * information is not needed. 
 *
 */

// We may want to build up special operations so that one can operate on these in ways that
// do the right thing with both the value and the diagnostics. (Analogous to the way Scala's
// Option class allows an option value to be treated as a collection.)
//
// I suggest we not add these until we understand the idioms we need.

class ExtResult[+T] private (resultArg : Option[T], extArg : Seq[Diagnostic])
  extends WithDiagnostics { self =>

  Assert.usageErrorUnless(resultArg != null)
  Assert.usageErrorUnless(extArg != null)
  
  Assert.usageErrorUnless(resultArg == None && extArg == Nil) // for now.
  // Note: there is a situation where we may want to allow no value and no diagnostics, which 
  // is if, for performance reasons, we decide to have a no-diagnostics mode which is faster
  // but provides little or no diagnostic information on failures. In that case, these results
  // objects might suppress the creation of diagnostic objects so the case where one has 
  // no value but a list of diagnostics would allow that list to be Nil, or perhaps a 
  // statically constant list containing a constant generic error diagnostic object.

  // these are how you really construct one
  def this(resultArg : T, extArg : Seq[Diagnostic]) = this(Some(resultArg), extArg)
  def this(resultArg : T) = this(Some(resultArg), Nil)
  def this(extArg : Seq[Diagnostic]) = this(None, extArg)

  def result = resultArg
  def ext = extArg

  def getDiagnostics() : Seq[Diagnostic] = ext

  def canProceed() : Boolean = {
    val hasError = ext.exists(_.isError())
    val res = result.isDefined && !hasError 
    res
  }
  
  def isError() = !canProceed()

  def hasDiagnostics() : Boolean = {
    val s = getDiagnostics()
    if (s.length > 0) true
    else false
  }

  // Delegate most implementation to the contained option object.

  def get = result.get
  def isEmpty = result.isEmpty
  def isDefined = result.isDefined

}



