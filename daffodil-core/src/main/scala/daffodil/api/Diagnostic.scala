package daffodil.api

/*
 * This file Copyright (c) 2012 Tresys
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
 * Base trait for all error, warning, info, and other sorts of objects
 * that capture diagnostic information. 
 * 
 * Allows for lazy message creation, internationalization, etc.
 */
trait Diagnostic {

  /**
   * Turns the diagnostic object into a string.
   * 
   * Should utilize locale information to properly internationalize. But if that is 
   * unavailable, will still construct an English-language string.
   */
  def getMessage : String
  
  override def toString() = getMessage 
  /**
   * Get data location information relevant to this diagnostic object.
   * 
   * For example, this might be a file name, and position within the file.
   */
  def getDataLocations : Seq[DataLocation]
  
   /**
   * Get schema location information relevant to this diagnostic object.
   * 
   * For example, this might be a file name of a schema, and position within the schema file.
   */
  def getSchemaLocations : Seq[SchemaLocation]
  
  /**
   * Determine if a diagnostic object represents an error or something less serious.
   */
  def isError : Boolean
  
}

/**
 * Relevant data location for a diagnostic message. E.g., file and line number.
 */
trait DataLocation {
  def toString : String
}

/**
 * Relevant schema location for a diagnostic message. E.g., file and line number.
 */
trait SchemaLocation {
  def toString : String
}

/**
 * Mix into classes that can carry diagnostic information as part of their structure.
 */
trait WithDiagnostics {
  
  /**
   * If multiple diagnostic messages can be created by an action, then this 
   * returns a sequence of multiple diagnostic objects. If the message is 
   * a fatal runtime issue, then this might be a singleton list, or it could be 
   * a bunch of warnings followed by a fatal runtime error.
   * 
   * The order of the sequence is important. When the diagnostics are about
   * a file of text, then diagnostics that are about lines earlier in the file
   * are earlier in the list. 
   */
  def getDiagnostics : Seq[Diagnostic]
  
  /**
   * This predicate indicates whether the object in question succeeded or failed 
   * at whatever some action was trying to do. That is to say, 
   * do the diagnostics contain a hard error, or do the diagnostics
   * only contain warnings and/or advisory content. If true then only warnings
   * and other non-fatal diagnostics have appeared, so subsequent actions can 
   * proceed.
   * 
   * The classic example of this is compilation. If only warnings were produced
   * then one can proceed to run the compiled entity.
   * 
   * This list is lazily constructed, so asking a compiler for its diagnostics forces
   * the completion of all compilation so that any diagnostics will have been
   * created. That is, this isn't for polling for diagnostics or anything like that.
   */
  final lazy val canProceed : Boolean = !isError
  def isError : Boolean 
  /**
   * Indicates whether there are any diagnostic objects available.
   */
  // def hasDiagnostics : Boolean
}
