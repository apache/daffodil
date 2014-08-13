package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom.GlobalElementDecl
import edu.illinois.ncsa.daffodil.compiler.ProcessorFactory
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.api.Diagnostic

class SchemaSetRuntimeData(
  val parser: Parser, // unparser too someday
  val diagnostics: Seq[Diagnostic],
  val elementRuntimeData: ElementRuntimeData,
  var variables: VariableMap,
  var validationMode: ValidationMode.Type)
  extends Serializable with ThrowsSDE {

  private val erd = elementRuntimeData // shorthand
  // 
  // FIXME - these can't work. All this information
  // must be computable from the ElementRuntimeData object for the 
  // root element. 
  // This is because many of the things below aren't knowable
  // e.g., knownEncodingIsFixedWidth faults if the encoding is an expression.
  //
  // This has to all be lazy vals 
  // 
  def isScannable = erd.isScannable

  def knownEncoding = encoding.isConstant
  lazy val defaultEncodingErrorPolicy = erd.defaultEncodingErrorPolicy
  lazy val knownEncodingIsFixedWidth = erd.knownEncodingIsFixedWidth
  lazy val knownEncodingAlignmentInBits = erd.knownEncodingAlignmentInBits
  lazy val charsetEncodingName = encoding.constantAsString
  def encoding = erd.encoding

  override def SDE(str: String, args: Any*) = erd.SDE(str, args)
  override def SDW(str: String, args: Any*) = erd.SDW(str, args)
  override def SDEButContinue(str: String, args: Any*) = erd.SDEButContinue(str, args)
}