package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom.GlobalElementDecl
import edu.illinois.ncsa.daffodil.compiler.ProcessorFactory
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.api.ValidationMode
import edu.illinois.ncsa.daffodil.api.Diagnostic
import edu.illinois.ncsa.daffodil.dsom.EncodingMixin

class SchemaSetRuntimeData(
  val parser: Parser, // unparser too someday
  val diagnostics: Seq[Diagnostic],
  val elementRuntimeData: ElementRuntimeData,
  val encodingInfo: EncodingInfo,
  var variables: VariableMap,
  var validationMode: ValidationMode.Type)
  extends Serializable with ThrowsSDE with EncodingMixin {

  override def schemaFileLocation = elementRuntimeData.schemaFileLocation
  override def SDE(str: String, args: Any*) = elementRuntimeData.SDE(str, args)

}