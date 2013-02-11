package edu.illinois.ncsa.daffodil.exceptions

trait ThrowsSDE {

  def SDE(str: String, args: Any*): Nothing
  def SDW(str: String, args: Any*): Unit
  def SDEButContinue(str: String, args: Any*): Unit

  def schemaDefinitionErrorButContinue(str: String, args: Any*): Unit = SDEButContinue(str, args: _*)

  def schemaDefinitionError(str: String, args: Any*): Nothing = SDE(str, args: _*) // long form synonym

  def schemaDefinition(testThatWillThrowIfFalse: => Boolean, str: String, args: Any*) {
    if (!testThatWillThrowIfFalse)
      SDE(str, args: _*)
  }

  def schemaDefinitionWarning(testThatWillWarnIfFalse: => Boolean, str: String, args: Any*) {
    if (!testThatWillWarnIfFalse) SDW(str, args: _*)
  }

}

