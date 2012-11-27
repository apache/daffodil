package daffodil.exceptions

trait ThrowsSDE {

  def SDE(str: String, args: Any*): Nothing
  def SDW(str: String, args: Any*): Unit

  def schemaDefinitionError(str: String, args: Any*): Nothing = SDE(str, args: _*) // long form synonym

  def schemaDefinition(testThatWillThrowIfFalse: => Boolean, str: String, args: Any*) {
    if (!testThatWillThrowIfFalse)
      SDE(str, args: _*)
  }

  def schemaDefintionWarning(testThatWillWarnIfFalse: => Boolean, str: String, args: Any*) {
    if (!testThatWillWarnIfFalse) SDW(str, args: _*)
  }

}

