package daffodil.exceptions

trait ThrowsSDE {

  def SDE(str: String, args: Any*): Nothing

  def schemaDefinitionError(str: String, args: Any*): Nothing = SDE(str, args: _*) // long form synonym

  def schemaDefinition(testThatWillThrowIfFalse: => Boolean, str: String, args: Any*) {
    if (!testThatWillThrowIfFalse)
      SDE(str, args: _*)
  }

}

