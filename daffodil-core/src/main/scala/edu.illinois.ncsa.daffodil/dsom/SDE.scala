package edu.illinois.ncsa.daffodil.dsom
import edu.illinois.ncsa.daffodil.exceptions.ThrowsSDE
import edu.illinois.ncsa.daffodil.exceptions.Assert

class SchemaDefinitionError(schemaContext: Option[SchemaComponent],
                            annotationContext: Option[DFDLAnnotation],
                            kind: String,
                            args: Any*)
  extends SchemaDefinitionDiagnosticBase(schemaContext, annotationContext, kind, args: _*) {

  def this(sc: SchemaComponent, kind: String, args: Any*) = this(Some(sc), None, kind, args: _*)
  val isError = true
  val diagnosticKind = "Error"
}

class SchemaDefinitionWarning(schemaContext: Option[SchemaComponent],
                              annotationContext: Option[DFDLAnnotation],
                              kind: String,
                              args: Any*)
  extends SchemaDefinitionDiagnosticBase(schemaContext, annotationContext, kind, args: _*) {

  def this(sc: SchemaComponent, kind: String, args: Any*) = this(Some(sc), None, kind, args: _*)

  val isError = false
  val diagnosticKind = "Warning"
}

abstract class SchemaDefinitionDiagnosticBase(
  val schemaContext: Option[SchemaComponent],
  val annotationContext: Option[DFDLAnnotation],
  val kind: String,
  val args: Any*) extends Exception with DiagnosticImplMixin {
  def isError: Boolean
  def diagnosticKind: String
  def getSchemaLocations = schemaContext.toList
  def getDataLocations = Nil
  // TODO: Alternate constructor that allows data locations.
  // Because some SDEs are caught only once Processing starts. 
  // They're still SDE but they will have data location information.

  override def toString = {
    //
    // It is important that this routine is robust. It is used to print error messages
    // so if something goes wrong in this, you run around in circles. I believe the 
    // stack-overaflow problems will be caught so long as one is running through lazy val aka 
    // OOLAG 'attributes' framework. 
    //
    val res = {
      //
      // Right here is where we would lookup the symbolic error kind id, and 
      // choose a locale-based message string.
      //
      // For now, we'll just do an automatic English message.
      //
      val msg =
        if (kind.contains("%"))
          kind.format(args: _*)
        else {
          val argsAsString = args.map { _.toString }.mkString(", ")
          (kind + "(%s)").format(argsAsString)
        }

      // this is where it gets kind of hairy. We're depending on fairly rich
      // attribute calculations in order to generate the context information 
      // in these diagnostic messages. Break any of that stuff, and suddenly 
      // you will get circularity errors from OOLAG.
      // beats a stack-overflow at least.
      val schContextLocDescription =
        schemaContext.map { " " + _.locationDescription }.getOrElse("")

      val annContextLocDescription =
        annotationContext.map { " " + _.locationDescription }.getOrElse("")

      val res = "Schema Definition " + diagnosticKind + ": " + msg +
        " Schema context: " + schemaContext.getOrElse("top level") + "." +
        // TODO: should be one or the other, never(?) both
        schContextLocDescription +
        annContextLocDescription

      res
    }
    res
  }

  override def getMessage = toString
}

trait ImplementsThrowsSDE
  extends ThrowsSDE
  with DiagnosticsProviding {
  /**
   * Centralize throwing for debug convenience
   */
  def toss(th: Throwable) = {
    throw th // good place for a breakpoint
  }

  def schemaComponent: SchemaComponent

  // TODO: create a trait to share various error stuff with DFDLAnnotation class.
  // Right now there is small code duplication since annotations aren't schema components.
  def SDE(id: String, args: Any*): Nothing = {
    val sde = new SchemaDefinitionError(Some(schemaComponent), None, id, args: _*)
    toss(sde)
  }

  def SDEButContinue(id: String, args: Any*): Unit = {
    val sde = new SchemaDefinitionError(Some(schemaComponent), None, id, args: _*)
    addDiagnostic(sde)
  }

  def SDW(id: String, args: Any*): Unit = {
    val sdw = new SchemaDefinitionWarning(Some(schemaComponent), None, id, args: _*)
    addDiagnostic(sdw)
  }

  /**
   * Use for cases where it is an SDE because of something we've chosen
   * not to implement. Not merely short term (haven't coded it yet, but intending to),
   * more like things we've chosen to defer intentionally to some future release.
   */
  def subset(testThatWillThrowIfFalse: Boolean, msg: String, args: Any*) = {
    if (!testThatWillThrowIfFalse) subsetError(msg, args: _*)
  }

  def subsetError(msg: String, args: Any*) = {
    val msgTxt = msg.format(args: _*)
    SDE("Subset " + msgTxt)
  }

}
