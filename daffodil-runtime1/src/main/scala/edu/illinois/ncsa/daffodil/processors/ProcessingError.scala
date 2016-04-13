package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.api.DataLocation
import edu.illinois.ncsa.daffodil.api.LocationInSchemaFile
import edu.illinois.ncsa.daffodil.processors.unparsers.UnparseError
import edu.illinois.ncsa.daffodil.dsom.DiagnosticImplMixin
import edu.illinois.ncsa.daffodil.exceptions.ThinThrowable
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.exceptions.SchemaFileLocation

abstract class ProcessingError(
  pOrU: String,
  rd: Maybe[SchemaFileLocation],
  loc: Maybe[DataLocation],
  kind: String,
  args: Any*)
  extends Exception with ThinThrowable with DiagnosticImplMixin {

  /**
   * Used to convert a processing error into a parse error so that it
   * looks like the same as other parse errors to tests that search for the
   * "Parse Error" string.
   */
  def toParseError = new ParseError(rd, loc, kind, args: _*)
  
  /**
   * Used to convert a processing error into a parse error so that it
   * looks like the same as other parse errors to tests that search for the
   * "Parse Error" string.
   */
  def toUnparseError = new UnparseError(rd, loc, kind, args: _*)

  override def getLocationsInSchemaFiles: Seq[LocationInSchemaFile] = rd.toSeq

  override def getDataLocations: Seq[DataLocation] = loc.toSeq

  private lazy val schemaLocationsString = {
    val strings = getLocationsInSchemaFiles.map { _.locationDescription }
    val res = if (strings.length > 0)
      " " + strings.mkString(", ")
    else
      " (no schema file location)"
    res
  }

  def componentText: String = ""

  override def toString = {
    //
    // Right here is where we would lookup the symbolic error kind id, and
    // choose a locale-based message string.
    //
    // For now, we'll just do an automatic English message.
    //
    val msg = {
      if (args.size > 0) {
        try {
          kind.format(args: _*)
        } catch {
          case e: IllegalArgumentException =>
            throw new IllegalArgumentException("""format string "%s" did not accept these arguments: %s""".format(kind, args.mkString(", ")))
        }
      } else kind
    }
    val res = pOrU + ": " + msg +
      componentText +
      "\nSchema context: %s%s".format((if (rd.isDefined) rd.value.toString else "(no schema component identifier)"), schemaLocationsString) +
      (if (loc.isDefined)
        "\nData location was preceding %s".format(loc.value)
      else
        "(no data location)")
    res
  }

  override def getMessage = toString
}