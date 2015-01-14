package edu.illinois.ncsa.daffodil.externalvars

import javax.xml.transform.stream.StreamSource
import javax.xml.validation.Schema
import javax.xml.validation.SchemaFactory
import javax.xml.validation.{ Validator => JValidator }
import org.xml.sax.SAXException
import java.io.File
import edu.illinois.ncsa.daffodil.util.Misc

object ExternalVariablesValidator {

  final val extVarXsd = {
    val stream = this.getClass().getResourceAsStream("/xsd/dafext.xsd")
    stream
  }

  def validate(xmlFile: File): Either[java.lang.Throwable, _] = {
    try {
      val schemaLang = "http://www.w3.org/2001/XMLSchema"
      val factory = SchemaFactory.newInstance(schemaLang)
      val schema = factory.newSchema(new StreamSource(extVarXsd))
      val validator = schema.newValidator()
      validator.validate(new StreamSource(xmlFile))
    } catch {
      case ex: SAXException => Left(ex)
    }
    Right(true)
  }

}
