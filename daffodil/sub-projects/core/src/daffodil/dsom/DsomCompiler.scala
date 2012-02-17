package daffodil.dsom

import scala.xml._
import scala.xml.parsing._
import daffodil.exceptions._
import daffodil.schema.annotation.props.gen._
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.collection.JavaConversions._

object DsomCompiler extends App {
  
//  def rip(schema: String) = {
//    val parser = new XSOMParser()
//    val apf = new DomAnnotationParserFactory()
//    parser.setAnnotationParser(apf)
//
//    val instream = new ByteArrayInputStream(schema.getBytes());
//
//    parser.parse(instream)
//
//    val sset = parser.getResult()
//    val sds = parser.getDocuments()
//    (sds, sset)
//  }

  /**
   * For unit tests
   */
  def compile(xml: Node) = {
    new SchemaSet(List(xml))
  }
}
