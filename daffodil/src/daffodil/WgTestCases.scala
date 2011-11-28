package daffodil

import daffodil.exceptions.DFDLSchemaDefinitionException
import daffodil.parser.SchemaParser
import org.jdom.Element
import daffodil.xml.XMLUtil
import org.jdom.input.SAXBuilder
import java.io.File
import java.io.FileInputStream

object WgTestCases {
	val TDML = "http://www.ibm.com/xmlns/dfdl/testData"
	val PARSER_TEST_CASE = TDML + "parserTestCase"
	val NAME = "name"
	val ROOT = "root"
	val MODEL = "model"
	val DESCRIPTION = "description"
	val DOCUMENT = TDML + "document"
	val DOCUMENT_PART = TDML + "documentPart"
	val TYPE = "type"
	val BYTE = "byte"
	val INFOSET = TDML + "infoset"
	val DFDL_INFOSET = TDML + "dfdlInfoset"
	
	
	def main(args : Array[String]) : Unit = {
		val directory = args(0); // directory containing tests
		val test = args(1); // test xml descriptor

		// interpret the descriptor
		val document = XMLUtil.parse(new File(directory,test))
		
		for(child <- XMLUtil.getChildren(document.getRootElement)) {
			XMLUtil.getFullNameWithNamespace(child) match {
				case PARSER_TEST_CASE => {
					processTestCase(child)
				}
				// ignore other cases
			}
		}
	}

	def attr(e:Element, a:String):String = {
		XMLUtil.getAttribute(e,a) match { case Some(v) => v ; case None => throw new Exception("attr getAttribute failed.") }
	}
	
	def processTestCase(tc:Element) = {
		val (name, description, root, model) = (attr(tc,NAME), attr(tc,DESCRIPTION), attr(tc,ROOT), attr(tc,MODEL))
		println("Processing test case \"" + description + "\"")
		println("Parsing DFDL schema "+model)
		try {
			val schema = new SchemaParser
			schema.parse(model)
		} catch {
			case e:DFDLSchemaDefinitionException => {
				e.printStackTrace()
			}
		}
	}
}