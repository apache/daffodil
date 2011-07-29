package daffodil.processors.test.unit

import java.nio.charset.Charset

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import daffodil.processors.input._
import daffodil.schema.annotation.Annotation
import daffodil.schema.annotation.InputValue
import daffodil.schema.annotation.enumerations._

class ProcessorFactoryTest extends FunSuite with ShouldMatchers  {

	test("creating a TextProcessor") {
		val processor = new TextProcessor(Charset.forName("ASCII"),true)
		processor.setLength("12")
		processor.setTerminator("term")
		processor.setInitiator("init")
		processor.setSeparator("sep")

  
		val annotation = new Annotation(null)
		annotation.format setRepresentation(Text)
		annotation.format setEncoding(Charset.forName("ASCII"))
		annotation.format setType("string")
		annotation.format setLength ("12")
		annotation.format setTerminator("term")
		annotation.format setInitiator("init")
		annotation.format setSeparator("sep")

		val processor1 = ProcessorFactory getInputProcessor(annotation,false)

		processor1 should equal (processor)
	}
 
	test("creating a FloatTextProcessor") {
		val processor = new FloatTextProcessor(Charset.forName("ASCII"),true)
		processor.setLength("12")
		processor.setTerminator("term")
		processor.setInitiator("init")
		processor.setSeparator("sep")

		val annotation = new Annotation(null)
  
		annotation.format setRepresentation(Text)
		annotation.format setEncoding(Charset.forName("ASCII"))
		annotation.format setType("float")
		annotation.format setLength("12")
		annotation.format setTerminator("term")
		annotation.format setInitiator("init")
		annotation.format setSeparator("sep")		

		val processor1 = ProcessorFactory getInputProcessor(annotation,false)

		processor1 should equal (processor)
	}
 
	test("creating a DoubleTextProcessor") {

		val processor = new DoubleTextProcessor(Charset.forName("ASCII"),true)
		processor.setLength("12")
		processor.setTerminator("term")
		processor.setInitiator("init")
		processor.setSeparator("sep")

		val annotation1 = new Annotation(null)
		annotation1.format setRepresentation(Text)
		annotation1.format setEncoding("ASCII")
		annotation1.format setType("double")
		annotation1.format setLength("12")
		annotation1.format setTerminator("term")
		annotation1.format setInitiator("init")
		annotation1.format setSeparator("sep")

		val annotation2 = new Annotation(null)
		annotation2.format setRepresentation(Text)
		annotation2.format setEncoding("ASCII")
		annotation2.format setType("decimal")
		annotation2.format setLength("12")
		annotation2.format setTerminator("term")
		annotation2.format setInitiator("init")
		annotation2.format setSeparator("sep")

		val processor1 = ProcessorFactory getInputProcessor(annotation1,false)
		val processor2 = ProcessorFactory getInputProcessor(annotation2,false)

		processor1 should equal (processor)
		processor2 should equal (processor)
	}
 
	test("creating a DecimalTextProcessor") (pending)
// {
//		val processor = new DecimalTextProcessor(Charset.forName("ASCII"),true)
//		processor.setLength(12)
//		processor.setTerminator("term")
//		processor.setInitiator("init")
//		processor.setSeparator("sep")
//		processor.setBase(10)
//
//		val annotation1 = new Annotation(Map((AnnotationKeys.REPRESENTATION_KEY,null)->Text,
//                                       (AnnotationKeys.ENCODING_KEY,null)->Charset.forName("ASCII"),
//				(AnnotationKeys.TYPE_KEY,null)->"int",
//				(AnnotationKeys.LENGTH_KEY,null)->12,
//				(AnnotationKeys.TERMINATOR_KEY,null)->"term",
//				(AnnotationKeys.INITIATOR_KEY,null)->"init",
//				(AnnotationKeys.SEPARATOR_KEY,null)->"sep"))
//
//		val annotation2 = new Annotation(Map((AnnotationKeys.REPRESENTATION_KEY,null)->Text,
//                                       (AnnotationKeys.ENCODING_KEY,null)->Charset.forName("ASCII"),
//				(AnnotationKeys.TYPE_KEY,null)->"integer",
//				(AnnotationKeys.LENGTH_KEY,null)->12,
//				(AnnotationKeys.TERMINATOR_KEY,null)->"term",
//				(AnnotationKeys.INITIATOR_KEY,null)->"init",
//				(AnnotationKeys.SEPARATOR_KEY,null)->"sep"))
//  
//		val annotation3 = new Annotation(Map((AnnotationKeys.REPRESENTATION_KEY,null)->Text,
//                                       (AnnotationKeys.ENCODING_KEY,null)->Charset.forName("ASCII"),
//				(AnnotationKeys.TYPE_KEY,null)->"long",
//				(AnnotationKeys.LENGTH_KEY,null)->12,
//				(AnnotationKeys.TERMINATOR_KEY,null)->"term",
//				(AnnotationKeys.INITIATOR_KEY,null)->"init",
//				(AnnotationKeys.SEPARATOR_KEY,null)->"sep"))
//  
//		val annotation4 = new Annotation(Map((AnnotationKeys.REPRESENTATION_KEY,null)->Text,
//                                       (AnnotationKeys.ENCODING_KEY,null)->Charset.forName("ASCII"),
//				(AnnotationKeys.TYPE_KEY,null)->"short",
//				(AnnotationKeys.LENGTH_KEY,null)->12,
//				(AnnotationKeys.TERMINATOR_KEY,null)->"term",
//				(AnnotationKeys.INITIATOR_KEY,null)->"init",
//				(AnnotationKeys.SEPARATOR_KEY,null)->"sep"))
//  
//		val annotation5 = new Annotation(Map((AnnotationKeys.REPRESENTATION_KEY,null)->Text,
//                                       (AnnotationKeys.ENCODING_KEY,null)->Charset.forName("ASCII"),
//				(AnnotationKeys.TYPE_KEY,null)->"byte",
//				(AnnotationKeys.LENGTH_KEY,null)->12,
//				(AnnotationKeys.TERMINATOR_KEY,null)->"term",
//				(AnnotationKeys.INITIATOR_KEY,null)->"init",
//				(AnnotationKeys.SEPARATOR_KEY,null)->"sep"))
//  
//		val annotation6 = new Annotation(Map((AnnotationKeys.REPRESENTATION_KEY,null)->Text,
//                                       (AnnotationKeys.ENCODING_KEY,null)->Charset.forName("ASCII"),
//				(AnnotationKeys.TYPE_KEY,null)->"long",
//				(AnnotationKeys.LENGTH_KEY,null)->12,
//				(AnnotationKeys.TERMINATOR_KEY,null)->"term",
//				(AnnotationKeys.INITIATOR_KEY,null)->"init",
//				(AnnotationKeys.SEPARATOR_KEY,null)->"sep"))
//  
//		val annotation7 = new Annotation(Map((AnnotationKeys.REPRESENTATION_KEY,null)->Text,
//                                       (AnnotationKeys.ENCODING_KEY,null)->Charset.forName("ASCII"),
//				(AnnotationKeys.TYPE_KEY,null)->"unsignedLong",
//				(AnnotationKeys.LENGTH_KEY,null)->12,
//				(AnnotationKeys.TERMINATOR_KEY,null)->"term",
//				(AnnotationKeys.INITIATOR_KEY,null)->"init",
//				(AnnotationKeys.SEPARATOR_KEY,null)->"sep"))
//  
//		val annotation8 = new Annotation(Map((AnnotationKeys.REPRESENTATION_KEY,null)->Text,
//                                       (AnnotationKeys.ENCODING_KEY,null)->Charset.forName("ASCII"),
//				(AnnotationKeys.TYPE_KEY,null)->"unsignedInt",
//				(AnnotationKeys.LENGTH_KEY,null)->12,
//				(AnnotationKeys.TERMINATOR_KEY,null)->"term",
//				(AnnotationKeys.INITIATOR_KEY,null)->"init",
//				(AnnotationKeys.SEPARATOR_KEY,null)->"sep"))
//  
//		val annotation9 = new Annotation(Map((AnnotationKeys.REPRESENTATION_KEY,null)->Text,
//                                       (AnnotationKeys.ENCODING_KEY,null)->Charset.forName("ASCII"),
//				(AnnotationKeys.TYPE_KEY,null)->"unsignedShort",
//				(AnnotationKeys.LENGTH_KEY,null)->12,
//				(AnnotationKeys.TERMINATOR_KEY,null)->"term",
//				(AnnotationKeys.INITIATOR_KEY,null)->"init",
//				(AnnotationKeys.SEPARATOR_KEY,null)->"sep"))
//  
//		val annotation10 = new Annotation(Map((AnnotationKeys.REPRESENTATION_KEY,null)->Text,
//                                        (AnnotationKeys.ENCODING_KEY,null)->Charset.forName("ASCII"),
//				(AnnotationKeys.TYPE_KEY,null)->"unsignedByte",
//				(AnnotationKeys.LENGTH_KEY,null)->12,
//				(AnnotationKeys.TERMINATOR_KEY,null)->"term",
//				(AnnotationKeys.INITIATOR_KEY,null)->"init",
//				(AnnotationKeys.SEPARATOR_KEY,null)->"sep"))
//  
//  
//		val processor1 = ProcessorFactory(annotation1)
//		val processor2 = ProcessorFactory(annotation2)
//		val processor3 = ProcessorFactory(annotation3)
//		val processor4 = ProcessorFactory(annotation4)
//		val processor5 = ProcessorFactory(annotation5)
//		val processor6 = ProcessorFactory(annotation6)
//		val processor7 = ProcessorFactory(annotation7)
//		val processor8 = ProcessorFactory(annotation8)
//		val processor9 = ProcessorFactory(annotation9)
//		val processor10 = ProcessorFactory(annotation10)
//
//		processor1 should equal (processor)
//		processor2 should equal (processor)
//		processor3 should equal (processor)
//		processor4 should equal (processor)
//		processor5 should equal (processor)
//		processor6 should equal (processor)
//		processor7 should equal (processor)
//		processor8 should equal (processor)
//		processor9 should equal (processor) 
//		processor10 should equal (processor)
//	}
 
	test("creating an InputExpressionProcessor") {	
	  val annotation1 = new Annotation(null)
	  annotation1.inputValue = new InputValue("{ 12 + 30 }")
	  val annotation2 = new Annotation(null)
	  annotation2.inputValue = new InputValue("{ 12 + 30 }")
   
	  val processor = new InputExpressionProcessor("{ 12 + 30 }")
	  val processor1 = ProcessorFactory getInputProcessor (annotation1,false)
	  val processor2 = ProcessorFactory getInputProcessor (annotation2,false)
   
	  processor1 should equal (processor)
	  processor2 should equal (processor)
	}
	
 
	test("creating a BooleanTextProcessor") (pending)
 
	test("creating a DateTextProcessor") (pending)
	
	test("creating a DateTimeTextProcessor") (pending)
 
	test("creating a HexBinaryTextProcessor") (pending)
 
	test("creating binary processors") (pending)
 
 
  
  
}

