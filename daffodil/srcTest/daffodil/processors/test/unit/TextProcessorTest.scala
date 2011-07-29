package daffodil.processors.test.unit

import java.nio.charset.Charset

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import daffodil.processors.input._


class TextProcessorTest extends FunSuite with ShouldMatchers  {
  
  test("equal things should be equal") {
    val processor1 = new TextProcessor(Charset.forName("ASCII"),true)
		processor1.setLength("12")
		processor1.setTerminator("term")
		processor1.setInitiator("init")
		processor1.setSeparator("sep")    
  
	val processor2 = new TextProcessor(Charset.forName("ASCII"),true)
		processor2.setLength("12")
		processor2.setTerminator("term")
		processor2.setInitiator("init")
		processor2.setSeparator("sep")
  
	processor1 should equal (processor2)
    processor2 should equal (processor1)
  }
  
  test("different things should be different") {
    val processor1 = new TextProcessor(Charset.forName("UTF-8"),true)
		processor1.setLength("12")
		processor1.setTerminator("term")
		processor1.setInitiator("init")
		processor1.setSeparator("sep")    
  
	val processor2 = new TextProcessor(Charset.forName("UTF-8"),true)
		processor2.setLength("15")
		processor2.setTerminator("term")
		processor2.setInitiator("init")
		processor2.setSeparator("sep")
  
	val processor3 = new TextProcessor(Charset.forName("UTF-8"),true)
		processor3.setLength("12")
		processor3.setTerminator("termo")
		processor3.setInitiator("init")
		processor3.setSeparator("sep")
  
	val processor4 = new TextProcessor(Charset.forName("UTF-8"),true)
		processor4.setLength("12")
		processor4.setTerminator("term")
		processor4.setInitiator("inito")
		processor4.setSeparator("sep")
  
	val processor5 = new TextProcessor(Charset.forName("UTF-8"),true)
		processor5.setLength("12")
		processor5.setTerminator("term")
		processor5.setInitiator("init")
		processor5.setSeparator("sepo")
  
	val processor6 = new TextProcessor(Charset.forName("ASCII"),true)
		processor6.setLength("12")
		processor6.setTerminator("term")
		processor6.setInitiator("init")
		processor6.setSeparator("sep")
  
	
  
	processor1 should not equal (processor2)
    processor1 should not equal (processor3)
    processor1 should not equal (processor4)
    processor1 should not equal (processor5)
    processor1 should not equal (processor6)
  }
  
  test("all others") (pending)
  
}
