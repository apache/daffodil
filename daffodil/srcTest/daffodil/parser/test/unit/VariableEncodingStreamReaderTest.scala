package daffodil.parser.test.unit

import java.io.ByteArrayInputStream
import java.nio.charset.Charset

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class VariableEncodingStreamReaderTest extends FunSuite with ShouldMatchers {

  test("reading ten chars in ASCII, one by one") {
    val input = new ByteArrayInputStream(Array('0' toByte,
					       '1' toByte,
					       '2' toByte,
					       '3' toByte,
					       '4' toByte,
					       '5' toByte,
					       '6' toByte,
					       '7' toByte,
					       '8' toByte,
					       '9' toByte))
    val rollbackStream = new RollbackStream(input)
    val reader = new VariableEncodingStreamReader(
      rollbackStream,Charset.forName("ASCII"))

    reader.read should equal ('0')
    reader.read should equal ('1')
    reader.read should equal ('2')
    reader.read should equal ('3')
    reader.read should equal ('4')
    reader.read should equal ('5')
    reader.read should equal ('6')
    reader.read should equal ('7')
    reader.read should equal ('8')
    reader.read should equal ('9')
    
    rollbackStream close
  }

  test("reading up to end of file"){
     val input = new ByteArrayInputStream(Array('0' toByte,
					       '1' toByte,
					       '2' toByte,
					       '3' toByte,
					       '4' toByte,
					       '5' toByte,
					       '6' toByte,
					       '7' toByte,
					       '8' toByte,
					       '9' toByte))
    val rollbackStream = new RollbackStream(input)
    val reader = new VariableEncodingStreamReader(
      rollbackStream,Charset.forName("ASCII"))

    reader.read should equal ('0')
    reader.read should equal ('1')
    reader.read should equal ('2')
    reader.read should equal ('3')
    reader.read should equal ('4')
    reader.read should equal ('5')
    reader.read should equal ('6')
    reader.read should equal ('7')
    reader.read should equal ('8')
    reader.read should equal ('9')
    reader.read should equal (-1)
    
    rollbackStream close
  }

  test("reading ten ASCII characters, all at once") (pending)

  test("reading unicode"){
    val telugu = 
      "\u0041\u0042\u0043\u0044\u0C20\u0C21\u0C22\u0C23\u0C24\u0C25\u0C26"
    val input = new ByteArrayInputStream(telugu getBytes("UTF-8"))
    val rollbackStream = new RollbackStream(input)
    val reader = new VariableEncodingStreamReader(
      rollbackStream,Charset.forName("UTF-8"))
    
    reader.read should equal ('\u0041')
    reader.read should equal ('\u0042')
    reader.read should equal ('\u0043')    
    reader.read should equal ('\u0044')
    reader.read should equal ('\u0C20')    
    reader.read should equal ('\u0C21')    
    reader.read should equal ('\u0C22')    
    reader.read should equal ('\u0C23')    
    reader.read should equal ('\u0C24')    
    reader.read should equal ('\u0C25')    
    reader.read should equal ('\u0C26')    
    reader.read should equal (-1)    
  }

  test("reading unicode and rolling back (I)"){
    val telugu = 
      "\u0041\u0042\u0043\u0044\u0C20\u0C21\u0C22\u0C23\u0C24\u0C25\u0C26"
    val input = new ByteArrayInputStream(telugu getBytes("UTF-8"))    
    val rollbackStream = new RollbackStream(input)
    val reader = new VariableEncodingStreamReader(
      rollbackStream,Charset.forName("UTF-8"))
    
    reader.read should equal ('\u0041')
    reader.read should equal ('\u0042')
    reader.read should equal ('\u0043')    
    reader.read should equal ('\u0044')

    rollbackStream checkpoint()
    
    reader.read should equal ('\u0C20')
    reader.read should equal ('\u0C21')
    reader.read should equal ('\u0C22')
    reader.read should equal ('\u0C23')
    reader.read should equal ('\u0C24')
    reader.read should equal ('\u0C25')
    reader.read should equal ('\u0C26')
    reader.read should equal (-1)
    
    rollbackStream rollback
    
    reader.read should equal ('\u0C20')
    reader.read should equal ('\u0C21')
    reader.read should equal ('\u0C22')
  }

  test("reading unicode and rolling back (II)"){
    val telugu = 
      "\u0041\u0042\u0043\u0044\u0C20\u0C21\u0C22\u0C23\u0C24\u0C25\u0C26"
    val input = new ByteArrayInputStream(telugu getBytes("UTF-8"))    
    val rollbackStream = new RollbackStream(input)
    val reader = new VariableEncodingStreamReader(
      rollbackStream,Charset.forName("UTF-8"))
    
    reader.read should equal ('\u0041')
    reader.read should equal ('\u0042')
    reader.read should equal ('\u0043')    

    rollbackStream checkpoint()

    reader.read should equal ('\u0044')
    reader.read should equal ('\u0C20')
    reader.read should equal ('\u0C21')
    reader.read should equal ('\u0C22')
    reader.read should equal ('\u0C23')
    reader.read should equal ('\u0C24')
    reader.read should equal ('\u0C25')
    reader.read should equal ('\u0C26')
    reader.read should equal (-1)
    
    rollbackStream rollback
    
    reader.read should equal ('\u0044')
    reader.read should equal ('\u0C20')
    reader.read should equal ('\u0C21')
    reader.read should equal ('\u0C22')
  }

  test("reading unicode and rolling back (III)"){
    val telugu = 
      "\u0041\u0042\u0043\u0044\u0C20\u0C21\u0C22\u0C23\u0C24\u0C25\u0C26"
    val input = new ByteArrayInputStream(telugu getBytes("UTF-8"))    
    val rollbackStream = new RollbackStream(input)
    val reader = new VariableEncodingStreamReader(
      rollbackStream,Charset.forName("UTF-8"))
    
    reader.read should equal ('\u0041')
    reader.read should equal ('\u0042')
    reader.read should equal ('\u0043')    
    reader.read should equal ('\u0044')    
    reader.read should equal ('\u0C20')
    reader.read should equal ('\u0C21')

    rollbackStream checkpoint()

    reader.read should equal ('\u0C22')
    reader.read should equal ('\u0C23')
    reader.read should equal ('\u0C24')
    reader.read should equal ('\u0C25')
    reader.read should equal ('\u0C26')
    reader.read should equal (-1)
    
    rollbackStream rollback
    
    reader.read should equal ('\u0C22')
    reader.read should equal ('\u0C23')
  }
  
  test("reading ASCII, then unicode") (pending)
  
  test("reading ASCII, then unicode, rollingback to ASCII") (pending)

  test("reading ASCII, then unicode, rollingback to unicode") (pending)

}
