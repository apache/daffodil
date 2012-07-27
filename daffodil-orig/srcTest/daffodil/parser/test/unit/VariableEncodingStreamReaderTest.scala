package daffodil.parser.test.unit

import daffodil.parser._

import java.io.ByteArrayInputStream
import java.nio.charset.Charset

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import daffodil.parser.RollbackStream
import daffodil.Implicits._

import org.scalatest.junit.JUnit3Suite
import scala.collection.mutable.ListBuffer
import junit.framework.Assert._

class VariableEncodingStreamReaderTest extends JUnit3Suite with ShouldMatchers {
  
  def testReadingTenCharsInASCIIOneByOne ()  { 
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
      rollbackStream,Charset.forName("US-ASCII"))

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

  def testReadingToEndOfFile ()  { 
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
      rollbackStream,Charset.forName("US-ASCII"))

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

  // def testReadingTenASCIICharacteresAllAtOnce ()  { //"reading ten ASCII characters, all at once") (pending)

  def testReadingUnicode ()  { 
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

  def testReadingUnicodeAndRollingBack ()  { 
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

  def testReadingUnicodeAndRollingBack2 ()  { 
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

  def testReadingUnicodeAndRollingBack3 ()  { 
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
  
//  def test ()  { //"reading ASCII, then unicode") (pending)
//  
//  def test ()  { //"reading ASCII, then unicode, rollingback to ASCII") (pending)
//
//  def test ()  { //"reading ASCII, then unicode, rollingback to unicode") (pending)

}
