package daffodil.schema.annotation.test.unit

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import daffodil.schema.annotation.enumerations._
import daffodil.schema.annotation.Annotation

import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._

class AnnotationTest extends JUnit3Suite with ShouldMatchers{
	
  def testSettingType() { // ("setting type") {
    
    val a1 = new Annotation(null); a1.format setTypeName("string")
    val a2 = new Annotation(null); a2.format setTypeName("integer")
    val b1 = new Annotation(null); b1.format setTypeName("string")
    val b2 = new Annotation(null); b2.format setTypeName("integer")
    val a3 = new Annotation(null);
    
    a1.format.typeName should equal (Some("string"))
    a2.format.typeName should equal (Some("integer"))
    b1.format.typeName should equal (Some("string"))
    b2.format.typeName should equal (Some("integer"))
    a3.format.typeName should equal (None)
  }
  
  def testSettingRepresentation() { // ("setting representation") {
    val a1 = new Annotation(null); a1.format setRepresentation("binary")
    val a2 = new Annotation(null); a2.format setRepresentation("text")
    val b1 = new Annotation(null); b1.format setRepresentation(Binary)
    val b2 = new Annotation(null); b2.format setRepresentation(Text)
    val a3 = new Annotation(null)
    
    a1.format.representation should equal (Some(Binary))
    a2.format.representation should equal (Some(Text))
    b1.format.representation should equal (Some(Binary))
    b2.format.representation should equal (Some(Text))
    a3.format.representation should equal (None)
  }
//  
//  def test() { // ("setting byte order") {
//    val a1 = new Annotation(null) + (AnnotationKeys.BYTE_ORDER_KEY,null,"big-Endian")
//    val a2 = new Annotation(null) + (AnnotationKeys.BYTE_ORDER_KEY,null,"little-Endian")
//    val b1 = new Annotation(null).setByteOrder(BigEndian)
//    val b2 = new Annotation(null).setByteOrder(LittleEndian)
//    val a3 = new Annotation(null)
//    
//    a1.getByteOrder() should equal (Some(BigEndian))
//    a2.getByteOrder() should equal (Some(LittleEndian))
//    b1.getByteOrder() should equal (Some(BigEndian))
//    b2.getByteOrder() should equal (Some(LittleEndian))
//    a3.getByteOrder() should equal (None)
//  }
// 
//  def test() { // ("setting encoding") {
//    val a1 = new Annotation(null) + (AnnotationKeys.ENCODING_KEY,null,"UTF-8")
//    val a2 = new Annotation(null) + (AnnotationKeys.ENCODING_KEY,null,"UTF-16")
//    val b1 = new Annotation(null).setEncoding(Charset.forName("UTF-8"))
//    val b2 = new Annotation(null).setEncoding(Charset.forName("UTF-16"))
//    val a3 = new Annotation(null)
//    
//    a1.getEncoding should equal (Some(Charset.forName("UTF-8")))
//    a2.getEncoding should equal (Some(Charset.forName("UTF-16")))
//    b1.getEncoding should equal (Some(Charset.forName("UTF-8")))
//    b2.getEncoding should equal (Some(Charset.forName("UTF-16")))
//    a3.getEncoding should equal (None)
//  }  
//  
//  def test() { // ("setting length") {
//    val a1 = new Annotation(null) + (AnnotationKeys.LENGTH_KEY,null,"42")
//    val b1 = new Annotation(null).setLength(42)
//    val a2 = new Annotation(null)
//    
//    a1.getLength should equal (Some(42))
//    b1.getLength should equal (Some(42))
//    a2.getLength should equal (None)
//  }
//  
//  def test() { // ("setting length kind") {
//    val a1 = new Annotation(null) + (AnnotationKeys.LENGTH_KIND_KEY,null,"delimited")
//    val a2 = new Annotation(null) + (AnnotationKeys.LENGTH_KIND_KEY,null,"endOfParent")
//    val a3 = new Annotation(null) + (AnnotationKeys.LENGTH_KIND_KEY,null,"explicit")
//    val a4 = new Annotation(null) + (AnnotationKeys.LENGTH_KIND_KEY,null,"implicit")
//    val a5 = new Annotation(null) + (AnnotationKeys.LENGTH_KIND_KEY,null,"pattern")
//    val a6 = new Annotation(null) + (AnnotationKeys.LENGTH_KIND_KEY,null,"prefixed")
//    val b1 = new Annotation(null).setLengthKind(Delimited)
//    val b2 = new Annotation(null).setLengthKind(EndOfParent)
//    val b3 = new Annotation(null).setLengthKind(Explicit)
//    val b4 = new Annotation(null).setLengthKind(Implicit)
//    val b5 = new Annotation(null).setLengthKind(Pattern)
//    val b6 = new Annotation(null).setLengthKind(Prefixed)    
//    val c = new Annotation(null)
//    
//    a1.getLengthKind should equal (Some(Delimited))
//    a2.getLengthKind should equal (Some(EndOfParent))
//    a3.getLengthKind should equal (Some(Explicit))
//    a4.getLengthKind should equal (Some(Implicit))
//    a5.getLengthKind should equal (Some(Pattern))
//    a6.getLengthKind should equal (Some(Prefixed))
//    b1.getLengthKind should equal (Some(Delimited))
//    b2.getLengthKind should equal (Some(EndOfParent))
//    b3.getLengthKind should equal (Some(Explicit))
//    b4.getLengthKind should equal (Some(Implicit))
//    b5.getLengthKind should equal (Some(Pattern))
//    b6.getLengthKind should equal (Some(Prefixed))
//    c.getLengthKind should equal (None)
//  }
//    
//  def test() { // ("setting terminator") {
//    val a1 = new Annotation(null) + (AnnotationKeys.TERMINATOR_KEY,null,"a terminator")
//    val b1 = new Annotation(null).setTerminator("a terminator")
//    val c = new Annotation(null)
//    
//    a1.getTerminator() should equal (Some("a terminator"))
//    b1.getTerminator() should equal (Some("a terminator"))
//    c.getTerminator() should equal (None)
//  }
//  
//  def test() { // ("setting initiator") {
//    val a1 = new Annotation(null) + (AnnotationKeys.INITIATOR_KEY,null,"an initiator")
//    val b1 = new Annotation(null).setInitiator("an initiator")
//    val c = new Annotation(null)
//    
//    a1.getInitiator() should equal (Some("an initiator"))
//    b1.getInitiator() should equal (Some("an initiator"))
//    c.getInitiator() should equal (None)
//  }
//  
//  def test() { // ("setting separator") {
//    val a1 = new Annotation(null) + (AnnotationKeys.SEPARATOR_KEY,null,"a separator")
//    val b1 = new Annotation(null).setSeparator("a separator")
//    val c = new Annotation(null)
//    
//    a1.getSeparator() should equal (Some("a separator"))
//    b1.getSeparator() should equal (Some("a separator"))
//    c.getSeparator() should equal (None)
//  }
//  
//  def test() { // ("setting separator position") {
//    val a1 = new Annotation(null) + (AnnotationKeys.SEPARATOR_POSITION_KEY,null,"infix")
//    val a2 = new Annotation(null) + (AnnotationKeys.SEPARATOR_POSITION_KEY,null,"prefix")
//    val a3 = new Annotation(null) + (AnnotationKeys.SEPARATOR_POSITION_KEY,null,"postfix")
//    val b1 = new Annotation(null).setSeparatorPosition(Infix)
//    val b2 = new Annotation(null).setSeparatorPosition(Prefix)
//    val b3 = new Annotation(null).setSeparatorPosition(Postfix)
//    val c = new Annotation(null)
//    
//    a1.getSeparatorPosition should equal (Some(Infix))
//    a2.getSeparatorPosition should equal (Some(Prefix))
//    a3.getSeparatorPosition should equal (Some(Postfix))
//    b1.getSeparatorPosition should equal (Some(Infix))
//    b2.getSeparatorPosition should equal (Some(Prefix))
//    b3.getSeparatorPosition should equal (Some(Postfix))
//    c.getSeparatorPosition should equal (None)
//  }
//  
//  def test() { // ("setting separator policy") {
//    val a1 = new Annotation(null) + (AnnotationKeys.SEPARATOR_POLICY_KEY,null,"require")
//    val a2 = new Annotation(null) + (AnnotationKeys.SEPARATOR_POLICY_KEY,null,"supress")
//    val a3 = new Annotation(null) + (AnnotationKeys.SEPARATOR_POLICY_KEY,null,"supressAtEnd")
//    val b1 = new Annotation(null).setSeparatorPolicy(Require)
//    val b2 = new Annotation(null).setSeparatorPolicy(Supress)
//    val b3 = new Annotation(null).setSeparatorPolicy(SupressAtEnd)
//    val c = new Annotation(null)
//    
//    a1.getSeparatorPolicy should equal (Some(Require))
//    a2.getSeparatorPolicy should equal (Some(Supress))
//    a3.getSeparatorPolicy should equal (Some(SupressAtEnd))
//    b1.getSeparatorPolicy should equal (Some(Require))
//    b2.getSeparatorPolicy should equal (Some(Supress))
//    b3.getSeparatorPolicy should equal (Some(SupressAtEnd))
//    c.getSeparatorPolicy should equal (None)
//  }
//  
//  def test() { // ("setting maxOccurs") {
//    val a1 = new Annotation(null) + (AnnotationKeys.MAX_OCCURS_KEY,null,"10")
//    val a2 = new Annotation(null) + (AnnotationKeys.MAX_OCCURS_KEY,null,"unbounded")
//    val b1 = new Annotation(null).setMaxOccurs(10)
//    val b2 = new Annotation(null).setMaxOccurs(-1)
//    val c = new Annotation(null)
//    
//    a1.getMaxOccurs() should equal (Some(10))
//    a2.getMaxOccurs() should equal (Some(-1))
//    b1.getMaxOccurs() should equal (Some(10))
//    b2.getMaxOccurs() should equal (Some(-1))
//    c.getMaxOccurs() should equal (None)
//  }
//  
//  def test() { // ("setting minOccurs") {
//    val a1 = new Annotation(null) + (AnnotationKeys.MIN_OCCURS_KEY,null,"10")
//    val b1 = new Annotation(null).setMinOccurs(10)
//    val c = new Annotation(null)
//    
//    a1.getMinOccurs() should equal (Some(10))
//    b1.getMinOccurs() should equal (Some(10))
//    c.getMinOccurs() should equal (None)
//  }
//  
//  def test() { // ("setting OccursCount") {
//    val a1 = new Annotation(null) + (AnnotationKeys.OCCURS_COUNT_KEY,null,"10")
//    val b1 = new Annotation(null).setOccursCount(10)
//    val c = new Annotation(null)
//    
//    a1.getOccursCount() should equal (Some(10))
//    b1.getOccursCount() should equal (Some(10))
//    c.getOccursCount() should equal (None)
//  }
//    
//  def test() { // ("setting OccursCountKind"){
//    val a1 = new Annotation(null) + (AnnotationKeys.OCCURS_COUNT_KIND_KEY,null,"expression")
//    val a2 = new Annotation(null) + (AnnotationKeys.OCCURS_COUNT_KIND_KEY,null,"fixed")
//    val a3 = new Annotation(null) + (AnnotationKeys.OCCURS_COUNT_KIND_KEY,null,"parsed")
//    val a4 = new Annotation(null) + (AnnotationKeys.OCCURS_COUNT_KIND_KEY,null,"stopValue")
//    val a5 = new Annotation(null) + (AnnotationKeys.OCCURS_COUNT_KIND_KEY,null,"useAvailableSpace")
//    val b1 = new Annotation(null).setOccursCountKind(Expression)
//    val b2 = new Annotation(null).setOccursCountKind(Fixed)
//    val b3 = new Annotation(null).setOccursCountKind(Parsed)
//    val b4 = new Annotation(null).setOccursCountKind(StopValue)
//    val b5 = new Annotation(null).setOccursCountKind(UseAvailableSpace)
//    val c = new Annotation(null)
//    
//    a1.getOccursCountKind should equal (Some(Expression))
//    a2.getOccursCountKind should equal (Some(Fixed))
//    a3.getOccursCountKind should equal (Some(Parsed))
//    a4.getOccursCountKind should equal (Some(StopValue))
//    a5.getOccursCountKind should equal (Some(UseAvailableSpace))
//    b1.getOccursCountKind should equal (Some(Expression))
//    b2.getOccursCountKind should equal (Some(Fixed))
//    b3.getOccursCountKind should equal (Some(Parsed))
//    b4.getOccursCountKind should equal (Some(StopValue))
//    b5.getOccursCountKind should equal (Some(UseAvailableSpace))
//    c.getOccursCountKind should equal (None)
//  }
//  
//  def test() { // ("setting OccursStopValue") {
//    val a1 = new Annotation(null) + (AnnotationKeys.OCCURS_STOP_VALUE_KEY,null,"a value")
//    val b1 = new Annotation(null).setOccursStopValue("a value")
//    val c = new Annotation(null)
//    
//    a1.getOccursStopValue() should equal (Some("a value"))
//    b1.getOccursStopValue() should equal (Some("a value"))
//    c.getOccursStopValue() should equal (None)
//  }
//
//  def test() { // ("setting NUmberPattern") {
//    val a1 = new Annotation(null) + (AnnotationKeys.NUMBER_PATTERN_KEY,null,"a value")
//    val b1 = new Annotation(null).setNumberPattern("a value")
//    val c = new Annotation(null)
//    
//    a1.getNumberPattern() should equal (Some("a value"))
//    b1.getNumberPattern() should equal (Some("a value"))
//    c.getNumberPattern() should equal (None)
//  }
//
//  def test() { // ("setting DecimalSeparator") {
//    val a1 = new Annotation(null) + (AnnotationKeys.DECIMAL_SEPARATOR_KEY,null,"a value")
//    val b1 = new Annotation(null).setDecimalSeparator("a value")
//    val c = new Annotation(null)
//    
//    a1.getDecimalSeparator() should equal (Some("a value"))
//    b1.getDecimalSeparator() should equal (Some("a value"))
//    c.getDecimalSeparator() should equal (None)
//  }
//
//  def test() { // ("setting BinaryFloatRepresentation") (pending)
//
//  def test() { // ("setting ignoreCase") {
//    val a1 = new Annotation(null) + (AnnotationKeys.IGNORE_CASE_KEY,null,"true")
//    val a2 = new Annotation(null) + (AnnotationKeys.IGNORE_CASE_KEY,null,"false")
//    val b1 = new Annotation(null).setIgnoreCase(true)
//    val b2 = new Annotation(null).setIgnoreCase(false)
//    val c = new Annotation(null)
//    
//    a1.getIgnoreCase() should equal (Some(true))
//    a2.getIgnoreCase() should equal (Some(false))
//    b1.getIgnoreCase() should equal (Some(true))
//    b2.getIgnoreCase() should equal (Some(false))
//    c.getIgnoreCase() should equal (None)
//  
//  }
//    
//  def test() { // ("setting base") {
//    val a1 = new Annotation(null) + (AnnotationKeys.BASE_KEY,null,"21")
//    val b1 = new Annotation(null).setBase(21)
//    val a2 = new Annotation(null)
//    
//    a1.getBase() should equal (Some(21))
//    b1.getBase() should equal (Some(21))
//    a2.getBase() should equal (None)
//  }
//    
//  def test() { // ("setting with illegal values") {
//    evaluating { new Annotation(null).setRepresentation("illegal value") } should produce [IllegalArgumentException]
//    evaluating { new Annotation(null).setByteOrder("illegal value") } should produce [IllegalArgumentException]
//    evaluating { new Annotation(null).setEncoding("illegal value") } should produce [IllegalCharsetNameException]
//    evaluating { new Annotation(null).setBase("illegal value") } should produce [NumberFormatException]
//    evaluating { new Annotation(null).setLength("illegal value") } should produce [NumberFormatException]    
//    evaluating { new Annotation(null).setLengthKind("illegal value") } should produce [IllegalArgumentException]
//    evaluating { new Annotation(null).setOccursCountKind("illegal value") } should produce [IllegalArgumentException]
//    //TODO implement all the others
//  }
//  
//  def test() { // ("annotations should be immutable") {
//    val a1 = new Annotation(null)
//    val a2 = a1.setBase(10)
//    val a3 = a2.setBase(20)
//    
//    a1 ++ a2
//    
//    a1.getBase should equal (None)
//    a2.getBase should equal (Some(10))
//    a3.getBase should equal (Some(20))
//  }
//  
//  def test() { // ("adding multiple values") {
//    val a1 = new Annotation(null)
//    val a2 = a1.setRepresentation(Text).
//      setEncoding(Charset.forName("UTF-8")).setLength(10).setTypeName("int").
//                    setTerminator("term").setInitiator("init").
//                    setBase(11).setSeparator("sep").setByteOrder(BigEndian)
//    
//    a2.getRepresentation should equal (Some(Text))
//    a2.getEncoding should equal (Some(Charset.forName("UTF-8")))
//    a2.getLength should equal (Some(10))
//    a2.getType should equal (Some("int"))
//    a2.getTerminator should equal (Some("term"))
//    a2.getInitiator should equal (Some("init"))
//    a2.getBase should equal (Some(11))
//    a2.getSeparator should equal (Some("sep"))
//    a2.getByteOrder should equal (Some(BigEndian)                                                         )
//  }
//    
//  def test() { // ("combining multiple annotations") {
//    val a1 = new Annotation(null).setLength(20).setTypeName("int")    
//    val a2 = new Annotation(null).setBase(10).setInitiator("init")
//    val a3 = a1 ++ a2
//    
//    a3.getLength should equal (Some(20))
//    a3.getType should equal (Some("int"))
//    a3.getBase should equal (Some(10))
//    a3.getInitiator should equal (Some("init"))
//  }
//  
//  def test() { // ("overriding values by readding") {
//    val a1 = new Annotation(null).setBase(5)
//    val a2 = a1.setLength(20)
//    val a3 = a2.setLength(30)
//    val a4 = a3.setLength(40)
//    
//    a1.getLength should equal (None)
//    a1.getBase should equal (Some(5))
//    a2.getLength should equal (Some(20))
//    a2.getBase should equal (Some(5))
//    a3.getLength should equal (Some(30))
//    a3.getBase should equal (Some(5))
//    a4.getLength should equal (Some(40))
//    a4.getBase should equal (Some(5))
//  }
//  
//  def test() { // ("overriding values by combining annotations") {
//    val a1 = new Annotation(null).setRepresentation(Text).setBase(5).setLength(10).setTypeName("int")
//    val a2 = new Annotation(null).setTerminator("term").setBase(8).setByteOrder(BigEndian).setTypeName("long")
//    
//    val a3 = a1 ++ a2
//    
//    a3.getRepresentation should equal (Some(Text))
//    a3.getTerminator should equal (Some("term"))
//    a3.getBase should equal (Some(8))
//    a3.getLength should equal (Some(10))
//    a3.getByteOrder should equal (Some(BigEndian))
//    a3.getType should equal (Some("long"))
//  }
   
}
