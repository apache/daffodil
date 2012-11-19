package daffodil.processors.input

import org.scalatest.junit.JUnitSuite
import junit.framework.Assert._
import scala.util.parsing.combinator._
import java.io.StringReader
import org.junit.Test
import daffodil.exceptions.Assert

import daffodil.processors.DFDLRegularExpressions
import daffodil.schema.annotation.props.gen.TextStringJustification

class TestDFDLRegularExpressions extends JUnitSuite {

  def escape = "E"
  def escapeEscape = "S"
  def delim = """\_*D\_*"""
  def padChar = "P"
  def bStart = "T"
  def bEnd = "N"

  @Test def testEscapeRegExNoPadding = {
    val cp = DFDLRegularExpressions.getEscapeRegEx(escape, escapeEscape, delim)
    def test(x: String) = x match {
      case cp(before, delim, after) => Some((before, delim))
      case z => None
    }
    assertEquals(Some("before", "D"), test("beforeDafter"))
    assertEquals(Some("beforeEDstillBefore", "D"), test("beforeEDstillBeforeDafter"))
    assertEquals(Some("beforeSE", "D"), test("beforeSEDstillBeforeDafter"))
    assertEquals(Some("beforeEEDstillBefore", "D"), test("beforeEEDstillBeforeDafter"))
    assertEquals(None, test("beforeE"))
    assertEquals(Some("PPPbeforeEDstillBeforePPP", "D"), test("PPPbeforeEDstillBeforePPPDafter"))
    assertEquals(Some("beforeS", "D"), test("beforeSDstillBeforeDafter"))
  }

  @Test def testEscapeRegExLeftJustified = {
    val cp = DFDLRegularExpressions.getEscapeRegExWithPadding(escape, escapeEscape, delim, padChar, TextStringJustification.Left)
    def test(x: String) = x match {
      case cp(before, delim, after) => Some((before, delim))
      case z => None
    }
    assertEquals(Some(("PPPbefore", "D")), test("PPPbeforePPPDafter"))
    assertEquals(Some(("PPPbefore", "D")), test("PPPbeforeDafter"))
    assertEquals(Some(("before", "D")), test("beforeDafter"))
    assertEquals(None, test("PPPbeforeEDafter"))
  }

  @Test def testEscapeRegExRightJustified = {
    val cp = DFDLRegularExpressions.getEscapeRegExWithPadding(escape, escapeEscape, delim, padChar, TextStringJustification.Right)
    def test(x: String) = x match {
      case cp(before, delim, after) => Some((before, delim))
      case z => None
    }
    assertEquals(Some(("beforePPP", "D")), test("PPPbeforePPPDafter"))
    assertEquals(Some(("beforePPP", "D")), test("beforePPPDafter"))
    assertEquals(Some(("before", "D")), test("beforeDafter"))
    assertEquals(None, test("PPPbeforeEDafter"))
  }

  @Test def testEscapeRegExCenterJustified = {
    val cp = DFDLRegularExpressions.getEscapeRegExWithPadding(escape, escapeEscape, delim, padChar, TextStringJustification.Center)
    def test(x: String) = x match {
      case cp(before, delim, after) => Some((before, delim))
      case z => None
    }
    assertEquals(Some(("beforePPPEDmore", "D")), test("PPPbeforePPPEDmoreDafter"))
    //assertEquals(Some(("beforePPPEDmoreEP", "D")), test("PPPbeforePPPEDmoreEPPPDafter"))
    assertEquals(Some(("before", "D")), test("PPPbeforePPPDafter"))
    assertEquals(Some(("before", "D")), test("beforeDafter"))
    assertEquals(Some(("before", "D")), test("beforePPPDafter"))
    assertEquals(Some(("before", "D")), test("PPPbeforeDafter"))
    assertEquals(None, test("PPPbeforeEDafter"))
  }

  @Test def testEscapeBlockRegExNoPadding = {
    val cp = DFDLRegularExpressions.getEscapeBlockRegEx(bStart, bEnd, escapeEscape, escape, delim)
    def test(x: String) = x match {
      case cp(before, delim, after, null, null, null) => {
        Some((before, delim, after))
      }
      case cp(null, null, null, before, delim, after) => {
        Some((before, delim, after))
      }
      case z => {
        // println("no match: " + z); 
        None
      }
    }
    // This particular regex lets you escape either the blockstart or blockend, or the delimiter

    // no blockstart/end
    assertEquals(Some(("before", "D", "after")), test("beforeDafter"))

    // no blockstart/end, but escape the delimiter
    assertEquals(Some(("beforeEDstillBefore", "D", "after")), test("beforeEDstillBeforeDafter"))

    // with blockstart/end
    assertEquals(Some(("beforeDstillBefore", "D", "after")), test("TbeforeDstillBeforeNDafter"))

    // with blockstart/end and esc and escEsc found inside (where they are inactive)
    assertEquals(Some(("beforeEDstillBeforeSEDstillBefore", "D", "after")), test("TbeforeEDstillBeforeSEDstillBeforeNDafter"))
    // Note: in the above, the SED is ok. No postprocessing. It is escaped in entirety by the T---N pair.

    // with blockstart/end, escape the first block end
    assertEquals(Some(("beforeDstillBeforeENstillBefore", "D", "after")), test("TbeforeDstillBeforeENstillBeforeNDafter"))

    // with blockstart/end, escapeEscape the escape of the first block end
    assertEquals(Some(("beforeDstillBeforeTstillBeforeSE", "D", "after")), test("TbeforeDstillBeforeTstillBeforeSENDafter"))

    // with blockstart, but escape it so it's not really a block.
    assertEquals(Some(("ETbefore", "D", "afterNstillafter")), test("ETbeforeDafterNstillafter"))
  }

  @Test def testEscapeBlockRegExLeftJustified = {
    val cp = DFDLRegularExpressions.getEscapeBlockRegExWithPadding(bStart, bEnd, escapeEscape, escape, padChar, delim, TextStringJustification.Left)
    def test(x: String) = x match {
      case cp(before, delim, after, null, null, null) => {
        Some((before, delim, after))
      }
      case cp(null, null, null, before, delim, after) => {
        Some((before, delim, after))
      }
      case z => {
        // println("no match: " + z); 
        None
      }
    }
    // This particular regex lets you escape either the blockstart or blockend, or the delimiter

    // Because this is Left justified the padding at the start of the field is actually
    // considered to be content.  This breaks the normal behavior of the blockstart and blockend
    // because it is required that blockstart must occur at the very beginning of the content. 

    // no blockstart/end
    assertEquals(Some(("PPPbefore", "D", "after")), test("PPPbeforePPPDafter"))

    // no blockstart/end, but escape the delimiter
    assertEquals(Some(("PPPbeforeEDstillBefore", "D", "after")), test("PPPbeforeEDstillBeforePPPDafter"))

    // with blockstart/end
    assertEquals(Some(("PPPTbefore", "D", "stillBeforeNPPPDafter")), test("PPPTbeforeDstillBeforeNPPPDafter"))

    // with blockstart/end and esc and escEsc found inside (where they are inactive)
    assertEquals(Some(("PPPTbeforeEDstillBeforeSE", "D", "stillBeforeNPPPDafter")), test("PPPTbeforeEDstillBeforeSEDstillBeforeNPPPDafter"))
    // Note: in the above, the SED is ok. No postprocessing. It is escaped in entirety by the T---N pair.

    // with blockstart/end, escape the first block end
    assertEquals(Some(("PPPTbefore", "D", "stillBeforeENstillBeforeNPPPDafter")), test("PPPTbeforeDstillBeforeENstillBeforeNPPPDafter"))

    // with blockstart/end, escapeEscape the escape of the first block end
    assertEquals(Some(("PPPTbefore", "D", "stillBeforeTstillBeforeSENPPPDafter")), test("PPPTbeforeDstillBeforeTstillBeforeSENPPPDafter"))

    // with blockstart, but escape it so it's not really a block.
    assertEquals(Some(("PPPETbefore", "D", "afterNstillafter")), test("PPPETbeforePPPDafterNstillafter"))
  }
  
  @Test def testEscapeBlockRegExRightJustified = {
    val cp = DFDLRegularExpressions.getEscapeBlockRegExWithPadding(bStart, bEnd, escapeEscape, escape, padChar, delim, TextStringJustification.Right)
    def test(x: String) = x match {
      case cp(before, delim, after, null, null, null) => {
        Some((before, delim, after))
      }
      case cp(null, null, null, before, delim, after) => {
        Some((before, delim, after))
      }
      case z => {
        // println("no match: " + z); 
        None
      }
    }
    // This particular regex lets you escape either the blockstart or blockend, or the delimiter

    // no blockstart/end
    assertEquals(Some(("beforePPP", "D", "after")), test("PPPbeforePPPDafter"))

    // no blockstart/end, but escape the delimiter
    assertEquals(Some(("beforeEDstillBeforePPP", "D", "after")), test("PPPbeforeEDstillBeforePPPDafter"))

    // with blockstart/end
    assertEquals(Some(("beforeDstillBefore", "D", "after")), test("PPPTbeforeDstillBeforeNDafter"))

    // with blockstart/end and esc and escEsc found inside (where they are inactive)
    assertEquals(Some(("beforeEDstillBeforeSEDstillBefore", "D", "after")), test("PPPTbeforeEDstillBeforeSEDstillBeforeNDafter"))
    // Note: in the above, the SED is ok. No postprocessing. It is escaped in entirety by the T---N pair.

    // with blockstart/end, escape the first block end
    assertEquals(Some(("beforeDstillBeforeENstillBefore", "D", "after")), test("PPPTbeforeDstillBeforeENstillBeforeNDafter"))

    // with blockstart/end, escapeEscape the escape of the first block end
    assertEquals(Some(("beforeDstillBeforeTstillBeforeSE", "D", "after")), test("PPPTbeforeDstillBeforeTstillBeforeSENDafter"))

    // with blockstart, but escape it so it's not really a block.
    assertEquals(Some(("ETbeforePPP", "D", "afterNstillafter")), test("PPPETbeforePPPDafterNstillafter"))
  }

  @Test def testEscapeBlockRegExCenterJustified = {
    val cp = DFDLRegularExpressions.getEscapeBlockRegExWithPadding(bStart, bEnd, escapeEscape, escape, padChar, delim, TextStringJustification.Center)
    def test(x: String) = x match {
      case cp(before, delim, after, null, null, null) => {
        Some((before, delim, after))
      }
      case cp(null, null, null, before, delim, after) => {
        Some((before, delim, after))
      }
      case z => {
        // println("no match: " + z); 
        None
      }
    }
    // This particular regex lets you escape either the blockstart or blockend, or the delimiter

    // no blockstart/end
    assertEquals(Some(("before", "D", "after")), test("PPPbeforePPPDafter"))

    // no blockstart/end, but escape the delimiter
    assertEquals(Some(("beforeEDstillBefore", "D", "after")), test("PPPbeforeEDstillBeforePPPDafter"))

    // with blockstart/end
    assertEquals(Some(("beforeDstillBefore", "D", "after")), test("PPPTbeforeDstillBeforeNPPPDafter"))

    // with blockstart/end and esc and escEsc found inside (where they are inactive)
    assertEquals(Some(("beforeEDstillBeforeSEDstillBefore", "D", "after")), test("PPPTbeforeEDstillBeforeSEDstillBeforeNPPPDafter"))
    // Note: in the above, the SED is ok. No postprocessing. It is escaped in entirety by the T---N pair.

    // with blockstart/end, escape the first block end
    assertEquals(Some(("beforeDstillBeforeENstillBefore", "D", "after")), test("PPPTbeforeDstillBeforeENstillBeforeNPPPDafter"))

    // with blockstart/end, escapeEscape the escape of the first block end
    assertEquals(Some(("beforeDstillBeforeTstillBeforeSE", "D", "after")), test("PPPTbeforeDstillBeforeTstillBeforeSENPPPDafter"))

    // with blockstart, but escape it so it's not really a block.
    assertEquals(Some(("ETbefore", "D", "afterNstillafter")), test("PPPETbeforePPPDafterNstillafter"))
  }
}