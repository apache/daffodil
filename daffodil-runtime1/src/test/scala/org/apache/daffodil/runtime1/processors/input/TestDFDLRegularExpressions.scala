/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.runtime1.processors.input

import org.apache.daffodil.lib.schema.annotation.props.gen.TextStringJustification
import org.apache.daffodil.runtime1.processors.DFDLRegularExpressions

import org.junit.Assert._
import org.junit.Test

/**
 * The purpose of this class is to verify that the configurable
 * nature of the regular expressions does not negatively affect
 * the results.  Also verifies that the expressions still work
 * when multiple delimiters are used.
 */
class TestDFDLRegularExpressions {

  def escape = "E"
  def escapeEscape = "S"
  def delim = """\_*D\_*"""
  def padChar = "P"
  def bStart = "T"
  def bEnd = "N"

  def delims = "D|C|F"

  @Test def testSameEscapeRegExNoPadding() = {
    val cp = DFDLRegularExpressions.getSameEscapeRegEx(escape, delim)
    def test(x: String) = x match {
      case cp(before, rubbish, delim, after) => {
        // System.err.println(before + ", " + rubbish + ", " + delim + ", " + after)
        Some((before, delim))
      }
      case z => None
    }
    assertEquals(Some("before", "D"), test("beforeDafter"))
    assertEquals(Some("beforeEDstillBefore", "D"), test("beforeEDstillBeforeDafter"))
    assertEquals(Some("before", "D"), test("beforeEEDstillBeforeDafter"))
    assertEquals(None, test("beforeE"))
    assertEquals(
      Some("PPPbeforeEDstillBeforePPP", "D"),
      test("PPPbeforeEDstillBeforePPPDafter")
    )
    assertEquals(Some("beforeS", "D"), test("beforeSDstillBeforeDafter"))
    assertEquals(Some("beforeEEEDstillBefore", "D"), test("beforeEEEDstillBeforeDafter"))
    assertEquals(Some("before", "D"), test("beforeEEEEDafter"))

    // What about multiple delimiters?
    val cp2 = DFDLRegularExpressions.getSameEscapeRegEx(escape, delims)
    def test2(x: String) = x match {
      case cp2(before, rubbish, delim, after) => {
        // System.err.println(before + ", " + rubbish + ", " + delim + ", " + after)
        Some((before, delim))
      }
      case z => None
    }
    assertEquals(Some("before", "D"), test2("beforeDafter"))
    assertEquals(Some("before", "C"), test2("beforeCafter"))
    assertEquals(Some("before", "F"), test2("beforeFafter"))
    assertEquals(None, test2("beforeGafter"))
    assertEquals(Some("beforeEDstillBefore", "D"), test2("beforeEDstillBeforeDafter"))
    assertEquals(Some("before", "D"), test2("beforeEEDstillBeforeDafter"))
    assertEquals(None, test2("beforeE"))
    assertEquals(
      Some("PPPbeforeEDstillBeforePPP", "D"),
      test2("PPPbeforeEDstillBeforePPPDafter")
    )
    assertEquals(Some("beforeS", "D"), test2("beforeSDstillBeforeDafter"))
    assertEquals(Some("beforeEEEDstillBefore", "D"), test2("beforeEEEDstillBeforeDafter"))
    assertEquals(Some("before", "D"), test2("beforeEEEEDafter"))
  }

  @Test def testSameEscapeRegExLeftJustified() = {
    val cp = DFDLRegularExpressions.getSameEscapeRegExWithPadding(
      escape,
      delim,
      padChar,
      TextStringJustification.Left
    )
    def test(x: String) = x match {
      case cp(ee1s, before, ee2s, delimAfterPad, ee3s, delimAfterEEs, after) => {
        // System.err.println(before + ", " + rubbish + ", " + delim + ", " + after)
        val delim1 = if (delimAfterPad == null) delimAfterEEs else delimAfterPad
        Some((before, delim1))
      }
      case cp(before, rubbish, delim, null, null, after) => {
        Some((before, delim))
      }
      case cp(before, null, null, ee, delim, after) => {
        // System.err.println(a + ", " + b + ", " + c + "," + d + ", " + e + ", " + f)
        Some((before, delim))
      }
      case z => None
    }
    assertEquals(Some("before", "D"), test("beforeDafter"))
    assertEquals(Some("beforeEDstillBefore", "D"), test("beforeEDstillBeforePPPDafter"))
    assertEquals(Some("before", "D"), test("beforeEEDstillBeforeDafter"))
    assertEquals(None, test("beforeE"))
    assertEquals(Some("PPPbeforeEDstillBefore", "D"), test("PPPbeforeEDstillBeforePPPDafter"))
    assertEquals(Some("beforeS", "D"), test("beforeSDstillBeforeDafter"))
    assertEquals(Some("beforeEEEDstillBefore", "D"), test("beforeEEEDstillBeforeDafter"))
    assertEquals(Some("before", "D"), test("beforeEEEEDafter"))

    // What about multiple delimiters?
    val cp2 = DFDLRegularExpressions.getSameEscapeRegExWithPadding(
      escape,
      delims,
      padChar,
      TextStringJustification.Left
    )
    def test2(x: String) = x match {
      case cp2(ee1s, before, ee2s, delimAfterPad, ee3s, delimAfterEEs, after) => {
        // System.err.println(before + ", " + rubbish + ", " + delim + ", " + after)
        val delim1 = if (delimAfterPad == null) delimAfterEEs else delimAfterPad
        Some((before, delim1))
      }
      case cp2(before, rubbish, delim, null, null, after) => {
        Some((before, delim))
      }
      case cp2(before, null, null, ee, delim, after) => {
        // System.err.println(a + ", " + b + ", " + c + "," + d + ", " + e + ", " + f)
        Some((before, delim))
      }
      case z => None
    }
    assertEquals(Some("before", "D"), test2("beforeDafter"))
    assertEquals(Some("before", "C"), test2("beforeCafter"))
    assertEquals(Some("before", "F"), test2("beforeFafter"))
    assertEquals(None, test2("beforeGafter"))
    assertEquals(Some("beforeEDstillBefore", "D"), test2("beforeEDstillBeforePPPDafter"))
    assertEquals(Some("before", "D"), test2("beforeEEDstillBeforeDafter"))
    assertEquals(None, test("beforeE"))
    assertEquals(Some("PPPbeforeEDstillBefore", "D"), test2("PPPbeforeEDstillBeforePPPDafter"))
    assertEquals(Some("beforeS", "D"), test2("beforeSDstillBeforeDafter"))
    assertEquals(Some("beforeEEEDstillBefore", "D"), test2("beforeEEEDstillBeforeDafter"))
    assertEquals(Some("before", "D"), test2("beforeEEEEDafter"))
  }

  @Test def testSameEscapeRegExRightJustified() = {
    val cp = DFDLRegularExpressions.getSameEscapeRegExWithPadding(
      escape,
      delim,
      padChar,
      TextStringJustification.Right
    )
    def test(x: String) = x match {
      case cp(ee1s, before, ee2s, delimAfterPad, ee3s, delimAfterEEs, after) => {
        // System.err.println(before + ", " + rubbish + ", " + delim + ", " + after)
        val delim1 = if (delimAfterPad == null) delimAfterEEs else delimAfterPad
        Some((before, delim1))
      }
      case cp(before, rubbish, delim, null, null, after) => {
        Some((before, delim))
      }
      case cp(before, null, null, ee, delim, after) => {
        // System.err.println(a + ", " + b + ", " + c + "," + d + ", " + e + ", " + f)
        Some((before, delim))
      }
      case cp(rubbish, before, rubbish2, delim, after) => {
        Some((before, delim))
      }
      case z => None
    }
    assertEquals(Some("before", "D"), test("beforeDafter"))
    assertEquals(Some("beforeEDstillBefore", "D"), test("PPPbeforeEDstillBeforeDafter"))
    assertEquals(Some("before", "D"), test("PPPbeforeEEDstillBeforeDafter"))
    assertEquals(None, test("beforeE"))
    assertEquals(Some("beforeEDstillBeforePPP", "D"), test("PPPbeforeEDstillBeforePPPDafter"))
    assertEquals(Some("beforeS", "D"), test("beforeSDstillBeforePPPDafter"))
    assertEquals(Some("beforeEEEDstillBefore", "D"), test("beforeEEEDstillBeforeDafter"))
    assertEquals(Some("before", "D"), test("beforeEEEEDafter"))

    // What about multiple delimiters?
    val cp2 = DFDLRegularExpressions.getSameEscapeRegExWithPadding(
      escape,
      delims,
      padChar,
      TextStringJustification.Right
    )
    def test2(x: String) = x match {
      case cp2(ee1s, before, ee2s, delimAfterPad, ee3s, delimAfterEEs, after) => {
        // System.err.println(before + ", " + rubbish + ", " + delim + ", " + after)
        val delim1 = if (delimAfterPad == null) delimAfterEEs else delimAfterPad
        Some((before, delim1))
      }
      case cp2(before, rubbish, delim, null, null, after) => {
        Some((before, delim))
      }
      case cp2(before, null, null, ee, delim, after) => {
        // System.err.println(a + ", " + b + ", " + c + "," + d + ", " + e + ", " + f)
        Some((before, delim))
      }
      case cp2(rubbish, before, rubbish2, delim, after) => {
        Some((before, delim))
      }
      case z => None
    }
    assertEquals(Some("before", "D"), test2("beforeDafter"))
    assertEquals(Some("beforeEDstillBefore", "D"), test2("PPPbeforeEDstillBeforeDafter"))
    assertEquals(Some("before", "D"), test2("PPPbeforeEEDstillBeforeDafter"))
    assertEquals(None, test2("beforeE"))
    assertEquals(Some("beforeEDstillBeforePPP", "D"), test2("PPPbeforeEDstillBeforePPPDafter"))
    assertEquals(Some("beforeS", "D"), test2("beforeSDstillBeforePPPDafter"))
    assertEquals(Some("beforeEEEDstillBefore", "D"), test2("beforeEEEDstillBeforeDafter"))
    assertEquals(Some("before", "D"), test2("beforeEEEEDafter"))
  }

  @Test def testSameEscapeRegExCenterJustified() = {
    val cp = DFDLRegularExpressions.getSameEscapeRegExWithPadding(
      escape,
      delim,
      padChar,
      TextStringJustification.Center
    )
    def test(x: String) = x match {
      case cp(ee1s, before, ee2s, delimAfterPad, ee3s, delimAfterEEs, after) => {
        // System.err.println(before + ", " + rubbish + ", " + delim + ", " + after)
        val delim1 = if (delimAfterPad == null) delimAfterEEs else delimAfterPad
        Some((before, delim1))
      }
      case cp(before, rubbish, delim, null, null, after) => {
        Some((before, delim))
      }
      case cp(before, null, null, ee, delim, after) => {
        // System.err.println(a + ", " + b + ", " + c + "," + d + ", " + e + ", " + f)
        Some((before, delim))
      }
      case cp(rubbish, before, rubbish2, delim, after) => {
        Some((before, delim))
      }
      case z => None
    }
    assertEquals(Some("before", "D"), test("beforeDafter"))
    assertEquals(Some("beforeEDstillBefore", "D"), test("PPPbeforeEDstillBeforePPPDafter"))
    assertEquals(Some("beforePPP", "D"), test("PPPbeforePPPEEDafter"))
    assertEquals(None, test("beforeE"))
    assertEquals(
      Some("beforePPPEDstillBefore", "D"),
      test("PPPbeforePPPEDstillBeforePPPDafter")
    )
    assertEquals(Some("beforeS", "D"), test("beforeSDstillBeforePPPDafter"))
    assertEquals(Some("beforeEEEDstillBefore", "D"), test("beforeEEEDstillBeforeDafter"))
    assertEquals(Some("before", "D"), test("beforeEEEEDafter"))

    // What about multiple delimiters?
    val cp2 = DFDLRegularExpressions.getSameEscapeRegExWithPadding(
      escape,
      delims,
      padChar,
      TextStringJustification.Center
    )
    def test2(x: String) = x match {
      case cp2(ee1s, before, ee2s, delimAfterPad, ee3s, delimAfterEEs, after) => {
        // System.err.println(before + ", " + rubbish + ", " + delim + ", " + after)
        val delim1 = if (delimAfterPad == null) delimAfterEEs else delimAfterPad
        Some((before, delim1))
      }
      case cp2(before, rubbish, delim, null, null, after) => {
        Some((before, delim))
      }
      case cp2(before, null, null, ee, delim, after) => {
        // System.err.println(a + ", " + b + ", " + c + "," + d + ", " + e + ", " + f)
        Some((before, delim))
      }
      case cp2(rubbish, before, rubbish2, delim, after) => {
        Some((before, delim))
      }
      case z => None
    }
    assertEquals(Some("before", "D"), test2("beforeDafter"))
    assertEquals(Some("beforeEDstillBefore", "D"), test2("PPPbeforeEDstillBeforePPPDafter"))
    assertEquals(Some("beforePPP", "D"), test2("PPPbeforePPPEEDafter"))
    assertEquals(None, test("beforeE"))
    assertEquals(
      Some("beforePPPEDstillBefore", "D"),
      test2("PPPbeforePPPEDstillBeforePPPDafter")
    )
    assertEquals(Some("beforeS", "D"), test2("beforeSDstillBeforePPPDafter"))
    assertEquals(Some("beforeEEEDstillBefore", "D"), test2("beforeEEEDstillBeforeDafter"))
    assertEquals(Some("before", "D"), test2("beforeEEEEDafter"))
  }

  @Test def testEscapeRegExNoPadding() = {
    val cp = DFDLRegularExpressions.getEscapeRegEx(escape, escapeEscape, delim)

    def test(x: String) = x match {
      case cp(before, delim, blah, after) => Some((before, delim))
      case cp(before, delim, after) => Some((before, delim))
      case z => None
    }
    assertEquals(Some("before", "D"), test("beforeDafter"))
    assertEquals(Some("beforeEDstillBefore", "D"), test("beforeEDstillBeforeDafter"))
    assertEquals(Some("beforeSE", "D"), test("beforeSEDstillBeforeDafter"))
    assertEquals(Some("beforeEEDstillBefore", "D"), test("beforeEEDstillBeforeDafter"))
    assertEquals(None, test("beforeE"))
    assertEquals(
      Some("PPPbeforeEDstillBeforePPP", "D"),
      test("PPPbeforeEDstillBeforePPPDafter")
    )
    assertEquals(Some("beforeS", "D"), test("beforeSDstillBeforeDafter"))

    // Multiple delimiters
    val cp2 = DFDLRegularExpressions.getEscapeRegEx(escape, escapeEscape, delims)

    def test2(x: String) = x match {
      case cp2(before, delim, blah, after) => Some((before, delim))
      case cp2(before, delim, after) => Some((before, delim))
      case z => None
    }
    assertEquals(Some("before", "D"), test2("beforeDafter"))
    assertEquals(Some("beforeEDstillBefore", "D"), test2("beforeEDstillBeforeDafter"))
    assertEquals(Some("beforeSE", "D"), test2("beforeSEDstillBeforeDafter"))
    assertEquals(Some("beforeEEDstillBefore", "D"), test2("beforeEEDstillBeforeDafter"))
    assertEquals(None, test2("beforeE"))
    assertEquals(
      Some("PPPbeforeEDstillBeforePPP", "D"),
      test2("PPPbeforeEDstillBeforePPPDafter")
    )
    assertEquals(Some("beforeS", "D"), test2("beforeSDstillBeforeDafter"))
  }

  @Test def testEscapeRegExLeftJustified() = {
    val cp = DFDLRegularExpressions.getEscapeRegExWithPadding(
      escape,
      escapeEscape,
      delim,
      padChar,
      TextStringJustification.Left
    )
    def test(x: String) = x match {
      case cp(before, delim, after) => Some((before, delim))
      case z => None
    }
    assertEquals(Some(("PPPbefore", "D")), test("PPPbeforePPPDafter"))
    assertEquals(Some(("PPPbefore", "D")), test("PPPbeforeDafter"))
    assertEquals(Some(("before", "D")), test("beforeDafter"))
    assertEquals(None, test("PPPbeforeEDafter"))

    // Multiple Delimiters
    val cp2 = DFDLRegularExpressions.getEscapeRegExWithPadding(
      escape,
      escapeEscape,
      delims,
      padChar,
      TextStringJustification.Left
    )
    def test2(x: String) = x match {
      case cp2(before, delim, after) => Some((before, delim))
      case z => None
    }
    assertEquals(Some(("PPPbefore", "D")), test2("PPPbeforePPPDafter"))
    assertEquals(Some(("PPPbefore", "D")), test2("PPPbeforeDafter"))
    assertEquals(Some(("before", "D")), test2("beforeDafter"))
    assertEquals(None, test2("PPPbeforeEDafter"))
    assertEquals(Some(("before", "C")), test2("beforeCafter"))
    assertEquals(Some(("before", "F")), test2("beforeFafter"))
  }

  @Test def testEscapeRegExRightJustified() = {
    val cp = DFDLRegularExpressions.getEscapeRegExWithPadding(
      escape,
      escapeEscape,
      delim,
      padChar,
      TextStringJustification.Right
    )
    def test(x: String) = x match {
      case cp(before, delim, after) => Some((before, delim))
      case z => None
    }
    assertEquals(Some(("beforePPP", "D")), test("PPPbeforePPPDafter"))
    assertEquals(Some(("beforePPP", "D")), test("beforePPPDafter"))
    assertEquals(Some(("before", "D")), test("beforeDafter"))
    assertEquals(None, test("PPPbeforeEDafter"))

    // Multiple Delimiters
    val cp2 = DFDLRegularExpressions.getEscapeRegExWithPadding(
      escape,
      escapeEscape,
      delims,
      padChar,
      TextStringJustification.Right
    )
    def test2(x: String) = x match {
      case cp2(before, delim, after) => Some((before, delim))
      case z => None
    }
    assertEquals(Some(("beforePPP", "D")), test2("PPPbeforePPPDafter"))
    assertEquals(Some(("beforePPP", "D")), test2("beforePPPDafter"))
    assertEquals(Some(("before", "D")), test2("beforeDafter"))
    assertEquals(None, test("PPPbeforeEDafter"))
    assertEquals(Some(("before", "C")), test2("beforeCafter"))
    assertEquals(Some(("before", "F")), test2("beforeFafter"))
  }

  @Test def testEscapeRegExCenterJustified() = {
    val cp = DFDLRegularExpressions.getEscapeRegExWithPadding(
      escape,
      escapeEscape,
      delim,
      padChar,
      TextStringJustification.Center
    )
    def test(x: String) = x match {
      case cp(before, delim, after) => Some((before, delim))
      case z => None
    }
    assertEquals(Some(("beforePPPEDmore", "D")), test("PPPbeforePPPEDmoreDafter"))
    // assertEquals(Some(("beforePPPEDmoreEP", "D")), test("PPPbeforePPPEDmoreEPPPDafter"))
    assertEquals(Some(("before", "D")), test("PPPbeforePPPDafter"))
    assertEquals(Some(("before", "D")), test("beforeDafter"))
    assertEquals(Some(("before", "D")), test("beforePPPDafter"))
    assertEquals(Some(("before", "D")), test("PPPbeforeDafter"))
    assertEquals(None, test("PPPbeforeEDafter"))

    // Multiple Delimiters
    val cp2 = DFDLRegularExpressions.getEscapeRegExWithPadding(
      escape,
      escapeEscape,
      delims,
      padChar,
      TextStringJustification.Center
    )
    def test2(x: String) = x match {
      case cp2(before, delim, after) => Some((before, delim))
      case z => None
    }
    assertEquals(Some(("beforePPPEDmore", "D")), test2("PPPbeforePPPEDmoreDafter"))
    // assertEquals(Some(("beforePPPEDmoreEP", "D")), test("PPPbeforePPPEDmoreEPPPDafter"))
    assertEquals(Some(("before", "D")), test2("PPPbeforePPPDafter"))
    assertEquals(Some(("before", "D")), test2("beforeDafter"))
    assertEquals(Some(("before", "D")), test2("beforePPPDafter"))
    assertEquals(Some(("before", "D")), test2("PPPbeforeDafter"))
    assertEquals(None, test2("PPPbeforeEDafter"))
    assertEquals(Some(("before", "C")), test2("beforeCafter"))
    assertEquals(Some(("before", "F")), test2("beforeFafter"))
  }

  @Test def testEscapeBlockRegExNoPadding() = {
    val cp =
      DFDLRegularExpressions.getEscapeBlockRegEx(bStart, bEnd, escapeEscape, escape, delim)
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
    assertEquals(
      Some(("beforeEDstillBeforeSEDstillBefore", "D", "after")),
      test("TbeforeEDstillBeforeSEDstillBeforeNDafter")
    )
    // Note: in the above, the SED is ok. No postprocessing. It is escaped in entirety by the T---N pair.

    // with blockstart/end, escape the first block end
    assertEquals(
      Some(("beforeDstillBeforeENstillBefore", "D", "after")),
      test("TbeforeDstillBeforeENstillBeforeNDafter")
    )

    // with blockstart/end, escapeEscape the escape of the first block end
    assertEquals(
      Some(("beforeDstillBeforeTstillBeforeSE", "D", "after")),
      test("TbeforeDstillBeforeTstillBeforeSENDafter")
    )

    // with blockstart, but escape it so it's not really a block.
    assertEquals(Some(("ETbefore", "D", "afterNstillafter")), test("ETbeforeDafterNstillafter"))

    // Multiple delimiters
    val cp2 =
      DFDLRegularExpressions.getEscapeBlockRegEx(bStart, bEnd, escapeEscape, escape, delims)
    def test2(x: String) = x match {
      case cp2(before, delim, after, null, null, null) => {
        Some((before, delim, after))
      }
      case cp2(null, null, null, before, delim, after) => {
        Some((before, delim, after))
      }
      case z => {
        // println("no match: " + z);
        None
      }
    }
    // This particular regex lets you escape either the blockstart or blockend, or the delimiter

    // no blockstart/end
    assertEquals(Some(("before", "D", "after")), test2("beforeDafter"))

    // no blockstart/end, but escape the delimiter
    assertEquals(
      Some(("beforeEDstillBefore", "D", "after")),
      test2("beforeEDstillBeforeDafter")
    )

    // with blockstart/end
    assertEquals(
      Some(("beforeDstillBefore", "D", "after")),
      test2("TbeforeDstillBeforeNDafter")
    )

    // with blockstart/end and esc and escEsc found inside (where they are inactive)
    assertEquals(
      Some(("beforeEDstillBeforeSEDstillBefore", "D", "after")),
      test2("TbeforeEDstillBeforeSEDstillBeforeNDafter")
    )
    // Note: in the above, the SED is ok. No postprocessing. It is escaped in entirety by the T---N pair.

    // with blockstart/end, escape the first block end
    assertEquals(
      Some(("beforeDstillBeforeENstillBefore", "D", "after")),
      test2("TbeforeDstillBeforeENstillBeforeNDafter")
    )

    // with blockstart/end, escapeEscape the escape of the first block end
    assertEquals(
      Some(("beforeDstillBeforeTstillBeforeSE", "D", "after")),
      test2("TbeforeDstillBeforeTstillBeforeSENDafter")
    )

    // with blockstart, but escape it so it's not really a block.
    assertEquals(
      Some(("ETbefore", "D", "afterNstillafter")),
      test2("ETbeforeDafterNstillafter")
    )

    assertEquals(
      Some(("beforeDstillBeforeENstillBefore", "C", "after")),
      test2("TbeforeDstillBeforeENstillBeforeNCafter")
    )
    assertEquals(
      Some(("beforeDstillBeforeENstillBefore", "F", "after")),
      test2("TbeforeDstillBeforeENstillBeforeNFafter")
    )
  }

  @Test def testEscapeBlockRegExLeftJustified() = {
    val cp = DFDLRegularExpressions.getEscapeBlockRegExWithPadding(
      bStart,
      bEnd,
      escapeEscape,
      escape,
      padChar,
      delim,
      TextStringJustification.Left
    )
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
    assertEquals(
      Some(("PPPbeforeEDstillBefore", "D", "after")),
      test("PPPbeforeEDstillBeforePPPDafter")
    )

    // with blockstart/end
    assertEquals(
      Some(("PPPTbefore", "D", "stillBeforeNPPPDafter")),
      test("PPPTbeforeDstillBeforeNPPPDafter")
    )

    // with blockstart/end and esc and escEsc found inside (where they are inactive)
    assertEquals(
      Some(("PPPTbeforeEDstillBeforeSE", "D", "stillBeforeNPPPDafter")),
      test("PPPTbeforeEDstillBeforeSEDstillBeforeNPPPDafter")
    )
    // Note: in the above, the SED is ok. No postprocessing. It is escaped in entirety by the T---N pair.

    // with blockstart/end, escape the first block end
    assertEquals(
      Some(("PPPTbefore", "D", "stillBeforeENstillBeforeNPPPDafter")),
      test("PPPTbeforeDstillBeforeENstillBeforeNPPPDafter")
    )

    // with blockstart/end, escapeEscape the escape of the first block end
    assertEquals(
      Some(("PPPTbefore", "D", "stillBeforeTstillBeforeSENPPPDafter")),
      test("PPPTbeforeDstillBeforeTstillBeforeSENPPPDafter")
    )

    // with blockstart, but escape it so it's not really a block.
    assertEquals(
      Some(("PPPETbefore", "D", "afterNstillafter")),
      test("PPPETbeforePPPDafterNstillafter")
    )

    // here it would appear that the additional padding to the left of the block start
    // invalidates the escape scheme as we were only expecting padding to the right of block end
    assertEquals(Some(("PPPTbefore", "D", "NPPPDafter")), test("PPPTbeforeDNPPPDafter"))

    // Multiple Delimiters
    val cp2 = DFDLRegularExpressions.getEscapeBlockRegExWithPadding(
      bStart,
      bEnd,
      escapeEscape,
      escape,
      padChar,
      delims,
      TextStringJustification.Left
    )
    def test2(x: String) = x match {
      case cp2(before, delim, after, null, null, null) => {
        Some((before, delim, after))
      }
      case cp2(null, null, null, before, delim, after) => {
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
    assertEquals(Some(("PPPbefore", "D", "after")), test2("PPPbeforePPPDafter"))

    // no blockstart/end, but escape the delimiter
    assertEquals(
      Some(("PPPbeforeEDstillBefore", "D", "after")),
      test2("PPPbeforeEDstillBeforePPPDafter")
    )

    // with blockstart/end
    assertEquals(
      Some(("PPPTbefore", "D", "stillBeforeNPPPDafter")),
      test2("PPPTbeforeDstillBeforeNPPPDafter")
    )

    // with blockstart/end and esc and escEsc found inside (where they are inactive)
    assertEquals(
      Some(("PPPTbeforeEDstillBeforeSE", "D", "stillBeforeNPPPDafter")),
      test2("PPPTbeforeEDstillBeforeSEDstillBeforeNPPPDafter")
    )
    // Note: in the above, the SED is ok. No postprocessing. It is escaped in entirety by the T---N pair.

    // with blockstart/end, escape the first block end
    assertEquals(
      Some(("PPPTbefore", "D", "stillBeforeENstillBeforeNPPPDafter")),
      test2("PPPTbeforeDstillBeforeENstillBeforeNPPPDafter")
    )

    // with blockstart/end, escapeEscape the escape of the first block end
    assertEquals(
      Some(("PPPTbefore", "D", "stillBeforeTstillBeforeSENPPPDafter")),
      test2("PPPTbeforeDstillBeforeTstillBeforeSENPPPDafter")
    )

    // with blockstart, but escape it so it's not really a block.
    assertEquals(
      Some(("PPPETbefore", "D", "afterNstillafter")),
      test2("PPPETbeforePPPDafterNstillafter")
    )

    // here it would appear that the additional padding to the left of the block start
    // invalidates the escape scheme as we were only expecting padding to the right of block end
    assertEquals(Some(("PPPTbefore", "D", "NPPPDafter")), test2("PPPTbeforeDNPPPDafter"))

    assertEquals(
      Some(("PPPTbefore", "C", "stillBeforeTstillBeforeSENPPPDafter")),
      test2("PPPTbeforeCstillBeforeTstillBeforeSENPPPDafter")
    )
    assertEquals(
      Some(("PPPTbefore", "F", "stillBeforeTstillBeforeSENPPPDafter")),
      test2("PPPTbeforeFstillBeforeTstillBeforeSENPPPDafter")
    )
  }

  @Test def testEscapeBlockRegExRightJustified() = {
    val cp = DFDLRegularExpressions.getEscapeBlockRegExWithPadding(
      bStart,
      bEnd,
      escapeEscape,
      escape,
      padChar,
      delim,
      TextStringJustification.Right
    )
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
    assertEquals(
      Some(("beforeEDstillBeforePPP", "D", "after")),
      test("PPPbeforeEDstillBeforePPPDafter")
    )

    // with blockstart/end
    assertEquals(
      Some(("beforeDstillBefore", "D", "after")),
      test("PPPTbeforeDstillBeforeNDafter")
    )

    // with blockstart/end and esc and escEsc found inside (where they are inactive)
    assertEquals(
      Some(("beforeEDstillBeforeSEDstillBefore", "D", "after")),
      test("PPPTbeforeEDstillBeforeSEDstillBeforeNDafter")
    )
    // Note: in the above, the SED is ok. No postprocessing. It is escaped in entirety by the T---N pair.

    // with blockstart/end, escape the first block end
    assertEquals(
      Some(("beforeDstillBeforeENstillBefore", "D", "after")),
      test("PPPTbeforeDstillBeforeENstillBeforeNDafter")
    )

    // with blockstart/end, escapeEscape the escape of the first block end
    assertEquals(
      Some(("beforeDstillBeforeTstillBeforeSE", "D", "after")),
      test("PPPTbeforeDstillBeforeTstillBeforeSENDafter")
    )

    // with blockstart, but escape it so it's not really a block.
    assertEquals(
      Some(("ETbeforePPP", "D", "afterNstillafter")),
      test("PPPETbeforePPPDafterNstillafter")
    )

    assertEquals(Some(("beforeD", "D", "after")), test("PPPTbeforeDNDafter"))

    // here it would appear that the additional padding to the right of the block end
    // invalidates the escape scheme as we were only expecting padding to the left of block start
    assertEquals(Some(("Tbefore", "D", "NPPPDafter")), test("PPPTbeforeDNPPPDafter"))

    val cp2 = DFDLRegularExpressions.getEscapeBlockRegExWithPadding(
      bStart,
      bEnd,
      escapeEscape,
      escape,
      padChar,
      delims,
      TextStringJustification.Right
    )
    def test2(x: String) = x match {
      case cp2(before, delim, after, null, null, null) => {
        Some((before, delim, after))
      }
      case cp2(null, null, null, before, delim, after) => {
        Some((before, delim, after))
      }
      case z => {
        // println("no match: " + z);
        None
      }
    }
    // This particular regex lets you escape either the blockstart or blockend, or the delimiter

    // no blockstart/end
    assertEquals(Some(("beforePPP", "D", "after")), test2("PPPbeforePPPDafter"))

    // no blockstart/end, but escape the delimiter
    assertEquals(
      Some(("beforeEDstillBeforePPP", "D", "after")),
      test2("PPPbeforeEDstillBeforePPPDafter")
    )

    // with blockstart/end
    assertEquals(
      Some(("beforeDstillBefore", "D", "after")),
      test2("PPPTbeforeDstillBeforeNDafter")
    )

    // with blockstart/end and esc and escEsc found inside (where they are inactive)
    assertEquals(
      Some(("beforeEDstillBeforeSEDstillBefore", "D", "after")),
      test2("PPPTbeforeEDstillBeforeSEDstillBeforeNDafter")
    )
    // Note: in the above, the SED is ok. No postprocessing. It is escaped in entirety by the T---N pair.

    // with blockstart/end, escape the first block end
    assertEquals(
      Some(("beforeDstillBeforeENstillBefore", "D", "after")),
      test2("PPPTbeforeDstillBeforeENstillBeforeNDafter")
    )

    // with blockstart/end, escapeEscape the escape of the first block end
    assertEquals(
      Some(("beforeDstillBeforeTstillBeforeSE", "D", "after")),
      test2("PPPTbeforeDstillBeforeTstillBeforeSENDafter")
    )

    // with blockstart, but escape it so it's not really a block.
    assertEquals(
      Some(("ETbeforePPP", "D", "afterNstillafter")),
      test2("PPPETbeforePPPDafterNstillafter")
    )

    assertEquals(Some(("beforeD", "D", "after")), test2("PPPTbeforeDNDafter"))

    // here it would appear that the additional padding to the right of the block end
    // invalidates the escape scheme as we were only expecting padding to the left of block start
    assertEquals(Some(("Tbefore", "D", "NPPPDafter")), test2("PPPTbeforeDNPPPDafter"))

    assertEquals(
      Some(("beforeDstillBeforeTstillBeforeSE", "C", "after")),
      test2("PPPTbeforeDstillBeforeTstillBeforeSENCafter")
    )
    assertEquals(
      Some(("beforeDstillBeforeTstillBeforeSE", "F", "after")),
      test2("PPPTbeforeDstillBeforeTstillBeforeSENFafter")
    )
  }

  @Test def testEscapeBlockRegExCenterJustified() = {
    val cp = DFDLRegularExpressions.getEscapeBlockRegExWithPadding(
      bStart,
      bEnd,
      escapeEscape,
      escape,
      padChar,
      delim,
      TextStringJustification.Center
    )
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
    assertEquals(
      Some(("beforeEDstillBefore", "D", "after")),
      test("PPPbeforeEDstillBeforePPPDafter")
    )

    // with blockstart/end
    assertEquals(
      Some(("beforeDstillBefore", "D", "after")),
      test("PPPTbeforeDstillBeforeNPPPDafter")
    )

    // with blockstart/end and esc and escEsc found inside (where they are inactive)
    assertEquals(
      Some(("beforeEDstillBeforeSEDstillBefore", "D", "after")),
      test("PPPTbeforeEDstillBeforeSEDstillBeforeNPPPDafter")
    )
    // Note: in the above, the SED is ok. No postprocessing. It is escaped in entirety by the T---N pair.

    // with blockstart/end, escape the first block end
    assertEquals(
      Some(("beforeDstillBeforeENstillBefore", "D", "after")),
      test("PPPTbeforeDstillBeforeENstillBeforeNPPPDafter")
    )

    // with blockstart/end, escapeEscape the escape of the first block end
    assertEquals(
      Some(("beforeDstillBeforeTstillBeforeSE", "D", "after")),
      test("PPPTbeforeDstillBeforeTstillBeforeSENPPPDafter")
    )

    // with blockstart, but escape it so it's not really a block.
    assertEquals(
      Some(("ETbefore", "D", "afterNstillafter")),
      test("PPPETbeforePPPDafterNstillafter")
    )

    // Multiple Delimiters
    val cp2 = DFDLRegularExpressions.getEscapeBlockRegExWithPadding(
      bStart,
      bEnd,
      escapeEscape,
      escape,
      padChar,
      delims,
      TextStringJustification.Center
    )
    def test2(x: String) = x match {
      case cp2(before, delim, after, null, null, null) => {
        Some((before, delim, after))
      }
      case cp2(null, null, null, before, delim, after) => {
        Some((before, delim, after))
      }
      case z => {
        // println("no match: " + z);
        None
      }
    }
    // This particular regex lets you escape either the blockstart or blockend, or the delimiter

    // no blockstart/end
    assertEquals(Some(("before", "D", "after")), test2("PPPbeforePPPDafter"))

    // no blockstart/end, but escape the delimiter
    assertEquals(
      Some(("beforeEDstillBefore", "D", "after")),
      test2("PPPbeforeEDstillBeforePPPDafter")
    )

    // with blockstart/end
    assertEquals(
      Some(("beforeDstillBefore", "D", "after")),
      test2("PPPTbeforeDstillBeforeNPPPDafter")
    )

    // with blockstart/end and esc and escEsc found inside (where they are inactive)
    assertEquals(
      Some(("beforeEDstillBeforeSEDstillBefore", "D", "after")),
      test2("PPPTbeforeEDstillBeforeSEDstillBeforeNPPPDafter")
    )
    // Note: in the above, the SED is ok. No postprocessing. It is escaped in entirety by the T---N pair.

    // with blockstart/end, escape the first block end
    assertEquals(
      Some(("beforeDstillBeforeENstillBefore", "D", "after")),
      test2("PPPTbeforeDstillBeforeENstillBeforeNPPPDafter")
    )

    // with blockstart/end, escapeEscape the escape of the first block end
    assertEquals(
      Some(("beforeDstillBeforeTstillBeforeSE", "D", "after")),
      test2("PPPTbeforeDstillBeforeTstillBeforeSENPPPDafter")
    )

    // with blockstart, but escape it so it's not really a block.
    assertEquals(
      Some(("ETbefore", "D", "afterNstillafter")),
      test2("PPPETbeforePPPDafterNstillafter")
    )

    assertEquals(
      Some(("beforeDstillBeforeTstillBeforeSE", "C", "after")),
      test2("PPPTbeforeDstillBeforeTstillBeforeSENPPPCafter")
    )
    assertEquals(
      Some(("beforeDstillBeforeTstillBeforeSE", "F", "after")),
      test2("PPPTbeforeDstillBeforeTstillBeforeSENPPPFafter")
    )
  }
}
