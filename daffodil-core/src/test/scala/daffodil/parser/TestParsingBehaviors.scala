package daffodil.parser

import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._
import scala.collection.mutable.Queue
import java.util.regex.Pattern

class TestParsingBehaviors extends JUnit3Suite {

  // RemoveEscapes code
  def removeEscapeCharacter(input: String, eses: Char, es: Char, delimRegex: String): String = {
    // need to know where the delims start/end
    val m = Pattern.compile(delimRegex).matcher(input)
    val qDelims: Queue[(Int, Int)] = Queue.empty[(Int, Int)]
    while (m.find()) {
      qDelims.enqueue((m.start(), m.end()))
    }

    val sb = new StringBuilder // Result of removal
    var isPrevEsEs: Boolean = false
    var isPrevEs: Boolean = false
    var idx: Int = 0

    input.foreach(c => {
      val nextIdx = idx + 1
      val isDelimNext: Boolean = qDelims.toSet.exists(x => x._1 == nextIdx)
      val isEsEsNext: Boolean = if (nextIdx < input.length()) { input.charAt(nextIdx) == eses } else { false }
      val isEsNext: Boolean = if (nextIdx < input.length()) { input.charAt(nextIdx) == es } else { false }

      c match {
        case x if (x == es && !isPrevEsEs && !isDelimNext && !isEsNext && !isPrevEs) => { isPrevEs = false } // Escape only => remove me
        case x if (x == es && !isPrevEsEs && !isDelimNext && isEsNext && !isPrevEs) => { isPrevEs = true } // Escape ~ Escape => remove me
        case x if (x == es && !isPrevEsEs && isDelimNext && !isPrevEs) => { isPrevEs = false } // Escape ~ Delim => remove escape
        case x if (x == es && !isPrevEsEs && !isDelimNext) => {
          // I was not preceded by escapeEscape AND a delimiter does not follow me, add me
          isPrevEs = false
          sb.append(c)
        }
        case x if (x == es && isPrevEsEs) => { // I was escaped by a previous escapeEscape, add me
          isPrevEsEs = false
          isPrevEs = false
          sb.append(c)
        }
        case x if (x == eses && !isPrevEsEs && !isEsEsNext && !isEsNext) => { // I don't escape anything, add me
          isPrevEsEs = false
          isPrevEs = false
          sb.append(c)
        }
        case x if (x == eses && !isPrevEsEs && !isEsEsNext && isEsNext) => { // I escape following es, don't add me
          isPrevEsEs = true
          isPrevEs = false
        }
        case x if (x == eses && !isPrevEsEs && isEsEsNext) => {
          isPrevEsEs = false
          isPrevEs = false
          sb.append(c)
        }
        case _ => {
          isPrevEsEs = false
          isPrevEs = false
          sb.append(c)
        }
      }
      idx += 1
    })
    sb.toString()
  }

  def testEscapeCharacterRemoval_Same = {
    val input0 = "texttexttext"
    val input1 = "text1//text2"
    val input2 = "text1//text2//text3"
    val input3 = "text1////text2"
    val input4 = "//text1"
    val input5 = "text1//"
    val input6 = "//text1//text2"
    val input7 = "text1//text2//"
    val input8 = "text1/,text2"
    val input9 = "text1///,text2"
    val input10 = "/,text1"

    assertEquals("texttexttext", removeEscapeCharacter(input0, '/', '/', ","))
    assertEquals("text1/text2", removeEscapeCharacter(input1, '/', '/', ","))
    assertEquals("text1/text2/text3", removeEscapeCharacter(input2, '/', '/', ","))
    assertEquals("text1//text2", removeEscapeCharacter(input3, '/', '/', ","))
    assertEquals("/text1", removeEscapeCharacter(input4, '/', '/', ","))
    assertEquals("text1/", removeEscapeCharacter(input5, '/', '/', ","))
    assertEquals("/text1/text2", removeEscapeCharacter(input6, '/', '/', ","))
    assertEquals("text1/text2/", removeEscapeCharacter(input7, '/', '/', ","))
    assertEquals("text1,text2", removeEscapeCharacter(input8, '/', '/', ","))
    assertEquals("text1/,text2", removeEscapeCharacter(input9, '/', '/', ","))
    assertEquals(",text1", removeEscapeCharacter(input10, '/', '/', ","))
  }

  def testEscapeCharacterRemoval_Diff = {
    val input0 = "texttexttext"
    val input1 = "text1%/text2"
    val input2 = "text1%/text2%/text3"
    val input3 = "text1%/%/text2"
    val input4 = "%/text1"
    val input5 = "text1%/"
    val input6 = "%/text1%/text2"
    val input7 = "text1%/text2%/"
    val input8 = "text1/,text2"
    val input9 = "text1%//,text2"
    val input10 = "/,text1"
    val input11 = "text1/?text2"
    val input12 = "text1%text2"
    val input13 = "text1%%/text2"
    val input14 = "text1%/%text2"

    assertEquals("texttexttext", removeEscapeCharacter(input0, '%', '/', ","))
    assertEquals("text1/text2", removeEscapeCharacter(input1, '%', '/', ","))
    assertEquals("text1/text2/text3", removeEscapeCharacter(input2, '%', '/', ","))
    assertEquals("text1//text2", removeEscapeCharacter(input3, '%', '/', ","))
    assertEquals("/text1", removeEscapeCharacter(input4, '%', '/', ","))
    assertEquals("text1/", removeEscapeCharacter(input5, '%', '/', ","))
    assertEquals("/text1/text2", removeEscapeCharacter(input6, '%', '/', ","))
    assertEquals("text1/text2/", removeEscapeCharacter(input7, '%', '/', ","))
    assertEquals("text1,text2", removeEscapeCharacter(input8, '%', '/', ","))
    assertEquals("text1/,text2", removeEscapeCharacter(input9, '%', '/', ","))
    assertEquals(",text1", removeEscapeCharacter(input10, '%', '/', ","))
    assertEquals("text1?text2", removeEscapeCharacter(input11, '%', '/', ","))
    assertEquals("text1%text2", removeEscapeCharacter(input12, '%', '/', ","))
    assertEquals("text1%/text2", removeEscapeCharacter(input13, '%', '/', ","))
    assertEquals("text1/%text2", removeEscapeCharacter(input14, '%', '/', ","))
  }

  def testEscapeCharacterRemoval_Diff_MultiCharDelim = {
    val input0 = "text1/septext2"
    val input1 = "text1%//septext2"
    val input2 = "/septext1text2"

    assertEquals("text1septext2", removeEscapeCharacter(input0, '%', '/', "sep"))
    assertEquals("text1/septext2", removeEscapeCharacter(input1, '%', '/', "sep"))
    assertEquals("septext1text2", removeEscapeCharacter(input2, '%', '/', "sep"))
  }

}