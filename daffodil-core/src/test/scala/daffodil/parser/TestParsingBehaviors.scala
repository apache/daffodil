package daffodil.parser

import org.scalatest.junit.JUnit3Suite
import junit.framework.Assert._
import scala.collection.mutable.Queue
import java.util.regex.Pattern
import java.io.FileInputStream
import scala.util.parsing.input.CharSequenceReader
import daffodil.util.Misc
import javax.xml.transform.stream.StreamSource
import java.io.File

class TestParsingBehaviors extends JUnit3Suite {

  val testFileDir = "/test/"

  val rsrcAB007 = Misc.getRequiredResource(testFileDir + "AB007.in")

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
    // escape and escapeEscape are the same
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
    // different escape and escapeEscape characters
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

  def removeEscapesBlocks(input: String, eses: Char, startBlockRegex: String, endBlockRegex: String): String = {

    // need to know where the delims start/end
    val mBlockEnd = Pattern.compile(endBlockRegex).matcher(input)
    val qBlockEnds: Queue[(Int, Int)] = Queue.empty[(Int, Int)]
    while (mBlockEnd.find()) {
      qBlockEnds.enqueue((mBlockEnd.start(), mBlockEnd.end()))
    }

    val mBlockStart = Pattern.compile(startBlockRegex).matcher(input)
    val qBlockStarts: Queue[(Int, Int)] = Queue.empty[(Int, Int)]
    while (mBlockStart.find()) {
      qBlockStarts.enqueue((mBlockStart.start(), mBlockStart.end()))
    }

    val sb = new StringBuilder // Result of removal

    var isPrevEsEs: Boolean = false
    var idx: Int = 0
    var hasValidBlockStart: Boolean = false

    def isEsEsBeforeBlock(input: String, q: Queue[(Int, Int)], idx: Int): Boolean = {
      val blockIdx = q.filter(x => idx >= x._1 && idx < x._2)(0)._1
      val prevIdx = blockIdx - 1
      if (prevIdx < 0) { return false }
      if (input.charAt(prevIdx) == eses) { return true }
      false
    }

    input.foreach(c => {
      val nextIdx = idx + 1
      val isNextBlockEnd: Boolean = qBlockEnds.exists(_._1 == nextIdx)
      val isBlockStart: Boolean = qBlockStarts.exists(x => idx >= x._1 && idx < x._2)
      val isBlockEnd: Boolean = qBlockEnds.exists(x => idx >= x._1 && idx < x._2)
      val isValidBlockStart: Boolean = qBlockStarts.exists(x => x._1 == 0 && idx >= x._1 && idx < x._2)
      val isValidBlockEnd: Boolean = qBlockEnds.exists(x => x._2 == input.length() && idx >= x._1 && idx < x._2)
      
      val isEsEsBeforeThisBlock: Boolean = {
        var result: Boolean = false
        if (isBlockStart) { result = isEsEsBeforeBlock(input, qBlockStarts, idx) }
        else if (isBlockEnd) { result = isEsEsBeforeBlock(input, qBlockEnds, idx) }
        result
      }
      
      c match {
        case x if (x == eses && isNextBlockEnd && hasValidBlockStart) => { isPrevEsEs = true }
        case x if (x == eses && !isNextBlockEnd) => {
          isPrevEsEs = false
          sb.append(c)
        }
        case x if (isBlockStart && isBlockEnd && !isValidBlockStart && hasValidBlockStart && !isEsEsBeforeThisBlock && isValidBlockEnd) => {
          // BlockStart and BlockEnd are the same
          isPrevEsEs = false
        }
        case x if (isBlockStart && isBlockEnd && !isValidBlockStart && isEsEsBeforeThisBlock) => {
          isPrevEsEs = false
          sb.append(c)
        }
        case x if (isBlockStart && isBlockEnd && isValidBlockStart) => {
          isPrevEsEs = false
          hasValidBlockStart = true
        }
        case x if (isBlockStart && isValidBlockStart) => {
          isPrevEsEs = false
          hasValidBlockStart = true
        }
        case x if (isBlockStart && !isValidBlockStart) => {
          isPrevEsEs = false
          sb.append(c)
        }
        case x if (isBlockEnd && isEsEsBeforeThisBlock) => { // invalid BlockEnd
          isPrevEsEs = false
          sb.append(c)
        }
        case x if (isBlockEnd && !isEsEsBeforeThisBlock && hasValidBlockStart) => {}
        case _ => {
          isPrevEsEs = false
          sb.append(c)
        }
      }
      idx += 1
    })

    sb.toString()
  }


  def testEscapeBlockRemoval_Diff = {
    // Different Start/End characters
    val qInputOutput = Queue.empty[(String, String)]
    val qOutput = Queue.empty[String]

    qInputOutput.enqueue("texttext" -> "texttext")
    qInputOutput.enqueue("[[texttext]" -> "[texttext")
    qInputOutput.enqueue("]texttext" -> "]texttext")
    qInputOutput.enqueue("text[text" -> "text[text")
    qInputOutput.enqueue("text]text" -> "text]text")
    qInputOutput.enqueue("texttext]" -> "texttext]")
    qInputOutput.enqueue("[[[texttext]" -> "[[texttext")
    qInputOutput.enqueue("texttext]]" -> "texttext]]")
    qInputOutput.enqueue("text[[text" -> "text[[text")
    qInputOutput.enqueue("text]]text" -> "text]]text")
    qInputOutput.enqueue("[[texttext%]]" -> "[texttext]")
    qInputOutput.enqueue("[[text%]text]" -> "[text]text")
    qInputOutput.enqueue("text[text]" -> "text[text]")
    qInputOutput.enqueue("[[text[text%]]" -> "[text[text]")
    qInputOutput.enqueue("[[text%]text%]]" -> "[text]text]")
    qInputOutput.enqueue("[[[texttext%]]" -> "[[texttext]")
    qInputOutput.enqueue("[[texttext%]%]]" -> "[texttext]]")
    qInputOutput.enqueue("[[[texttext%]%]]" -> "[[texttext]]")
    qInputOutput.enqueue("text%text" -> "text%text")
    qInputOutput.enqueue("text%%text" -> "text%%text")
    qInputOutput.enqueue("text%[text" -> "text%[text")
    qInputOutput.enqueue("text%]text" -> "text%]text")
    qInputOutput.enqueue("%[texttext" -> "%[texttext")
    qInputOutput.enqueue("texttext%]" -> "texttext%]")
    qInputOutput.enqueue("%[texttext%]" -> "%[texttext%]")
    qInputOutput.enqueue("[[text%text%]]" -> "[text%text]")
    qInputOutput.enqueue("[[text%%]text%]]" -> "[text%]text]")
    qInputOutput.enqueue("[text;text]" -> "text;text")
    qInputOutput.enqueue("[text%;text]" -> "text%;text")
    qInputOutput.enqueue("[[text;text%]]" -> "[text;text]")
    qInputOutput.enqueue("[text?text]" -> "text?text")

    var idx = 1
    qInputOutput.foreach(x => {
      //println("trying... expect: " + x._2 + " for input: " + x._1)
      val result = removeEscapesBlocks(x._1, '%', """\[""", """]""")
      //println("...got: " + result)
      assertEquals(x._2, result)
      //println("test " + idx + " succeeded")
      idx += 1
    })
  }
  
  def testEscapeBlockRemoval_Same = {
    // Same Start/End characters
    val qInputOutput = Queue.empty[(String, String)]
    val qOutput = Queue.empty[String]

    qInputOutput.enqueue("texttext" -> "texttext")
    qInputOutput.enqueue("'%'texttext'" -> "'texttext")
    qInputOutput.enqueue("text'text" -> "text'text")
    qInputOutput.enqueue("texttext'" -> "texttext'")
    qInputOutput.enqueue("'%'%'texttext'" -> "''texttext")
    qInputOutput.enqueue("texttext''" -> "texttext''")
    qInputOutput.enqueue("text''text" -> "text''text")
    qInputOutput.enqueue("'%'texttext%''" -> "'texttext'")
    
    qInputOutput.enqueue("'%'text%'text'" -> "'text'text")
    qInputOutput.enqueue("text'text'" -> "text'text'")
    qInputOutput.enqueue("'%'text%'text%''" -> "'text'text'")
    qInputOutput.enqueue("'%'%'texttext%''" -> "''texttext'")
    qInputOutput.enqueue("'%'texttext%'%''" -> "'texttext''")
    qInputOutput.enqueue("'%'%'texttext%'%''" -> "''texttext''")
    
    qInputOutput.enqueue("text%text" -> "text%text")
    qInputOutput.enqueue("text%%text" -> "text%%text")
    qInputOutput.enqueue("text%'text" -> "text%'text")
    qInputOutput.enqueue("%'texttext" -> "%'texttext")
    
    qInputOutput.enqueue("texttext%'" -> "texttext%'")
    qInputOutput.enqueue("'%'texttext%%''" -> "'texttext%'")
    qInputOutput.enqueue("%'texttext%'" -> "%'texttext%'")
    qInputOutput.enqueue("'%'text%text%''" -> "'text%text'")
    qInputOutput.enqueue("'%'text%%'text%''" -> "'text%'text'")
    
    qInputOutput.enqueue("'text;text'" -> "text;text")
    qInputOutput.enqueue("'text%;text'" -> "text%;text")
    qInputOutput.enqueue("'%'text;text%''" -> "'text;text'")
    qInputOutput.enqueue("'text?text'" -> "text?text")

    var idx = 1
    qInputOutput.foreach(x => {
      //println("trying... expect: " + x._2 + " for input: " + x._1)
      val result = removeEscapesBlocks(x._1, '%', """'""", """'""")
      //println("...got: " + result)
      assertEquals(x._2, result)
      //println("test " + idx + " succeeded")
      idx += 1
    })
  }

  def testParseSingleFieldFromAB007 = {
    ////println(System.getProperty("user.dir"))
    //val channel = new FileInputStream(testFileDir + "AB007.in").getChannel()
   val channel = new FileInputStream(new File(rsrcAB007.getPath().substring(1))).getChannel()

    val byteR = new delimsearch.DFDLByteReader(channel)

    val r = byteR.charReader("UTF-8")

    val d = new delimsearch.DelimParser

    val separators = Set[String](",")

    val terminators = Set[String]("%NL;")

    val res = d.parseInput(separators, terminators, r)

    assertEquals("1", res.field)
    assertEquals(",", res.delimiter)
  }

  def testParsingEscapeSchemeBlockAtStart = {
    val r = new CharSequenceReader("/*hidden/*:text*/:def:ghi") // Input 1
    val d = new delimsearch.DelimParser
    val separators = Set[String](":")
    val terminators = Set[String]()

    val escapeBlockStart = "/*"
    val escapeBlockEnd = "*/"
    val escapeEscapeCharacter = ""

    val res = d.parseInputEscapeBlock(separators, terminators, r,
      escapeBlockStart, escapeBlockEnd, escapeEscapeCharacter)

    assertTrue(res.isSuccess)
    assertEquals("/*hidden/*:text*/", res.field)
    assertEquals(":", res.delimiter)
  }

  def testParsingEscapedEscapeSchemeBlockAtStart = {
    val r = new CharSequenceReader("//*hidden/*:text*/:def:ghi") // Input 1
    val d = new delimsearch.DelimParser
    val separators = Set[String](":")
    val terminators = Set[String]()

    val escapeBlockStart = "/*"
    val escapeBlockEnd = "*/"
    val escapeEscapeCharacter = "/"

    val res = d.parseInputEscapeBlock(separators, terminators, r,
      escapeBlockStart, escapeBlockEnd, escapeEscapeCharacter)

    assertTrue(res.isSuccess)
    assertEquals("//*hidden/*", res.field)
    assertEquals(":", res.delimiter)
  }

  def testParsingEscapeSchemeBlockInMiddle = {

    val r = new CharSequenceReader("abc/*hidden/*:text*/:def:ghi") // Input 1

    val d = new delimsearch.DelimParser

    val separators = Set[String](":")

    val terminators = Set[String]()

    val escapeBlockStart = "/*"
    val escapeBlockEnd = "*/"
    val escapeEscapeCharacter = ""

    val res = d.parseInputEscapeBlock(separators, terminators, r,
      escapeBlockStart, escapeBlockEnd, escapeEscapeCharacter)

    assertTrue(res.isSuccess)
    assertEquals("abc/*hidden/*", res.field)
    assertEquals(":", res.delimiter)
  }

}