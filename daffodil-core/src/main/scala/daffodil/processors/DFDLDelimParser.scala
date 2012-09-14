package delimsearch
import scala.annotation.migration
import scala.collection.mutable.Queue
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Reader
import delimsearch.DelimiterType.DelimiterType
import scala.util.matching.Regex
import java.nio.charset.Charset
import java.util.regex.Pattern

class DelimParseResult {
  var field: String = ""
  var isSuccess: Boolean = false
  var delimiter: String = ""
  var delimiterType: DelimiterType = DelimiterType.Delimiter
  var numBytes: Int = 0

  def apply(pField: String, pIsSuccess: Boolean, pDelimiter: String, pDelimiterType: DelimiterType, pNumBytes: Int) = {
    field = pField // The parsed field
    isSuccess = pIsSuccess // parse success or failure
    delimiter = pDelimiter // delimiter denoting the field
    delimiterType = pDelimiterType // Might be useful to know if the delimiter was a separator or terminator
    numBytes = pNumBytes // Number of bytes consumed to create result
    //TODO: Would it be useful to provide the underlying ParseResult object?
  }

  override def toString(): String = {
    "DelimParseResult - Field: " + field + "\tisSuccess: " + isSuccess + "\tDelimiter: " + delimiter + " DelimiterType: " + delimiterType
  }
}

class DelimParser extends RegexParsers {
  override val skipWhitespace = false

  // Need a parser that will always fail.
  // Essentially a parser to reflect the ability to pass in an empty Separator
  // or empty Terminator Set.
  //
  def parserAlwaysFail[T](expected: String)(name: String) = new Parser[T] {
    def apply(in: Input): ParseResult[T] = {
      Failure(name + " expected to fail.", in)
    }
  }

  // This function performs the "longest match" alternation by recursively combining
  // all of the Parsers present in pArray.
  //
  // Assumes pArray is at least of length 1.
  //
  // pArray: An array of delimiters where each delimiter is represented by a Parser
  // idx: The index of the current delimiter/Parser, defaults to 0
  //
  def combineLongest[T](pArray: Array[Parser[T]], idx: Int = 0): Parser[T] = {
    val len = pArray.length

    if (len == 0) { return parserAlwaysFail("")("empty delimiter list") }

    val p0 = pArray(idx)

    if ((idx + 1) == len) { p0 }
    else {
      (p0) ||| (combineLongest(pArray, idx + 1))
    }
  }

  // Constructs an Array of Parser[String] which holds the Parser representations
  // of the delimList.
  //
  // Constructs an Array of String which holds the Regex representations of the
  // delimList.
  //
  def buildDelims(delimList: Set[String]): (Array[Parser[String]], Array[String]) = {
    var delimsParser: Queue[Parser[String]] = Queue.empty
    var delimsRegex: Queue[String] = Queue.empty
    delimList.foreach(str => {
      val d = new Delimiter()
      d(str)
      delimsParser.enqueue(d.delimRegEx.r)
      delimsRegex.enqueue(d.delimRegEx)
    })
    (delimsParser.toArray, delimsRegex.toArray)
  }

  // Combines the delimiters into a single alternation
  //
  def combineDelimitersRegex(sepsRegex: Array[String], termsRegex: Array[String]): String = {
    val sb = new StringBuilder()
    sepsRegex.foreach(x => {
      sb.append(x)
      sb.append("|")
    })
    termsRegex.foreach(x => {
      sb.append(x)
      sb.append("|")
    })
    val delimRegex = sb.toString().replaceFirst("[\\|]$", "") // trimEnd("|")
    delimRegex
  }

  // Default parseInput method
  // Looks for a field followed by a separator or terminator
  //
  // Assumes postfix, the grammar should handle all prefix, infix, postfix stuff
  //
  def parseInputDefault(field: Parser[String], seps: Parser[String], terms: Parser[String],
    input: Reader[Char], name: String, charset: Charset): DelimParseResult = {
    // TODO: Should we have EOF passed as a separator or terminator?  That way we can error when we do not
    // expect the data to be terminated by EOF?

    // The Parse Statement: field ~ (seps | terms)
    // might be overkill as the field parser seems to find the field no problem.
    //
    // However, the addition of "~ (seps | terms)" guarantees that we will receive a failure
    // if a separator or terminator is not found!
    val EOF: Parser[String] = """\z""".r
    //val res = this.parse(this.log(field ~ (seps | terms))("DelimParser." + name), input)
    //val delims: Parser[String] = (seps | terms) <~ opt(EOF)
    //val entry = phrase((field ~ (seps | terms)))
    val delims: Parser[String] = (seps | terms)
    val entry = (field ~ ( delims | (delims <~ opt(EOF)))) | (field ~ EOF)
    val res = this.parse(this.log(entry)("DelimParser." + name), input)
    res

    var fieldResult = ""
    var delimiterResult = ""
    var isSuccess: Boolean = false
    var delimiterType = DelimiterType.Delimiter
    var fieldResultBytes: Int = 0

    if (!res.isEmpty) {
      fieldResult = res.get._1
      delimiterResult = res.get._2
      isSuccess = true
      fieldResultBytes = fieldResult.getBytes(charset).length
      val result = this.parse(seps, delimiterResult)
      if (result.isEmpty) { delimiterType = DelimiterType.Terminator }
      else { delimiterType = DelimiterType.Separator }
    }

    val result: DelimParseResult = new DelimParseResult
    result(fieldResult, isSuccess, delimiterResult, delimiterType, fieldResultBytes)
    result
  }

  def failedResult: DelimParseResult = {
    val result: DelimParseResult = new DelimParseResult
    result("", false, "", DelimiterType.Delimiter, 0)
    result
  }

  def parseInput(separators: Set[String], terminators: Set[String], input: Reader[Char], charset: Charset): DelimParseResult = {
    if (terminators.size == 0 && separators.size == 0) { return failedResult }

    val (sepsParser, sepsRegex) = this.buildDelims(separators)
    val (termsParser, termsRegex) = this.buildDelims(terminators)
    val pSeps: Parser[String] = this.combineLongest(sepsParser)
    val pTerms: Parser[String] = this.combineLongest(termsParser)

    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)

    // A word is anything but a delimiter or EOF
    val wordRegex: String = """((.*?)(?=(%s)|\z))"""
    //  val wordRegex: String = """(.*?)(?=(%s|\z))"""
    val word: Parser[String] = String.format(wordRegex, delimsRegex).r

    val result = parseInputDefault(word, pSeps, pTerms, input, "default", charset)
    result
  }

  def parseInputEscapeBlock(separators: Set[String], terminators: Set[String],
    input: Reader[Char], escapeBlockStart: String, escapeBlockEnd: String,
    escapeEscapeCharacter: String = "", charset: Charset): DelimParseResult = {

    if (terminators.size == 0 && separators.size == 0) { return failedResult }
    if (escapeBlockStart.length() == 0 || escapeBlockEnd.length() == 0) { return failedResult }

    // PARAMETERS:	(1) EscapeBlockStart (2) EscapeBlockEnd (3) Delimiters
    val wordRegexUnescaped: String = """^(%1$s)(.*?)(%2$s)(?=(%3$s)|\z)"""
      
    // PARAMETERS:	(1) EscapeEscapeCharacter (2) EscapeBlockStart 
    // 				(3) EscapeEscapeCharacter (4) EscapeBlockEnd (5) Delimiters
    val wordRegexEscaped: String = """^((?<!%1$s)%2$s)(.*?)((?<!%3$s)%4$s)(?=(%5$s)|\z)"""
      
    val (sepsParser, sepsRegex) = this.buildDelims(separators)
    val (termsParser, termsRegex) = this.buildDelims(terminators)
    val pSeps: Parser[String] = this.combineLongest(sepsParser)
    val pTerms: Parser[String] = this.combineLongest(termsParser)

    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)

    val escapeBlockStartRegex = convertDFDLLiteralToRegex(escapeBlockStart)
    val escapeBlockEndRegex = convertDFDLLiteralToRegex(escapeBlockEnd)

    val word: Parser[String] = escapeEscapeCharacter match {
      case "" => String.format(wordRegexUnescaped, escapeBlockStartRegex, escapeBlockEndRegex, delimsRegex).r
      case _ => {
        val escapeEscapeCharacterRegex = convertDFDLLiteralToRegex(escapeEscapeCharacter)
        String.format(wordRegexEscaped, escapeEscapeCharacterRegex, escapeBlockStartRegex, 
          escapeEscapeCharacterRegex, escapeBlockEndRegex, delimsRegex).r
      }
    }
    
    val result = parseInputDefault(word, pSeps, pTerms, input, "escapeBlock", charset)
    
    // If failed, try regular parse.
    if (!result.isSuccess){ return parseInput(separators, terminators, input, charset)}
    
    val newField = removeEscapesBlocks(result.field, escapeEscapeCharacter, escapeBlockStartRegex, escapeBlockEndRegex)
    
    result.field = newField
    
    result
  }

  def parseInputEscapeCharacter(separators: Set[String], terminators: Set[String],
    input: Reader[Char], escapeCharacter: String, escapeEscapeCharacter: String = "", charset: Charset): DelimParseResult = {

    if (terminators.size == 0 && separators.size == 0) { return failedResult }
    if (escapeCharacter.length() == 0) { return failedResult }

    // PARAMETERS: (1) EscapeEscapeCharacter (2) EscapeCharacter (3) Delimiters
    val wordRegexEscaped: String = """(.*?)(?=(?<!(?<!%1$s)%2$s)%3$s|\z)"""
      
      // PARAMETERS: (1) EscapeCharacter (2) Delimiters
    val wordRegexUnescaped: String = """(.*?)(?=(?<!%1$s)%2$s|\z)"""

    val (sepsParser, sepsRegex) = this.buildDelims(separators)
    val (termsParser, termsRegex) = this.buildDelims(terminators)
    val pSeps: Parser[String] = this.combineLongest(sepsParser)
    val pTerms: Parser[String] = this.combineLongest(termsParser)

    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)

    val escapeEscapeCharacterRegex = convertDFDLLiteralToRegex(escapeEscapeCharacter)
    val escapeCharacterRegex = convertDFDLLiteralToRegex(escapeCharacter)
    
    val word: Parser[String] = escapeEscapeCharacter match {
      case "" => String.format(wordRegexUnescaped, escapeCharacterRegex, delimsRegex).r
      case _ => {
        val escapeEscapeCharacterRegex = convertDFDLLiteralToRegex(escapeEscapeCharacter)
        String.format(wordRegexEscaped, escapeEscapeCharacterRegex, escapeCharacterRegex, delimsRegex).r
      }
    }

    val result = parseInputDefault(word, pSeps, pTerms, input, "escapeCharacter", charset)
    
    val newField = removeEscapeCharacters(result.field, escapeEscapeCharacter, escapeCharacter.charAt(0), delimsRegex)
    
    result.field = newField
    
    result
  }

  def convertDFDLLiteralToRegex(dfdlLiteral: String): String = {
    var sb: StringBuilder = new StringBuilder("(")
    dfdlLiteral foreach {
      char =>
        {
          char match {
            case '[' => sb.append("\\[")
            case '\\' => sb.append("\\\\")
            case '^' => sb.append("\\^")
            case '$' => sb.append("\\$")
            case '.' => sb.append("\\.")
            case '|' => sb.append("\\|")
            case '?' => sb.append("\\?")
            case '*' => sb.append("\\*")
            case '+' => sb.append("\\+")
            case '(' => sb.append("\\(")
            case ')' => sb.append("\\)")
            case '{' => sb.append("\\{")
            case '}' => sb.append("\\}")
            case x => sb.append(x)
          }
        }
    }
    sb.append(")")
    sb.toString()
  }
  
  // Here eses must be of type String as it's possible for it to be empty.
  // Here es can be Char as es must be specified if the escapeSchemeKind is escapeCharacter
  //
  def removeEscapeCharacters(input: String, eses: String, es: Char, delimRegex: String): String = {
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
      val isEsEsNext: Boolean = if (nextIdx < input.length() && eses.length() == 1) { input.charAt(nextIdx) == eses.charAt(0) } else { false }
      val isEsNext: Boolean = if (nextIdx < input.length()) { input.charAt(nextIdx) == es } else { false }
      val isEsEs: Boolean = if (eses.length == 1 && eses.charAt(0) == c){ true} else {false}

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
        case x if (isEsEs && !isPrevEsEs && !isEsEsNext && !isEsNext) => { // I don't escape anything, add me
          isPrevEsEs = false
          isPrevEs = false
          sb.append(c)
        }
        case x if (isEsEs && !isPrevEsEs && !isEsEsNext && isEsNext) => { // I escape following es, don't add me
          isPrevEsEs = true
          isPrevEs = false
        }
        case x if (isEsEs && !isPrevEsEs && isEsEsNext) => {
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
  
  // eses can be an empty String, so must be of type String here
  //
  def removeEscapesBlocks(input: String, eses: String, startBlockRegex: String, endBlockRegex: String): String = {

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
      if (prevIdx < 0 || eses.length() == 0) { return false }
      else if (input.charAt(prevIdx) == eses.charAt(0)) { return true }
      false
    }

    input.foreach(c => {
      val nextIdx = idx + 1
      val isNextBlockEnd: Boolean = qBlockEnds.exists(_._1 == nextIdx)
      val isBlockStart: Boolean = qBlockStarts.exists(x => idx >= x._1 && idx < x._2)
      val isBlockEnd: Boolean = qBlockEnds.exists(x => idx >= x._1 && idx < x._2)
      val isValidBlockStart: Boolean = qBlockStarts.exists(x => x._1 == 0 && idx >= x._1 && idx < x._2)
      val isValidBlockEnd: Boolean = qBlockEnds.exists(x => x._2 == input.length() && idx >= x._1 && idx < x._2)
      val isEsEs: Boolean = if (eses.length() == 1 && c == eses.charAt(0)){ true } else { false }

      val isEsEsBeforeThisBlock: Boolean = {
        var result: Boolean = false
        if (isBlockStart) { result = isEsEsBeforeBlock(input, qBlockStarts, idx) }
        else if (isBlockEnd) { result = isEsEsBeforeBlock(input, qBlockEnds, idx) }
        result
      }

      c match {
        case x if (isEsEs && isNextBlockEnd && hasValidBlockStart) => { isPrevEsEs = true }
        case x if (isEsEs && !isNextBlockEnd) => {
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

}
