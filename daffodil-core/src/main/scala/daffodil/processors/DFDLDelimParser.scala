package delimsearch
import scala.annotation.migration
import scala.collection.mutable.Queue
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Reader
import delimsearch.DelimiterType.DelimiterType
import delimsearch.DelimiterLocation.DelimiterLocation
import scala.util.matching.Regex
import java.nio.charset.Charset
import java.util.regex.Pattern
import daffodil.util.Logging
import daffodil.util.LogLevel
import daffodil.util.Debug

class DelimParseResult {
  var field: String = ""
  var isSuccess: Boolean = false
  var delimiter: String = ""
  var delimiterType: DelimiterType = DelimiterType.Delimiter
  var delimiterLoc: DelimiterLocation = DelimiterLocation.Local
  var numBytes: Int = 0
  var numCharsRead: Int = 0 // Number of characters read

  def apply(pField: String, pIsSuccess: Boolean, pDelimiter: String, pDelimiterType: DelimiterType, pNumBytes: Int, pDelimiterLoc: DelimiterLocation = DelimiterLocation.Local) = {
    field = pField // The parsed field
    isSuccess = pIsSuccess // parse success or failure
    delimiter = pDelimiter // delimiter denoting the field
    delimiterType = pDelimiterType // Might be useful to know if the delimiter was a separator or terminator
    numBytes = pNumBytes // Number of bytes consumed to create result
    delimiterLoc = pDelimiterLoc
    //TODO: Would it be useful to provide the underlying ParseResult object?
  }

  override def toString(): String = {
    "DelimParseResult - Field: " + field + "\tisSuccess: " + isSuccess + "\tDelimiter: " + delimiter + " DelimiterType: " + delimiterType + " DelimiterLoc: " + delimiterLoc
  }
}

class DelimParser extends RegexParsers with Logging {
  override val skipWhitespace = false
  
  /** A helper method that turns a `Parser` into one that will
   *  print debugging information to stdout before and after
   *  being applied.
   */
   override def log[T](p: => Parser[T])(name: String): Parser[T] = Parser{ in =>
    //println("trying "+ name +" at "+ in)
    log(Debug("trying %s at %s", name, in))
    val r = p(in)
    //println(name +" --> "+ r)
    log(Debug("%s --> %s", name, r))
    r
  }

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

    // We probably always want delims ordered:
    // Multi-char delims containing WSP+/*, WSP+, WSP*, multi-char delims, WSP, single-char delims

    sortDelims(delimList).toList.foreach(str => {
      val d = new Delimiter()
      d(str)
      delimsParser.enqueue(d.delimRegExParseDelim.r) // The regex representing the actual delimiter
      delimsRegex.enqueue(d.delimRegExParseDelim) // The regex representing the actual delimiter
    })
    (delimsParser.toArray, delimsRegex.toArray)
  }

  def sortDelims(delimList: Set[String]): Seq[String] = {
    val wspStarByItself = delimList.filter(s => s == "%WSP*;")
    val wspPlusByItself = delimList.filter(s => s == "%WSP+;")

    val filteredDelimList = (delimList -- (wspStarByItself union wspPlusByItself))

    val multiCharUnboundedLength = filteredDelimList.filter(s => (s.contains("%WSP*;") || s.contains("%WSP+;")))
    val multiChar = (filteredDelimList -- multiCharUnboundedLength).filter(s => s.length() > 1)
    val singleChar = filteredDelimList -- (multiChar union multiCharUnboundedLength)

    val sortedUnbounded = multiCharUnboundedLength.toArray[String]
    val sortedMultiChar = multiChar.toArray[String]

    scala.util.Sorting.quickSort(sortedUnbounded)
    scala.util.Sorting.quickSort(sortedMultiChar)

    val orderedResultSeq: Seq[String] = sortedUnbounded.reverse.toSeq ++ wspPlusByItself ++ wspStarByItself ++ sortedMultiChar.reverse.toSeq ++ singleChar
    orderedResultSeq
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

  def parseInputPatterned(pattern: String, input: Reader[Char], charset: Charset): DelimParseResult = { 
    withLoggingLevel(LogLevel.Info) {
    val EOF: Parser[String] = """\z""".r

    val thePattern: Parser[String] = ("(?s)" + pattern).r
    val entry = thePattern <~ opt(EOF)

//    System.err.println(input.source)
//    System.err.println(input.source.toString().getBytes().map(b => b.toByte.toHexString).toList)
    // FOR DEBUGGING might want this logging version
    val res = this.parse(this.log(entry)("DelimParser.parseInputPatterned"), input)
    //val res = this.parse(entry, input)

    var fieldResult = ""
    var delimiterResult = ""
    var isSuccess: Boolean = false
    var delimiterType = DelimiterType.Delimiter
    var fieldResultBytes: Int = 0

    if (!res.isEmpty) {
      fieldResult = res.get
      isSuccess = true
      fieldResultBytes = fieldResult.getBytes(charset).length
    }

    val result: DelimParseResult = new DelimParseResult
    result(fieldResult, isSuccess, delimiterResult, delimiterType, fieldResultBytes)
    result.numCharsRead = fieldResult.length()
    result
  }
  }

  def parseInputNCharacters(nChars: Long, input: Reader[Char], charset: Charset): DelimParseResult = { withLoggingLevel(LogLevel.Info) {
    val EOF: Parser[String] = """\z""".r
    val anything: Parser[String] = """.*""".r
    val firstNChars: Parser[String] = String.format("""(?s).{%s}""", nChars.toString()).r

    val entry = firstNChars //<~ anything // Technically shouldn't need to add anything, we only want the first nChars

    // For debug can use this logging parser instead.
    val res = this.parse(this.log(entry)("DelimParser.parseInputNCharacters"), input)
    //val res = this.parse(entry, input)

    var fieldResult = ""
    var delimiterResult = ""
    var isSuccess: Boolean = false
    var delimiterType = DelimiterType.Delimiter
    var fieldResultBytes: Int = 0

    if (!res.isEmpty) {
      fieldResult = res.get
      isSuccess = true
      fieldResultBytes = fieldResult.getBytes(charset).length
    }

    val result: DelimParseResult = new DelimParseResult
    result(fieldResult, isSuccess, delimiterResult, delimiterType, fieldResultBytes)
    result.numCharsRead = fieldResult.length()
    result
  }
  }

  // Default parseInput method
  // Looks for a field followed by a separator or terminator
  //
  // Assumes postfix, the grammar should handle all prefix, infix, postfix stuff
  //
  def parseInputDefault(field: Parser[String], seps: Parser[String], terms: Parser[String],
    input: Reader[Char], name: String, charset: Charset): DelimParseResult = { withLoggingLevel(LogLevel.Info) {
    // The Parse Statement: field ~ (seps | terms)
    // might be overkill as the field parser seems to find the field no problem.
    //
    // However, the addition of "~ (seps | terms)" guarantees that we will receive a failure
    // if a separator or terminator is not found!
    val EOF: Parser[String] = """\z""".r

    val delims: Parser[String] = (seps | terms)
    val entry = (field ~ (delims | (delims <~ opt(EOF)))) | (field ~ EOF)

    // FOR DEBUG: might want to use this logging variant.
    val res = this.parse(this.log(entry)("DelimParser." + name), input)
    //val res = this.parse(entry, input)

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
    result.numCharsRead = fieldResult.length()
    result
    }
  }

  /**
   * This is a canned failedResult so that we don't have to continually
   * set the constructor.
   */
  def failedResult: DelimParseResult = {
    val result: DelimParseResult = new DelimParseResult
    result("", false, "", DelimiterType.Delimiter, 0)
    result
  }
  
/***
 * localDelims - delimiters local to the component in question
 * remoteDelims - delimiters of an enclosing container of this component
 * 
 * Assumes that remoteDelims does not contain any String found in localDelims
 * 
 * The call to buildDelims sorts the delimiters by length or possible length.
 */
  def parseInputDelimiter(localDelims: Set[String], remoteDelims: Set[String], 
      input: Reader[Char], charset: Charset): DelimParseResult = { withLoggingLevel(LogLevel.Info) {
    val (localDelimsParser, localDelimsRegex) = this.buildDelims(localDelims)
    val combinedLocalDelimsParser = this.combineLongest(localDelimsParser)
    
    val (remoteDelimsParser, remoteDelimsRegex) = this.buildDelims(remoteDelims)
    
    val combinedDelims = remoteDelimsParser ++ localDelimsParser
    val combinedDelimsParser = this.combineLongest(combinedDelims)
    
    val EOF: Parser[String] = """\z""".r

    //val entry = combinedLocalDelimsParser <~ opt(EOF)
    val entry = combinedDelimsParser <~ opt(EOF) // Should yield longest match of all the delimiters

    // FOR DEBUG: might want this logging variant
    val res = this.parse(this.log(entry)("DelimParser.parseInputDelimiter.allDelims"), input)
    //val res = this.parse(entry, input)

    var fieldResult = ""
    var delimiterResult = ""
    var isSuccess: Boolean = false
    var delimiterType = DelimiterType.Delimiter
    var delimiterLoc = DelimiterLocation.Local
    var fieldResultBytes: Int = 0

    if (!res.isEmpty) {
      // We have a result but was it a remote or local match?
      delimiterResult = res.get
      isSuccess = true
      
      // We need the regex to match exactly the whole delimiterResult
      
      // Here localDelimsRegex should have already been sorted by the buildDelims call
      // we simply need to tell the regex that it has to match the full delimiterResult String.
      // If it doesn't match, then that means the match had to be a remote delimiter.
      val newLocalDelimsRegex = "(?s)^(" + combineDelimitersRegex(localDelimsRegex, Array.empty[String]) + ")$"
      val newLocalDelimsParser: Parser[String] = newLocalDelimsRegex.r
      
      val result = this.parseAll(this.log(newLocalDelimsParser)("DelimParser.parseInputDelimiter.isLocal"), delimiterResult)
      //val result = this.parseAll(newLocalDelimsParser, delimiterResult)
      if (result.isEmpty){ delimiterLoc = DelimiterLocation.Remote}
    }
    val result: DelimParseResult = new DelimParseResult
    result(fieldResult, isSuccess, delimiterResult, delimiterType, fieldResultBytes, delimiterLoc)
    result.numCharsRead = delimiterResult.length()
    result
      }
  }

  def parseInput(separators: Set[String], terminators: Set[String], input: Reader[Char], charset: Charset): DelimParseResult = {
    val (sepsParser, sepsRegex) = this.buildDelims(separators)
    val (termsParser, termsRegex) = this.buildDelims(terminators)
    val pSeps: Parser[String] = this.combineLongest(sepsParser)
    val pTerms: Parser[String] = this.combineLongest(termsParser)

    // A word is anything but a delimiter or EOF
    val wordRegex: String = """(?s)(.*?)(?=(%s)|\z)"""

    val word: Parser[String] = {
      // No Delimiters? Give me everything.
      if (separators.size == 0 && terminators.size == 0) {
        //        System.err.println("no terminating markup")
        """(?s).*""".r
      } else {
        val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)
        val finalRegex = String.format(wordRegex, delimsRegex).r
        //        System.err.println(finalRegex)
        finalRegex
      }
    }

    val result = parseInputDefault(word, pSeps, pTerms, input, "default", charset)
    result
  }

  def parseInputEscapeBlock(separators: Set[String], terminators: Set[String],
    input: Reader[Char], escapeBlockStart: String, escapeBlockEnd: String,
    escapeEscapeCharacter: String = "", charset: Charset): DelimParseResult = {

    if (escapeBlockStart.length() == 0 || escapeBlockEnd.length() == 0) { return failedResult }

    // PARAMETERS:	(1) EscapeBlockStart (2) EscapeBlockEnd (3) Delimiters
    val wordRegexUnescaped: String = """(?s)^(%1$s)(?s)(.*?)(%2$s)(?=(%3$s)|\z)"""

    // PARAMETERS:	(1) EscapeEscapeCharacter (2) EscapeBlockStart 
    // 				(3) EscapeEscapeCharacter (4) EscapeBlockEnd (5) Delimiters
    val wordRegexEscaped: String = """(?s)^((?<!%1$s)%2$s)(?s)(.*?)((?<!%3$s)%4$s)(?=(%5$s)|\z)"""

    val (sepsParser, sepsRegex) = this.buildDelims(separators)
    val (termsParser, termsRegex) = this.buildDelims(terminators)
    val pSeps: Parser[String] = this.combineLongest(sepsParser)
    val pTerms: Parser[String] = this.combineLongest(termsParser)

    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)

    val escapeBlockStartRegex = convertDFDLLiteralToRegex(escapeBlockStart)
    val escapeBlockEndRegex = convertDFDLLiteralToRegex(escapeBlockEnd)

    val word: Parser[String] = escapeEscapeCharacter match {
      case "" => {
        if (separators.size == 0 && terminators.size == 0) {
          """(?s).*""".r
        } else { String.format(wordRegexUnescaped, escapeBlockStartRegex, escapeBlockEndRegex, delimsRegex).r }
      }
      case _ => {
        val escapeEscapeCharacterRegex = convertDFDLLiteralToRegex(escapeEscapeCharacter)
        if (separators.size == 0 && terminators.size == 0) {
          """(?s).*""".r
        } else { String.format(wordRegexEscaped, escapeEscapeCharacterRegex, escapeBlockStartRegex, escapeEscapeCharacterRegex, escapeBlockEndRegex, delimsRegex).r }
      }
    }

    val result = parseInputDefault(word, pSeps, pTerms, input, "escapeBlock", charset)

    // If failed, try regular parse.
    if (!result.isSuccess) { return parseInput(separators, terminators, input, charset) }

    val newField = removeEscapesBlocks(result.field, escapeEscapeCharacter, escapeBlockStartRegex, escapeBlockEndRegex)
    result.numCharsRead = result.field.length()
    result.field = newField

    result
  }

  def parseInputEscapeCharacter(separators: Set[String], terminators: Set[String],
    input: Reader[Char], escapeCharacter: String, escapeEscapeCharacter: String = "", charset: Charset): DelimParseResult = {

    //if (terminators.size == 0 && separators.size == 0) { return failedResult }
    if (escapeCharacter.length() == 0) { return failedResult }

    // PARAMETERS: (1) EscapeEscapeCharacter (2) EscapeCharacter (3) Delimiters
    val wordRegexEscaped: String = """(?s)(.*?)(?=(?<!(?<!%1$s)%2$s)%3$s|\z)"""

    // PARAMETERS: (1) EscapeCharacter (2) Delimiters
    val wordRegexUnescaped: String = """(?s)(.*?)(?=(?<!%1$s)%2$s|\z)"""

    val (sepsParser, sepsRegex) = this.buildDelims(separators)
    val (termsParser, termsRegex) = this.buildDelims(terminators)
    val pSeps: Parser[String] = this.combineLongest(sepsParser)
    val pTerms: Parser[String] = this.combineLongest(termsParser)

    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)

    val escapeEscapeCharacterRegex = convertDFDLLiteralToRegex(escapeEscapeCharacter)
    val escapeCharacterRegex = convertDFDLLiteralToRegex(escapeCharacter)

    val word: Parser[String] = escapeEscapeCharacter match {
      case "" => {
        if (separators.size == 0 && terminators.size == 0) { """(?s).*""".r }
        else { String.format(wordRegexUnescaped, escapeCharacterRegex, delimsRegex).r }
      }
      case _ => {
        val escapeEscapeCharacterRegex = convertDFDLLiteralToRegex(escapeEscapeCharacter)
        if (separators.size == 0 && terminators.size == 0) { """(?s).*""".r }
        else { String.format(wordRegexEscaped, escapeEscapeCharacterRegex, escapeCharacterRegex, delimsRegex).r }
      }
    }

    val result = parseInputDefault(word, pSeps, pTerms, input, "escapeCharacter", charset)

    val newField = removeEscapeCharacters(result.field, escapeEscapeCharacter, escapeCharacter.charAt(0), delimsRegex)

    result.numCharsRead = result.field.length()

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

  def getDfdlLiteralRegex(dfdlLiteralList: Set[String]): String = {
    val (_, regex) = this.buildDelims(dfdlLiteralList)
    combineDelimitersRegex(regex, Array.empty[String])
  }

  def isFieldDfdlLiteral(field: String, dfdlLiteralList: Set[String]): Boolean = {
    val dfdlLiteralRegex = getDfdlLiteralRegex(dfdlLiteralList)
    val m = Pattern.compile(dfdlLiteralRegex).matcher(field)
    m.find()
    m.matches()
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
      val isEsEs: Boolean = if (eses.length == 1 && eses.charAt(0) == c) { true } else { false }

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
      val isEsEs: Boolean = if (eses.length() == 1 && c == eses.charAt(0)) { true } else { false }

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
