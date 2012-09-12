package delimsearch
import scala.annotation.migration
import scala.collection.mutable.Queue
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Reader
import delimsearch.DelimiterType.DelimiterType
import scala.util.matching.Regex

class DelimParseResult {
  var field: String = ""
  var isSuccess: Boolean = false
  var delimiter: String = ""
  var delimiterType: DelimiterType = DelimiterType.Delimiter

  def apply(pField: String, pIsSuccess: Boolean, pDelimiter: String, pDelimiterType: DelimiterType) = {
    field = pField // The parsed field
    isSuccess = pIsSuccess // parse success or failure
    delimiter = pDelimiter // delimiter denoting the field
    delimiterType = pDelimiterType // Might be useful to know if the delimiter was a separator or terminator
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
    input: Reader[Char], name: String): DelimParseResult = {
    // TODO: Should we have EOF passed as a separator or terminator?  That way we can error when we do not
    // expect the data to be terminated by EOF?

    // The Parse Statement: field ~ (seps | terms)
    // might be overkill as the field parser seems to find the field no problem.
    //
    // However, the addition of "~ (seps | terms)" guarantees that we will receive a failure
    // if a separator or terminator is not found!
    val res = this.parse(this.log(field ~ (seps | terms))("DelimParser." + name), input)
    res

    var fieldResult = ""
    var delimiterResult = ""
    var isSuccess: Boolean = false
    var delimiterType = DelimiterType.Delimiter

    if (!res.isEmpty) {
      fieldResult = res.get._1
      delimiterResult = res.get._2
      isSuccess = true
      val result = this.parse(seps, delimiterResult)
      if (result.isEmpty) { delimiterType = DelimiterType.Terminator }
      else { delimiterType = DelimiterType.Separator }
    }

    val result: DelimParseResult = new DelimParseResult
    result(fieldResult, isSuccess, delimiterResult, delimiterType)
    result
  }

  def failedResult: DelimParseResult = {
    val result: DelimParseResult = new DelimParseResult
    result("", false, "", DelimiterType.Delimiter)
    result
  }

  def parseInput(separators: Set[String], terminators: Set[String], input: Reader[Char]): DelimParseResult = {
    if (terminators.size == 0 && separators.size == 0) { return failedResult }

    val (sepsParser, sepsRegex) = this.buildDelims(separators)
    val (termsParser, termsRegex) = this.buildDelims(terminators)
    val pSeps: Parser[String] = this.combineLongest(sepsParser)
    val pTerms: Parser[String] = this.combineLongest(termsParser)

    val delimsRegex = combineDelimitersRegex(sepsRegex, termsRegex)

    // A word is anything but a delimiter or EOF
    val wordRegex: String = """((.*?)(?=(%s)|\z))"""
    val word: Parser[String] = String.format(wordRegex, delimsRegex).r

    val result = parseInputDefault(word, pSeps, pTerms, input, "default")
    result
  }

  def parseInputEscapeBlock(separators: Set[String], terminators: Set[String],
    input: Reader[Char], escapeBlockStart: String, escapeBlockEnd: String,
    escapeEscapeCharacter: String = ""): DelimParseResult = {

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
    
    val result = parseInputDefault(word, pSeps, pTerms, input, "escapeBlock")
    
    // If failed, try regular parse.
    if (!result.isSuccess){ return parseInput(separators, terminators, input)}
    result
  }

  def parseInputEscapeCharacter(separators: Set[String], terminators: Set[String],
    input: Reader[Char], escapeCharacter: String, escapeEscapeCharacter: String = ""): DelimParseResult = {

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

    println(escapeEscapeCharacterRegex)
    println(escapeCharacterRegex)
    
    val word: Parser[String] = escapeEscapeCharacter match {
      case "" => String.format(wordRegexUnescaped, escapeCharacterRegex, delimsRegex).r
      case _ => {
        val escapeEscapeCharacterRegex = convertDFDLLiteralToRegex(escapeEscapeCharacter)
        String.format(wordRegexEscaped, escapeEscapeCharacterRegex, escapeCharacterRegex, delimsRegex).r
      }
    }

    val result = parseInputDefault(word, pSeps, pTerms, input, "escapeCharacter")
    
    val esChar: Parser[String] = escapeCharacterRegex.r
    val esEsChar: Parser[String] = escapeEscapeCharacterRegex.r
    
    val anything: Parser[String] = """(.*?)""".r
    val field: Parser[String] = anything <~ (opt(escapeEscapeCharacter) ~ escapeCharacter ~ (pSeps | pTerms))
    
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

}
