package daffodil.grammar
import java.nio.CharBuffer

class DelimSearcher {
  var delimiters: List[PNode] = List.empty[PNode]
  
  def addDelimiter(strDelim: String) = {
    val p = new PNode(strDelim)
    p(strDelim)
    delimiters ++= List[PNode]{p}
  }

  def searchAll(input: String, startPos: Int = 0) = {
    clear
    if (startPos >= 0 && startPos < input.length()){
	    for (i <- startPos until input.length()){
	      delimiters.foreach(x => x.checkMatchChar(input.charAt(i), i))
	    }
    }
  }
  
  // Search Until a delimiter is satisfied
  def searchUntil(input: String, startPos: Int = 0): String = {
    clear
    if (startPos >= 0 && startPos < input.length()){
	    for (i <- startPos until input.length()){
	      delimiters.foreach(x => {
	        x.checkMatchChar(input.charAt(i), i)
	        if (x.isMatched){
	          return input.substring(startPos, x.indexes.head._1)
	        }
	        })
	    }
    }
    ""
  }
  
  // Search Until a delimiter is satisfied or end of buffer
  // Returns:
  //	Indexing Error:	Empty String, -1
  //	End of buffer:	Input String, -1
  //	Success:		String, endPos
  // TODO:	Probably want to change to give a better status message or use a better
  //		status structure!
  //
  def searchUntilDelimiter(input: CharBuffer, startPos: Int = 0): (String, Int) = {
    clear
    
    if (startPos >= 0 && startPos < input.length()){
	    for (i <- startPos until input.length()){
	      delimiters.foreach(x => {
	        x.checkMatchChar(input.charAt(i), i)
	        if (x.isMatched){
	          // A match was found, return the contents of the buffer
	          // until the delimiter.
	          var endPos = x.indexes.head._1
	          return (input.subSequence(startPos, endPos).toString(), endPos)
	        }
	        })
	    }
	    // Reached end of buffer and a delimiter was not found
	    // return the full buffer.
	    return (input.toString(), -1)
    }
    ("", -1)
  }
  
  def printMatchStruct = {
    delimiters foreach {
      x => x.print
    }
  }
  
  def clear = {
    delimiters.foreach( x => x.clear)
  }
}

class PNode(delimiter: String) {
  var indexes: collection.mutable.Set[(Int, Int)] = collection.mutable.Set.empty[(Int, Int)]
  var delimBuf: Array[(Char, Boolean, Int, Int)] = Array.empty[(Char, Boolean, Int, Int)]

  def apply(pDelimiter: String) = {
    val length = pDelimiter.length()
    delimBuf = new Array[(Char, Boolean, Int, Int)](length)

    resetDelimBuf
  }

  def resetDelimBuf = {
    var newDelimBuf = new Array[(Char, Boolean, Int, Int)](delimiter.length())
    for (i <- 0 until delimBuf.length) {
      delimBuf(i) = (delimiter.charAt(i), false, i,-1)
    }
  }

  def isMatched: Boolean = {
    indexes.size >= 1
  }

  //	Evaluate whether or no the supplied character
  //	exists within the delimiter list
  //
  def checkMatchChar(charIn: Char, charPosIn: Int) = {

    // We only care to check against characters that were 
    // previously unmatched.
    val unMatched = delimBuf.filter(x => x._2 != true)

    if (unMatched.length > 0) {
      val (char, bool, index, charPos) = unMatched(0)
      if (char == charIn) {
        delimBuf.update(index, (char, true, index, charPosIn))
      } else {
        resetDelimBuf
      }
    }
    processDelimBuf
  }

  //	If all characters were matched this pass
  //	add the character positions to the index
  //	list and reset the delimBuf
  //
  def processDelimBuf = {
    if (delimBuf.length > 0) {
      val allMatched = delimBuf.filter(x => x._2 == false).length == 0
      if (allMatched) {
        val startPos = delimBuf(0)._4
        val endPos = delimBuf(delimBuf.length - 1)._4
        add(startPos, endPos)
        resetDelimBuf
      }
    }
  }

  def add(startPos: Int, endPos: Int) = {
    indexes += (startPos -> endPos)
  }
  
  def clear = {
    resetDelimBuf
    indexes.clear()
  }
  
  def print = {
    println("DELIMITER: '" + delimiter + "'")
    println("INDEXES: ")
    indexes foreach {
      x => println("\tSTART POS: " + x._1 + "\tEND POS: " + x._2)
    }
  }
}