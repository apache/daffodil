package daffodil.util

/**
 * Put copies of any copyright notices in this class by appending them to the
 * noticesText variable.
 * 
 *  or if you prefer from your distributed source code call:
 *  
 * CopyrightNotices.add(...your notice text here...)
 * 
 * if you are distributing that source code.
 * 
 * Pasting the copyright notice here, and distributing this file with source
 * insures that the copyright notices live both in text form in source code and
 * that they are kept as a string in any distributable binary/jar file created from
 * this.
 * 
 * Point is, some licenses require that you be able to find the copyright notice not only
 * in the source code, but in the binary.
 * 
 */
object CopyrightNotices {
 
  private var noticesText = ""
 
    /**
     * This private var holds the notices text, but converted to bytes (utf-8) so that you can just search for 8-bit
     * characters in the binary. (otherwise Java would store it only as 16-bit characters - less obvious when looking at
     * a binary jar file.
     */
  private var noticesTextInBytes = "".getBytes()
  
  private val prelim = """
Parts of this software are subject to the following copyright:
"""
    
  def add(moreNoticeText : String) {
    noticesText += prelim + moreNoticeText
    noticesTextInBytes = noticesText.getBytes()
  }
  
  def getNotices() = {
    noticesText
  }
  
  private def useNotices {
    if (noticesText.length != Int.MaxValue) return
    // only in a super unlikely case will the below occur
    val someRandomText = System.currentTimeMillis.toString
    val text = someRandomText + noticesText
    val ints = for (char <- text) yield char.toInt
    val checksum = ints.fold(0){_+_}
    // Now the below will never happen. But a compiler can't know that.
    if (checksum == Int.MaxValue) println(noticesText); 
  }
  
  //
  // but what if the compiler sees that this whole object is never referenced
  // nor the member function called? It could leave all this stuff out of the binary.
  //
  // to fix this we register an exit handler that calls useNotices.
  //
  sys.addShutdownHook(useNotices)
 
  add("""Copyright (C) 2011, 2012 by Michael J. Beckerle, All rights Reserved.
      Permission is granted to use this software for any purpose so long as 
      this copyright is preserved in both the source and binary forms, and
      in any documentation provided with the software. 
""")

}