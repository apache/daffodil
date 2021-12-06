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

package org.apache.daffodil.util

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.InputStream
import java.net.URI
import java.net.URL
import java.net.URLClassLoader
import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.channels.ReadableByteChannel
import java.nio.channels.WritableByteChannel
import java.nio.charset.CodingErrorAction
import java.nio.charset.{ Charset => JavaCharset }
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

import scala.collection.JavaConverters._

import org.apache.daffodil.equality._

/**
 * Various reusable utilities that I couldn't easily find a better place for.
 */
object Misc {

  def boolToOpt[T](test: Boolean, thing: => T): Option[T] = {
    if (test) Some(thing) else None
  }

  def getNameFromClass(obj: Any): String = getNameGivenAClassObject(obj.getClass)

  def getNameGivenAClassObject(clazz: Class[_]): String = {
    if (clazz == null) return "null"
    val nonPackageClassName = clazz.getName.split("""\.""").toList.reverse.head
    val nonDollarsParts = nonPackageClassName.split("""\$""").toList.reverse
    val nonNumericParts = nonDollarsParts.filterNot { _.matches("""\d*""") }
    val nameToken = nonNumericParts.head
    nameToken
  }

  /**
   * Removes a suffix string, if it is found.
   */
  def stripSuffix(s: String, suffix: String) = {
    if (s.endsWith(suffix))
      s.substring(0, s.length - suffix.length)
    else s
  }

  def stripQuotes(s: String) = {
    val stripFirst = if (s.startsWith("\"")) s.substring(1) else s
    val stripLast = if (stripFirst.endsWith("\"")) stripFirst.substring(0, stripFirst.length - 1) else stripFirst
    stripLast
  }

  def isNullOrBlank(s: String) = {
    val b = Option(s).isEmpty || s.trim.isEmpty
    b
  }

  def isFileURI(uri: URI) = {
    if (uri.isAbsolute()) {
      val protocol = uri.toURL.getProtocol()
      val result = protocol == "file"
      result
    } else
      false
  }

  /**
   * Takes care of using the resource built-in to the jar, or
   * if we're just running interactively in eclipse or Intellij, doesn't use the jar.
   * But those put the same things on the classpath.
   *
   * resourcePath argument is relative to the classpath root.
   */
  def getResourceOption(resourcePathRaw: String): (Option[URI], String) = {
    val resourcePath = resourcePathRaw.replaceAll("""\s""", "%20")

    // more time is wasted by people forgetting that the initial "/" is needed
    // to get classpath relative behavior... Let's make sure there is a leading "/"
    val resPath = if (resourcePath.startsWith("/")) resourcePath else "/" + resourcePath
    val res = this.getClass().getResource(resPath)
    if (res == null) {
      (None, resPath)
    } else (Some(res.toURI), resPath)
  }

  /**
   * Gets a resource on the classpath, or relative to another URI
   */
  def getResourceRelativeOption(rawResName: String, optContextURI: Option[URI]): Option[URI] = {
    val resName = rawResName.replaceAll("""\s""", "%20")
    val (maybeRes, _) = Misc.getResourceOption(resName)
    if (maybeRes.isDefined) {
      maybeRes // found directly on the classpath.
    } else {
      optContextURI.flatMap { contextURI =>
        //
        // try relative to enclosing context uri
        //
        // Done using URL constructor because the URI.resolve(uri) method
        // doesn't work against so called opaque URIs, and jar URIs of the
        // sort we get here if the resource is in a jar, are opaque.
        // Some discussion of this issue is https://issues.apache.org/jira/browse/XMLSCHEMA-3
        //
        val contextURL = contextURI.toURL
        val completeURL = new URL(contextURL, resName)
        val res = tryURL(completeURL)
        res
      }
    }
  }

  /**
   * Search for a resource name, trying a handful of heuristics.
   *
   * This is useful in cases where it's okay to be a little lax when searching
   * for a resource name (e.g. testing). This is often needed in situations
   * like directory structures not exactly match paths and we just want to try
   * our best to find a file. Heuristics include things like ignoring leading
   * directories in a resource, looking for a resource as a file, etc. This
   * does mean that it's possible that this could find the wrong file if there
   * are ambiguities, so be careful when using.
   *
   * If ambiguities would be a serious problem, use getResourceOption or
   * getResourceRelativeOption.
   */
  def searchResourceOption(resName: String, relativeTo: Option[URI]): Option[URI] = {
    val resAsURI = new URI(resName)
    val resPath =
      if (resAsURI.getScheme != null) Paths.get(resAsURI) else Paths.get(resName)
    val resolvedURI =
      if (Files.exists(resPath)) Some(resPath.toFile().toURI())
      else Misc.getResourceRelativeOption(resName, relativeTo)
    val res = resolvedURI.orElse {
      // try ignoring the directory part
      val parts = resName.split("/")
      if (parts.length > 1) { // if there is one
        val filePart = parts.last
        val secondTry = searchResourceOption(filePart, relativeTo) // recursively
        secondTry
      } else {
        None
      }
    }
    res
  }

  private def tryURL(url: URL): Option[URI] = {
    var is: InputStream = null
    val res =
      try {
        is = url.openStream()
        // worked! We found it.
        Some(url.toURI)
      } catch {
        case e: java.io.IOException => None
      } finally {
        if (is != null) is.close()
      }
    res
  }

  lazy val classPath = {
    val cl = this.getClass().getClassLoader()
    val urls = cl match {
      case url: URLClassLoader => url.getURLs().toSeq
      case _ => Seq.empty
    }
    urls
  }

  def getRequiredResource(resourcePath: String): URI = {
    getResourceOption(resourcePath) match {
      case (None, resPath) => {
        val msg = "Required resource " + resPath + " was not found.\nClasspath is " +
          (if (classPath.length == 0) "unknown."
          else ": " + classPath.mkString("\n"))
        throw new java.io.FileNotFoundException(msg)
      }
      case (Some(res), _) => res
    }
  }

  def initialUpperCase(s: String): String = s.head.toUpper + s.substring(1)
  def initialLowerCase(s: String): String = s.head.toLower + s.substring(1)

  /**
   * Convert FooBar to fooBar, but leave FOOBAR as FOOBAR.
   */
  def toInitialLowerCaseUnlessAllUpperCase(s: String): String = {
    if (s(0).isLower) return s
    //
    // At this point we know the first letter is uppercase
    //
    if (isAllUpper(s, 1)) s
    else s(0).toLower + s.substring(1)
  }

  def isAllUpper(s: String, start: Int): Boolean = {
    var i = start
    val l = s.length
    while (i < l) {
      if (s(i).isLower) return false
      i += 1
    }
    true
  }

  /**
   * Returns true if a StringBuilder contains all whitespace
   */
  def isAllWhitespace(sb: StringBuilder): Boolean = {
    if (sb.isEmpty) false
    else {
      var in = 0
      val sbLen = sb.length
      while (in < sbLen) {
        if (!sb.charAt(in).isWhitespace) return false
        in += 1
      }
      true
    }
  }

  /**
   * Returns the primary version of daffodil from the jar
   */
  def getDaffodilVersion: String = {
    val implVersion = this.getClass.getPackage.getImplementationVersion
    if (implVersion == null) {
      ""
    } else {
      implVersion
    }
  }

  /**
   * Requires an even number of nibbles.
   */
  def hex2Bytes(hex: String): Array[Byte] = {
    // This function originally just looked like this:
    //
    //   hex.sliding(2, 2).toArray.map { Integer.parseInt(_, 16).toByte }
    //
    // Although that is very clear and simple, it is actually very slow. The
    // code below, which accomplishes the same thing, is about 2 orders of
    // magnitude faster. We're leaving the above comment in place to more
    // clearly show what this function is doing, but keeping this below code
    // due to its speed, which is very important for schemas containing
    // xs:hexBinary types.

    def hexCharToValue(c: Char): Int = {
      val i = c.toInt
      val v =
        if (i >= 48 && i <= 57) i - 48 // number 0-9
        else if (i >= 65 && i <= 70) (i - 65) + 10 // capital A-F
        else if (i >= 97 && i <= 102) (i - 97) + 10 // lowercase a-f
        else throw new java.lang.IllegalArgumentException("Hex character must be 0-9, a-z, or A-Z, but was '" + c + "'")
      v
    }

    val len = hex.length
    if (len % 2 != 0) {
      throw new java.lang.IllegalArgumentException("Hex string must have an even number of characters, but was " + len)
    }
    val numBytes: Int = len / 2
    val arr = new Array[Byte](numBytes)
    var arrIdx: Int = 0
    var hexIdx: Int = 0
    while (arrIdx < numBytes) {
      val l = hexCharToValue(hex(hexIdx))
      val r = hexCharToValue(hex(hexIdx + 1))
      arr(arrIdx) = ((l << 4) | r).toByte
      arrIdx += 1
      hexIdx += 2
    }
    arr
  }

  def hex2Bits(hex: String): String = {
    val nums = hex.map { ch => Integer.parseInt(ch.toString, 16) }
    val bits = nums map { java.lang.Long.toString(_, 2) }
    val paddedBits = bits map { "%4s".format(_).replaceAll(" ", "0") }
    val res = paddedBits.mkString
    res
  }

  private val hexLookup = "0123456789ABCDEF".toArray

  def bytes2Hex(bytes: Array[Byte]): String = {
    val hexArr = new Array[Char](bytes.length * 2)
    var bytIdx = 0
    var hexIdx = 0
    while (bytIdx < bytes.length) {
      val b = bytes(bytIdx) & 0xFF
      hexArr(hexIdx) = hexLookup(b >>> 4)
      hexArr(hexIdx + 1) = hexLookup(b & 0x0F)
      bytIdx += 1
      hexIdx += 2
    }
    new String(hexArr)
  }

  def bits2Bytes(bits: String): Array[Byte] =
    if (bits.isEmpty()) Nil.toArray
    else bits2Bytes(Seq(bits))

  def bits2Bytes(bits: Seq[String]): Array[Byte] = {
    // split at any character not a 0 or 1, then concatenate all
    val allBitsOnly = bits.flatMap { _.split("[^01]") }.mkString
    val byteSizedBits = allBitsOnly.sliding(8, 8)
    byteSizedBits.map(Integer.parseInt(_, 2).toByte).toArray
  }

  def bytes2Bits(bytes: Array[Byte]): String = {
    bytes.map { b => (b & 0xFF).toBinaryString.reverse.padTo(8, '0').reverse }.mkString
  }

  // Moved here from Compiler object.

  def stringToReadableByteChannel(s: String): ReadableByteChannel = {
    val bytes = s.getBytes(StandardCharsets.UTF_8) // never use default charset. NEVER.
    byteArrayToReadableByteChannel(bytes)
  }

  def stringToWritableByteChannel(s: String): WritableByteChannel = {
    val size = s.length() // TODO: get byte count by encoding
    byteArrayToWritableByteChannel(size)
  }

  def byteArrayToReadableByteChannel(bytes: Array[Byte]): ReadableByteChannel = {
    val inputStream = new ByteArrayInputStream(bytes);
    val rbc = java.nio.channels.Channels.newChannel(inputStream);
    rbc
  }

  def byteArrayToWritableByteChannel(size: Int): WritableByteChannel = {
    val outputStream = new ByteArrayOutputStream(size);
    val wbc = java.nio.channels.Channels.newChannel(outputStream);
    wbc
  }

  def fileToReadableByteChannel(file: java.io.File): ReadableByteChannel = {
    val inputStream = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(inputStream);
    rbc
  }

  /**
   * This function creates a representation of data which doesn't
   * contain any whitespace characters that jump around the screen.
   * It replaces those with characters that have a simple glyph.
   *
   * The point of this is when you display the stream of data for
   * debugging, or for a diagnostic message,
   * the characters which control position like CR, LF, FF,
   * VT, HT, BS, etc. all make it hard to figure out what is going on.
   * Replacing these with the picture characters (designed for this purpose)
   * in the unicode x2400 block helps.
   */
  def remapControlsAndLineEndingsToVisibleGlyphs(s: String) = {
    s.map { remapControlOrLineEndingToVisibleGlyphs(_) }.mkString
  }

  def remapControlOrLineEndingToVisibleGlyphs(c: Char) = {
    val URC = 0x2426 // Unicode control picture character for substutition (also looks like arabic q-mark)
    val code = c.toInt match {
      //
      // C0 Control pictures
      case n if (n <= 0x1F) => n + 0x2400
      case 0x20 => 0x2423 // For space we use the SP we use the ␣ (Unicode OPEN BOX)
      case 0x7F => 0x2421 // DEL pic isn't at 0x247F, it's at 0x2421
      //
      // Unicode separators & joiners
      case 0x00A0 => URC // no-break space
      case 0x200B => URC // zero width space
      case 0x2028 => URC // line separator
      case 0x2029 => URC // paragraph separator
      case 0x200C => URC // zero width non-joiner
      case 0x200D => URC // zero width joiner
      case 0x2060 => URC // word joiner
      // bi-di controls
      case 0x200E | 0x200F => URC
      case b if (b >= 0x202A && b <= 0x202E) => URC
      // byte order mark
      case 0xFFFE => URC // ZWNBS aka Byte Order Mark
      case 0xFFFF => URC // non-character FFFF
      // we assume surrogate codepoints all have a glyph (depends on font used of course)
      //
      // TODO: this could go on and on. There's a flock of 'space' characters (EM SPACE)
      // all over the place in Unicode.
      //
      // TODO: combining characters,
      // all whitespace, zero-width, and combining/joining characters would be
      // represented by a separate glyph-character.
      //
      // Probably could be done by checking the character against some
      // unicode regex character classes like \p{M} which is the class
      // of combining mark characters
      //
      //
      // Special case - if incoming character is one of the glyph
      // characters we're remapping onto, then change to URC
      //
      case n if (n > 0x2400 && n < 0x2423) => URC
      case _ => c
    }
    code.toChar
  }

  private val bytesCharset = JavaCharset.forName("windows-1252") // same as iso-8859-1 but has a few more glyphs.
  private val bytesDecoder = {
    val decoder = bytesCharset.newDecoder()
    decoder.onMalformedInput(CodingErrorAction.REPLACE)
    decoder.onUnmappableCharacter(CodingErrorAction.REPLACE)
    decoder
  }

  /**
   * Used when creating a debugging dump of data, where the data might be binary stuff
   * but we want to show some sort of glyph for each byte.
   *
   * This uses windows-1252 for all the places it has glyphs, and other unicode
   * glyph characters to replace the control characters and unused characters.
   *
   * This allows printing a data dump to the screen, without worry that the control
   * characters will ring bells or cause the text to jump around, and unmapped
   * characters will not look like spaces, nor all look like the same unicde replacement
   * character.
   */
  def remapBytesToVisibleGlyphs(bb: ByteBuffer, cb: CharBuffer): Unit = {
    val numBytes = bb.remaining()
    bytesDecoder.decode(bb, cb, true)
    cb.flip
    var i = 0
    while (i < numBytes) {
      val newCodepoint = remapOneByteToVisibleGlyph(bb.get(i))
      if (newCodepoint != -1) {
        cb.put(i, newCodepoint.toChar)
      }
      i += 1
    }
  }

  /**
   * For unicode codepoints in the range 0 to 255, or signed -128 to 127,
   * make sure there is a visible glyph.
   */
  def remapCodepointToVisibleGlyph(codepoint: Int): Int = {
    if (codepoint > 255 || codepoint < -128) return codepoint
    val b = codepoint.toByte
    val r = remapOneByteToVisibleGlyph(b)
    if (r == -1) codepoint else r
  }

  def remapStringToVisibleGlyphs(s: String) = {
    s.map { c => remapCodepointToVisibleGlyph(c.toInt).toChar }
  }

  def remapBytesToStringOfVisibleGlyphs(ba: Array[Byte]): String = {
    ba.map { b => remapCodepointToVisibleGlyph(b.toInt).toChar }.mkString
  }

  def remapByteToVisibleGlyph(b: Byte): Int = {
    val bb = ByteBuffer.allocate(1)
    bb.put(0, b)
    val cb = CharBuffer.allocate(1)
    remapBytesToVisibleGlyphs(bb, cb)
    cb.get(0).toChar.toInt
  }

  /**
   * Remaps a byte to a unicode codepoint for a visible picture, or -1 if
   * no remapping is needed.
   *
   * A difficulty is that there do not seem to be generally available Unicode fonts
   * which are truly monospaced for every Unicode character. So since we are
   * trying to produce data dumps that are monospaced, the tabular layout is off a bit.
   *
   * Even if there was such a font, it wouldn't be the default font.
   *
   * Courier New seems to work well. It is monospaced for every character we use
   * in this remap stuff. But not for the "double wide" Kanji or other wide oriental
   * characters.
   */
  private def remapOneByteToVisibleGlyph(b: Byte): Int = {
    Bits.asUnsignedByte(b) match {
      //
      // replace C0 controls with unicode control pictures
      //
      case n if (n <= 0x1F) => n + 0x2400
      //
      // replace space and DEL with control pictures
      //
      case 0x20 => 0x2423 // For space we use the SP we use the ␣ (Unicode OPEN BOX)
      case 0x7F => 0x2421 // DEL pic isn't at 0x247F, it's at 0x2421
      //
      // replace undefined characters in the C1 control space with
      // glyph characters. These are the only codepoints in the C1
      // space which do not have a glyph defined by windows-1252
      //
      // We remap these into the Unicode Latin Extended B codepoints by
      // adding 0x100 to their basic value.
      //
      case 0x81 => 0x0181
      case 0x8D => 0x018d
      case 0x8F => 0x018F
      case 0x90 => 0x0190
      case 0x9D => 0x019D
      //
      // Non-break space
      //
      case 0xA0 => 0x2422 // little b with stroke
      case 0xAD => 0x002D // soft hyphen becomes hyphen
      case regular => -1 // all other cases -1 means we just use the regular character glyph.
    }
  }

  /**
   * True if this charset encoding is suitable for display using the
   * all-visible-glyph stuff above.
   */
  def isAsciiBased(csName: String): Boolean = isAsciiBased(JavaCharset.forName(csName))

  def isAsciiBased(cs: JavaCharset): Boolean = {
    val aliases: Seq[String] = cs.aliases().asScala.toSeq.map { _.toUpperCase }
    val byName =
      aliases.exists { s =>
        !(s.contains("7-BIT")) &&
          !(s.contains("EBCDIC")) && (
            s.startsWith("ASCII") ||
            s.startsWith("US-ASCII") ||
            s.startsWith("ISO-8859") ||
            s.startsWith("UTF"))
      }
    if (byName) byName
    else {
      val decoder = cs.newDecoder()
      decoder.onMalformedInput(CodingErrorAction.REPLACE)
      decoder.onUnmappableCharacter(CodingErrorAction.REPLACE)
      val abcBytes = "abc".getBytes("ascii")
      val bb = ByteBuffer.wrap(abcBytes)
      val cb = decoder.decode(bb)
      val abc = cb.toString()
      if (abc =:= "abc") true
      else false
    }
  }

  import scala.language.reflectiveCalls // scala 2.10 creates warning unless we have this.
  /**
   * Convenient I/O tools
   */
  def using[A <: { def close(): Unit }, B](param: A)(f: A => B): B =
    try { f(param) } finally { param.close() }

  /**
   * convenience method
   */
  def writeToFile(fileName: String)(body: java.io.Writer => Unit): Unit = {
    val writer = new java.io.FileWriter(fileName)
    using(writer) { body(_) }
  }

  /**
   * Detects the encoding of the File for us.
   *
   * This was needed for the ConstructingParser
   * in order to read in xml from a file.
   */
  def determineEncoding(uri: URI): String = {
    val encH = scala.xml.include.sax.EncodingHeuristics
    val is = uri.toURL.openStream()
    val bis = new java.io.BufferedInputStream(is)
    val enc = encH.readEncodingFromStream(bis)
    is.close()
    enc
  }

  def determineEncoding(file: File): String = determineEncoding(file.toURI)

  /**
   * CharSequence to String
   */
  def csToString(cs: CharSequence): String = {
    cs match {
      case s: String => s
      case _ => {
        val sb = new StringBuilder(cs.length())
        sb.append(cs)
        sb.mkString
      }
    }
  }

  /**
   * Java throwable/exception objects may or may not have a message. They are supposed to have a cause if they
   * don't have a message of their own, but might have neither, or might have both.
   *
   * This is too painful to deal with in code when you want to be generic about converting throws/exceptions
   * into diagnostic information.
   *
   * So we have a more uniform behavior. Never returns null. Always gets a message.
   * If the argument has none, but has a cause object, then it
   * gets the message from that, if that has no message, it chases further.
   * Ultimately, if there's no message, it just uses the innermost cause object's class name.
   */

  def getSomeMessage(th: Throwable): Some[String] = {
    val m = th.getMessage()
    val c = th.getCause()
    val res = (m, c) match {
      case (null, null) => Misc.getNameFromClass(th)
      case ("", null) => Misc.getNameFromClass(th)
      case (m, null) => m
      case (null, c) => getSomeMessage(c).get
      case (m, c) => {
        val Some(cmsg) = getSomeMessage(c)
        cmsg + " (within " + m + ")"
      }
    }
    Some(res)
  }

  def getSomeCause(th: Throwable): Some[Throwable] = {
    val c = th.getCause()
    val res = c match {
      case null => th
      case _ => getSomeCause(c).get
    }
    Some(res)
  }
}
