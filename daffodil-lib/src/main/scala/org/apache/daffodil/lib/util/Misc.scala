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

package org.apache.daffodil.lib.util

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.IOException
import java.lang.reflect.Field
import java.net.URI
import java.net.URLClassLoader
import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import java.nio.channels.WritableByteChannel
import java.nio.charset.Charset
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets
import java.nio.charset.{ Charset => JavaCharset }
import java.nio.file.Files
import java.nio.file.Paths
import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.Using

import org.apache.daffodil.lib.equality._
import org.apache.daffodil.lib.exceptions.Assert

import passera.unsigned.UByte

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
    val stripLast =
      if (stripFirst.endsWith("\"")) stripFirst.substring(0, stripFirst.length - 1)
      else stripFirst
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
    val res = Option(this.getClass().getResource(resPath))
    (res.map(_.toURI), resPath)
  }

  /**
   * Gets a resource on the classpath, or relative to another URI
   */
  private def getResourceAbsoluteOrRelativeOption(
    rawResName: String,
    optContextURI: Option[URI]
  ): Option[URI] = {
    val resName = rawResName.replaceAll("""\s""", "%20")
    val (maybeRes, _) = Misc.getResourceOption(resName)
    if (maybeRes.isDefined) {
      maybeRes // found directly on the classpath.
    } else {
      optContextURI.flatMap { contextURI =>
        getResourceRelativeOnlyOption(resName, contextURI)
      }
    }
  }

  /**
   * Get resource relative to the context URI.
   *
   * Does NOT try the string as an absolute location first
   * or anything like that.
   *
   * @param relPath
   * @param contextURI
   * @return Some uri if the relative resource exists.
   */
  def getResourceRelativeOnlyOption(relPath: String, contextURI: URI): Option[URI] = {
    Assert.usage(relPath ne null)
    Assert.usage(contextURI ne null)
    if (contextURI.isOpaque) {
      //
      // We used to call new URL(jarURI, relativePathString)
      // but that is deprecated now (as of Java 20)
      //
      optRelativeJarFileURI(contextURI, relPath)
    } else {
      // context URI is not opaque. It's probably a file URI
      if (contextURI.getScheme == "file") {
        val relURI = contextURI.resolve(relPath)
        if (Paths.get(relURI).toFile.exists())
          Some(relURI)
        else None
      } else {
        // not a file nor an opaque resource URI. What is it?
        throw new IllegalArgumentException(s"Unrecognized URI type: $contextURI")
      }
    }
  }

  /**
   * Split a jar URI into its two component parts: 1) the path to a jar file 2) the resource
   * path within that jar file. By definition, these two parts are delimited by the last
   * occurrence of an exclamation point. If the resource path part contains an exclamation point
   * it should have already been escaped using "%21", which is the URI escape code for an
   * exclamation point.
   *
   * Note that it is important that we look for the last index of an exclamation point, since it
   * is possible some tools have added support for nested jars, and could have multiple
   * exclamation points for each nesting.
   */
  def splitJarUri(uri: URI): (String, String) = {
    Assert.invariant(uri.getScheme == "jar")
    var uriStr = uri.toString
    val exclamIdx = uriStr.lastIndexOf("!")
    val jarPart = uriStr.substring(0, exclamIdx)
    val pathPart = uriStr.substring(exclamIdx + 1)
    (jarPart, pathPart)
  }

  /**
   * Java 20 deprecated the 2-arg URL constructor which worked to create relative URIs
   * within the same Jar file.
   *
   * This is a bit harder to achieve now. You are not allowed to resolve relative to a jar file URI.
   * That is URI.resolve(relPath) doesn't work if the URI is a jar file URI.
   *
   * Now we have to hack the jar:file: URI as a string because URI.resolve won't work
   *
   * jar file URIs look like this:
   *
   *    `jar:file:/..absolute path to jar file.jar!/absolute path from root inside jar to file``
   *
   * We split this URI into its component parts, make a relative path on just the resource path
   * inside the jar, then glue back together.
   *
   * @param contextURI
   * @param relPath
   * @return Some(uri) for an existing relative path within the same jar file, or None if it does not exist.
   */
  def optRelativeJarFileURI(contextURI: URI, relPath: String): Option[URI] = {
    val (jarPart, pathPart) = Misc.splitJarUri(contextURI)
    Assert.invariant(pathPart.startsWith("/"))
    val contextURIPathOnly = URI.create(pathPart)
    val resolvedURIPathOnly = contextURIPathOnly.resolve(relPath)
    val newJarPathURI = URI.create(jarPart + "!" + resolvedURIPathOnly.toString)
    try {
      newJarPathURI.toURL.openStream().close()
      // that worked, so we can open it so it exists.
      Some(newJarPathURI)
    } catch {
      case io: IOException =>
        // failed. So that jar file doesn't exist
        None
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
      else Misc.getResourceAbsoluteOrRelativeOption(resName, relativeTo)
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

  // TODO: no test coverage
  def initialUpperCase(s: String): String = s.head.toUpper +: s.substring(1)
  def initialLowerCase(s: String): String = s.head.toLower +: s.substring(1)

  /**
   * Convert FooBar to fooBar, but leave FOOBAR as FOOBAR.
   */
  def toInitialLowerCaseUnlessAllUpperCase(s: String): String = {
    if (s(0).isLower) return s
    //
    // At this point we know the first letter is uppercase
    //
    if (isAllUpper(s, 1)) s
    else s(0).toLower +: s.substring(1)
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
    val uri = getRequiredResource("org/apache/daffodil/lib/VERSION")
    val version = Source.fromInputStream(uri.toURL.openStream, "UTF-8").mkString
    version
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
        else
          throw new NumberFormatException(
            "Hex character must be 0-9, a-f, or A-F, but was '" + c + "'"
          )
      v
    }

    val len = hex.length
    if (len % 2 != 0) {
      throw new NumberFormatException(
        "Hex string must have an even number of characters, but was " + len + " for " + hex
      )
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
    val bits = nums.map { java.lang.Long.toString(_, 2) }
    val paddedBits = bits.map { "%4s".format(_).replaceAll(" ", "0") }
    val res = paddedBits.mkString
    res
  }

  private val hexLookup = "0123456789ABCDEF".toArray

  def bytes2Hex(bytes: Array[Byte]): String = {
    Assert.invariant(bytes.length <= Int.MaxValue / 2)
    val hexArr = new Array[Char](bytes.length * 2)
    var bytIdx = 0
    var hexIdx = 0
    while (bytIdx < bytes.length) {
      val b = bytes(bytIdx) & 0xff
      hexArr(hexIdx) = hexLookup(b >>> 4)
      hexArr(hexIdx + 1) = hexLookup(b & 0x0f)
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
    bytes.map { b => (b & 0xff).toBinaryString.reverse.padTo(8, '0').reverse }.mkString
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
   * contain any control or whitespace characters that jump around the screen.
   * It replaces those with characters that have a simple glyph.
   *
   * The point of this is when you display the stream of data for
   * debugging, or for a diagnostic message,
   * the characters which control position like CR, LF, FF,
   * VT, HT, BS, etc. all make it hard to figure out what is going on.
   * Replacing these with the picture characters (designed for this purpose)
   * in the unicode x2400 block helps.
   */
  def remapStringToVisibleGlyphs(s: String) =
    nonGlyphToVisibleGlyphsRemapper.remap(s)

  object nonGlyphToVisibleGlyphsRemapper extends CharacterSetRemapper {
    override protected def remap(prev: Char, curr: Char, next: Char): Int =
      remapControlOrLineEndingToVisibleGlyphs(curr)
  }

  /**
   * For debugger displays, data dumps, etc.
   *
   * Control characters, line-endings, spaces, and various others do not have a glyph that is displayed.
   * They also can cause text to be split across lines, bells to ring, characters to be overwritten, etc.
   *
   * Convert to a character that has a glyph. Unicode provides some control-picture
   * characters for this purpose. Note that this by-definition loses information, and many characters
   * will map to the unicode replacement character. It is expected this display would be complemented
   * by a hex dump or other means of understanding the actual representation of these remapped
   * characters.
   *
   * Keep in mind this is a Unicode to Unicode transformation. It is not starting from byte values.
   * See `byteToChar(b)` for how to got from raw byte values to unicode chars.
   * @param c a unicode character that may or may not have a glyph.
   * @param replaceControlPictures when true means the Unicode control pictures characters are replaced by
   *                               the Unicode replacement character. When false these characters are preserved.
   *                               Defaults to false.
   * @return a unicode character that definitely has a glyph.
   */
  def remapControlOrLineEndingToVisibleGlyphs(
    c: Char,
    replaceControlPictures: Boolean = false
  ): Char = {
    val URC =
      0x2426 // Unicode control picture character for substitution (also looks like arabic q-mark)
    val code: Int = c.toInt match {
      //
      // C0 Control pictures
      case n if (n <= 0x1f) => n + 0x2400
      case 0x20 => 0x2423 // For space we use the SP we use the ␣ (Unicode OPEN BOX)
      case 0x7f => 0x2421 // DEL pic isn't at 0x247F, it's at 0x2421
      //
      // We remap these into the Unicode Latin Extended B codepoints by
      // adding 0x100 to their basic value.
      //
      case n if (n >= 0x80 && n <= 0x9f) =>
        n + 0x100
      case 0xa0 => 0x2422 // non-break space => ␢ (blank symbol or little b with stroke)
      case 0xad => 0x002d // soft hyphen => hyphen
      //
      // Unicode separators & joiners
      case 0x200b => URC // zero width space
      case 0x2028 => URC // line separator
      case 0x2029 => URC // paragraph separator
      case 0x200c => URC // zero width non-joiner
      case 0x200d => URC // zero width joiner
      case 0x2060 => URC // word joiner
      // bi-di controls
      case 0x200e | 0x200f => URC
      case b if (b >= 0x202a && b <= 0x202e) => URC
      // byte order mark
      case 0xfffe => URC // ZWNBS aka Byte Order Mark
      case 0xffff => URC // non-character FFFF
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
      // characters we're remapping onto, then we could issue
      // a substitution character, but there are things that depend
      // on these being preserved. So we have a flag to control this.
      //
      case n if (n > 0x2400 && n < 0x2423 && replaceControlPictures) => URC
      case x => x
    }
    code.toChar
  }

  private lazy val byteToCharTable = {
    val cs = Charset.forName("windows-1252")
    val dec = cs
      .newDecoder()
      .onUnmappableCharacter(CodingErrorAction.REPLACE)
      .onMalformedInput(CodingErrorAction.REPORT)
    val bb = ByteBuffer.wrap((0 to 255).map { i => i.toByte }.toArray)
    val cb = dec.decode(bb)
    assert(cb.position() == 0)
    assert(cb.limit() == 256)
    // These 5 are unmapped by Windows-1252 but we want to turn any
    // byte into a legit character. So these We add 0x100
    // to get unicode codepoints.
    cb.put(0x81, 0x181.toChar)
    cb.put(0x8d, 0x18d.toChar)
    cb.put(0x8f, 0x18f.toChar)
    cb.put(0x90, 0x190.toChar)
    cb.put(0x9d, 0x19d.toChar)
    val res = cb.toString
    res
  }

  /**
   * Convert a byte to a unicode character assuming the byte is iso-8859-1
   * (or really, windows-1252 which has a few more glyph chars but is otherwise
   * the same as iso-8859-1)
   *
   * This is a super pain to do using Java charsets because they
   * don't provide an API to convert one character, only byte buffers
   * into char buffers.
   *
   * So we just use a lookup table.
   * @param b a byte containing a code point of windows-1252 encoding
   * @return a unicode equivalent character
   */
  def byteToChar(b: Byte): Char = {
    byteToCharTable(UByte(b).toInt)
  }

  /**
   * This function creates a representation of data which doesn't
   * contain any control or whitespace characters that jump around the screen.
   * It replaces those with characters that have a simple glyph.
   *
   * The point of this is when you display the stream of data for
   * debugging, or for a diagnostic message, and it is mostly single-byte text
   * characters, then the characters which control position like CR, LF, FF,
   * VT, HT, BS, etc. all make it hard to figure out what is going on.
   * Replacing these with the picture characters (designed for this purpose)
   * in the unicode x2400 block helps.
   */
  def remapBytesToStringOfVisibleGlyphs(ba: Array[Byte]): String = {
    val len = ba.length
    if (len == 0) ""
    else {
      val sb = new StringBuilder(ba.length)
      var i: Int = 0
      while (i < ba.length) {
        sb.append(remapControlOrLineEndingToVisibleGlyphs(byteToChar(ba(i))))
        i += 1
      }
      sb.toString()
    }
  }

  def remapOneByteToVisibleGlyph(b: Byte) =
    remapControlOrLineEndingToVisibleGlyphs(byteToChar(b))

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
        !(s.contains("EBCDIC")) && (s.startsWith("ASCII") ||
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

  /**
   * convenience method
   */
  def writeToFile(fileName: String)(body: java.io.Writer => Unit): Unit = {
    val writer = new java.io.FileWriter(fileName)
    Using.resource(writer) { body(_) }
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
  def getAMessage(th: Throwable): String = {
    val m = th.getMessage()
    val c = th.getCause()
    val res = (m, c) match {
      case (null, null) => Misc.getNameFromClass(th)
      case ("", null) => Misc.getNameFromClass(th)
      case (m, null) => m
      case (null, c) => getAMessage(c)
      case (m, c) => {
        val cmsg = getAMessage(c)
        cmsg + " (within " + th.getClass.getSimpleName + " " + m + ")"
      }
    }
    Assert.invariant(res != null)
    res
  }

  def getSomeMessage(th: Throwable): Some[String] = Some(getAMessage(th))

  def getACause(th: Throwable): Throwable = {
    val c = th.getCause()
    val res = c match {
      case null => th
      case _ => getACause(c)
    }
    res
  }

  def getSomeCause(th: Throwable): Some[Throwable] = Some(getACause(th))

  /**
   * Get the diagnosticFilepath from a uri
   */
  def uriToDiagnosticFile(uri: URI): File = {
    uri.getScheme match {
      case "jar" => {
        val (_, pathPart) = Misc.splitJarUri(uri)
        Paths.get(pathPart).toFile
      }
      case "file" => Paths.get(uri).toFile
      case _ => Paths.get(uri.getPath).toFile
    }
  }

  // helper function for scala 2.13 and 3 compatibility
  def lookupDeclaredField(clazz: Class[?], fieldName: String): Field = {
    try {
      // TODO scala 2.12 phase out, we can remove the check for the plain name, and always
      // check the lazy name version instead
      clazz.getDeclaredField(fieldName)
    } catch {
      case _: NoSuchFieldException =>
        // scala 3 changes the way lazy val are deserialized so it looks like fieldName$lzy1
        clazz.getDeclaredField(fieldName + "$lzy1")
    }
  }

}
