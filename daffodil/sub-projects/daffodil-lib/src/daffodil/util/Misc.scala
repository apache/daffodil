package daffodil.util

import java.io.FileNotFoundException
import java.io.FileInputStream
import java.io.InputStream
import java.io.File
import java.net.URL

/**
 * Various reusable utilities that I couldn't easily find a better place for.
 */
object Misc {
  
  def getNameFromClass(obj : Object) : String = {
    if (obj == null) return "null"
    // val hexHash = obj.hashCode.formatted("%x")
    val nonPackageClassName = obj.getClass.getName.split("""\.""").toList.reverse.head
    val nonDollarsParts = nonPackageClassName.split("""\$""").toList.reverse
    val nonNumericParts = nonDollarsParts.filterNot{_.matches("""\d*""")}
    val nameToken = nonNumericParts.head
    nameToken // + "@" + hexHash
  }
   
  def stripQuotes(s : String) = {
    val stripFirst = if (s.startsWith("\"")) s.substring(1) else s
    val stripLast = if (stripFirst.endsWith("\"")) stripFirst.substring(0, stripFirst.length - 1) else stripFirst
    stripLast
  }
  /**
   * Takes care of using the resource built-in to the jar, or 
   * if we're just running interactively in eclipse or Intellij, doesn't use the jar.
   * But those put the same things on the classpath.
   * 
   * resourcePath argument is relative to the classpath root.
   */
  def getRequiredResource (resourcePath : String) : URL = {
    var res = this.getClass.getResource(resourcePath) 
    if (res == null) {
      val props = System.getProperties()
      val cp = props.getProperty("java.class.path", null)
      val cpLines = cp.replaceAll(":", "\n")
      val msg = "Required resource " + resourcePath + " was not found.\nClasspath is: " + cpLines
      System.err.println(msg)
      throw new Exception(msg)
      }
     res
  }
  
  def initialUpperCase(s: String): String = s.head.toUpper + s.substring(1)
  def initialLowerCase(s: String): String = s.head.toLower + s.substring(1)
  
  
  /**
   * Returns a tuple with the primary version number in the first slot and
   * the build number in the second slot. 
   */
  def getDaffodilVersion : Tuple2[String,String] = {
    val implVersion = Package.getPackage("daffodil.util").getImplementationVersion
    if (implVersion == null) {
      ("","")
    } else {
		val index = implVersion.indexOf("+")
		if (index > 0 && index < implVersion.length()) {
		  (implVersion.substring(0, index), implVersion.substring(index+1))
		} else {
		  (implVersion, "")
		}
    }
  }
 
    
  def hex2Bytes(hex: String): Array[Byte] = {
    (for { i <- 0 to hex.length - 1 by 2 if i > 0 || !hex.startsWith("0x") }
      yield hex.substring(i, i + 2))
      .map(Integer.parseInt(_, 16).toByte).toArray
  }

  def bytes2Hex(bytes: Array[Byte]): String = {
    def cvtByte(b: Byte): String = {
      (if ((b & 0xff) < 0x10) "0" else "") + java.lang.Long.toString(b & 0xff, 16)
    }

    "0x" + bytes.map(cvtByte(_)).mkString.toUpperCase
  }
  
  def bits2Bytes(bits: String) : Array[Byte] = {
    (for { i <- 0 to bits.length - 1 by 8 }
      yield bits.substring(i, math.min(bits.length, i + 8)))
      .map(Integer.parseInt(_, 2).toByte).toArray
  }
  
  def bytes2Bits(bytes : Array[Byte]) : String = {
    def cvtByte(b : Byte) = {
      val indexes = Stream.range(0, 7)
      val bits = indexes.map{index => (b >> index) & 0x01 }
      bits
    }
    bytes.flatMap{cvtByte(_)}.mkString
  }

  
}