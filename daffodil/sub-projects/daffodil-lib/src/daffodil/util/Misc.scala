package daffodil.util

import java.io.FileNotFoundException
import java.io.FileInputStream
import java.io.InputStream
import java.io.File

/**
 * Various reusable utilities that I couldn't easily find a better place for.
 */
object Misc {
  
  def getNameFromClass(obj : Object) : String = {
    if (obj == null) return "null"
    val hexHash = obj.hashCode.formatted("%x")
    val tokens = obj.getClass.getName.split("[\\$\\.]").toList.reverse
    val Some(nameToken) = tokens.find{_.matches("""\p{Alpha}\w*""")}
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
   */
  def getResourceOrFileStream (fn : String) : InputStream = {
    var is = this.getClass.getResourceAsStream(File.separator + fn) // we want the file path separation character. Otherwise it uses the class's package name as part of path.
    if (is == null) {
      var f = new File(fn)
      if (!f.exists()) {
        f = new File (".." + File.separator + "daffodil-lib" + File.separator + fn)                    // For Eclipse
        if (!f.exists()) {
          f = new File ("daffodil" + File.separator + "sub-projects" + File.separator + "daffodil-lib" + File.separator + fn)      // For Intellij
          if (!f.exists()) {
            System.out.println(f.getAbsolutePath)
            val e = new FileNotFoundException(fn)
            throw e
          }
        }
      }
      val abs = f.getAbsolutePath
      is = new FileInputStream(abs)
    }
    return is
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

  
}