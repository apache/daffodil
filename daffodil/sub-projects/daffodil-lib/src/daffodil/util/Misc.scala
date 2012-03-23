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
    val tokens = obj.getClass().getName().split("[\\$\\.]").toList.reverse
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
   * if we're just running interactively in eclipse, doesn't use the jar.
   */
  def getResourceOrFileStream (fn : String) : InputStream = {
    var is = this.getClass().getResourceAsStream("/" + fn)
    if (is == null) {
      val f = new File(fn)
      if (!f.exists()) {
        val e = new FileNotFoundException(fn)
        throw e
      }
      val abs = f.getAbsolutePath()
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
    val implVersion = Package.getPackage("daffodil.util").getImplementationVersion()
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
  
}