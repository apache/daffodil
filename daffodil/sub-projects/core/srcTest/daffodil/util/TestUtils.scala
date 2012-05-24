package daffodil.util

import scala.xml._
import daffodil.xml.XMLUtils
import daffodil.xml.XMLUtils._
import junit.framework.Assert.assertEquals
import java.io.File
import java.io.FileNotFoundException

/*
 * This is not a file of tests.
 * 
 * These are utilities to support unit testing schemas
 */
object TestUtils {

  /**
   * utility to create test schemas without having to repeat all the namespace definitions,
   * and the appinfo boilerplate. This makes tests more uniform.
   * 
   * Defines standard named formats that can be re-used by tests. This helps with the issue
   * of having to add properties everywhere when a new property starts being expected.
   */
  def dfdlTestSchema(topLevelAnnotations: Seq[Node], contentElements: Seq[Node]) = {
    val realSchema = <xs:schema xmlns:xs={ xsdURI } xmlns:dfdl={ dfdlURI } xmlns:xsi={ xsiURI } xmlns={ targetNS } xmlns:tns = { targetNS } targetNamespace={ targetNS }>
                       <xs:annotation>
                         <xs:appinfo source={ dfdlURI }>
                           <dfdl:defineFormat name="daffodilTest1">
                             <dfdl:format lengthKind="implicit" representation="text" lengthUnits="bytes" encoding="US-ASCII" initiator="" terminator="" separator="" ignoreCase="no" textNumberRep="standard" />
                           </dfdl:defineFormat>
                           { topLevelAnnotations }
                         </xs:appinfo>
                       </xs:annotation>
                       { contentElements }
                     </xs:schema>
    val realSchemaText = realSchema.toString()
    val real = XML.loadString(realSchemaText)
    real
  }

  /**
   * Compares two XML Elements, after having stripped off all attributes.
   * 
   * TODO: we might start using xsi:type attributes at some point. If so fix this to 
   * save that attribute.
   *
   * NOTE: Has Side Effects: strips off attributes
   */
  def assertEqualsXMLElements(expected : Node, actual : Node) {
    val exp = XMLUtils.removeAttributes(expected)
    val act = XMLUtils.removeAttributes(actual)
    assertEquals(exp, act)
  }


  /**
   * We want to be able to run tests from Eclipse or from batch builds that
   * are rooted in a different directory, so, since Java/JVMs don't have a notion
   * of setting the current directory to a specific value for interpreting things,
   * we have to do that ourselves manually like this.
   * 
   * When you specify a file for use in a test, you want to specify it 
   * relative to the root of the sub-project of which it is part. I.e., within core,
   * the file you specify should be relative to daffodil/sub-projects/core.
   *
   * Returns null if the file cannot be found.
   */
  def findFile(fn : String) : File = findFile(new File(fn))
  def findFile(f : File) : File = {
    if (f.exists()) return f
    // let's figure out where paths are defaulting to....
    val cwd = new File("").getAbsolutePath
    if (cwd.endsWith("daffodil" + File.separator + "sub-projects" + File.separator + "core")) {
        // we're rooted in the core module
        // not sure why the path wouldn't be right from here, this is where our 
        // tdml relative paths are supposed to be relative to.
        //
        // We could try stripping prefixes off of f's path,...
        // For now we'll just give up.
        System.err.println("Couldn't find file " + f + " relative to " + cwd + ".")
        return null
    }
    else if (cwd.endsWith("daffodil")) {
        // I think we're rooted at the overall daffodil project root.
        // Let's check
        val coreRelativePath = cwd + File.separator + "sub-projects" + File.separator + "core" + File.separator + f.getPath
        val coreF = new File(coreRelativePath)
        if (coreF.exists()) return coreF
    }
    else if (new File(cwd + File.separator + "daffodil" + File.separator).exists()) {
        // We're in the parent of the overall daffodil project root (typical for Intellij)
        val coreRelativePath = cwd + File.separator + "daffodil" + File.separator + "sub-projects" + File.separator + "core" + File.separator + f.getPath
        val coreF = new File(coreRelativePath)
        if (coreF.exists()) return coreF
    }
    else {
        System.err.println("Couldn't find file " + f + " relative to " + cwd + ".")
    }
    // throw new FileNotFoundException("Couldn't find file " + f + " relative to " + cwd + ".")
    null
  }


}