package daffodil.compiler

import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import scala.xml.Node
import daffodil.api.DFDL._
import daffodil.exceptions.Assert
import daffodil.util._
import daffodil.util.Misc.hex2Bytes
import junit.framework.Assert.assertEquals
import daffodil.dsom.DiagnosticsProviding
import daffodil.dsom.GlobalElementDecl
import daffodil.dsom.SchemaSet
import daffodil.processors.DataProcessor
import daffodil.api.DFDL
import daffodil.debugger.Debugger
import daffodil.xml.DaffodilXMLLoader
import daffodil.xml.XMLUtils
import java.io.File
import daffodil.xml.NS

/**
 * Contains a specification of the root element to be used.
 *
 * The whole RootSpec is generally optional, but if you have one,
 * the namespace part of it is optional as well.
 *
 * When the namespace part is None, it means "you, daffodil, figure out the namespace".
 * Which it will do so long as it is unambiguous.
 */
case class RootSpec(ns: Option[NS], name: String) {
  override def toString() = {
    val nsStr = ns.getOrElse("")
    "{" + nsStr + "}" + name
  }
}

class ProcessorFactory(sset: SchemaSet)
  extends DiagnosticsProviding // (sset)
  with DFDL.ProcessorFactory
  with HavingRootSpec {

  lazy val prettyName = "ProcessorFactory"
  lazy val path = prettyName

  // println("Creating Processor Factory")
  lazy val rootElem = rootElem_.value
  private lazy val rootElem_ = LV('rootELem) { sset.rootElement(rootSpec) }

  lazy val diagnosticChildren: DiagnosticsList = List(rootElem, sset) // order may matter as to error msg order.

  def onPath(xpath: String): DFDL.DataProcessor = {
    Assert.usage(canProceed)
    Assert.notYetImplemented(xpath != "/")
    val dataProc = new DataProcessor(this, rootElem)
    if (dataProc.isError) {
      val diags = dataProc.getDiagnostics
      log(Error("Compilation (DataProcessor) reports %s compile errors/warnings.", diags.length))
      diags.foreach { diag => log(daffodil.util.Error(diag.toString())) }
    } else {
      log(Compile("Parser = %s.", dataProc.parser.toString))
      log(Compile("Unparser = %s.", dataProc.unparser.toString))
      log(Compile("Compilation (DataProcesor) completed with no errors."))
    }
    dataProc
  }

}

/**
 * Both Compiler and ProcessorFactory share this same API call.
 */
trait HavingRootSpec {
  var rootSpec: Option[RootSpec] = None

  def setDistinguishedRootNode(name: String, namespace: String): Unit = {

    val ns =
      if (namespace != null) Some(NS(namespace))
      else None
    rootSpec = Some(RootSpec(ns, name))
    //
    // null means we search for the namespace
    // Must be only one answer.
    //

  }
}

class Compiler extends DFDL.Compiler with Logging with HavingRootSpec {

  def setExternalDFDLVariable(name: String, namespace: String, value: String): Unit = {
    Assert.notYetImplemented()
  }

  def setDebugging(flag: Boolean) {
    Debugger.setDebugging(flag)
  }

  /**
   * Controls whether we check everything in the schema, or just the element
   * we care about (and everything reachable from it.)
   *
   * You need this control, since many of the big TDML test files have many things
   * in them, some of which use unimplemented features. Each time we run exactly one
   * test from the set, we want to ignore errors in compilation of the others.
   */
  private var checkAllTopLevel = false
  def setCheckAllTopLevel(flag: Boolean) {
    checkAllTopLevel = flag
  }

  /*
   * for unit testing of front end
   */
  def frontEnd(xml: Node): (SchemaSet, GlobalElementDecl) = {
    val (sset, pf) = compileInternal(xml)
    val ge = pf.rootElem
    (sset, ge)
  }

  def reload(fileNameOfSavedParser: String): DFDL.ProcessorFactory = {
    Assert.notYetImplemented()
  }

  /**
   * Compilation works entirely off of schema files because that allows XMLCatalogs
   * to work for Xerces without (much) pain.
   *
   * This method exposes both the schema set and processor factory as results because
   * our tests often want to do things on the schema set.
   */
  def compileInternal(schemaFileNames: Seq[String]): (SchemaSet, ProcessorFactory) = {
    Assert.usage(schemaFileNames.length >= 1)
    val sset = new SchemaSet(schemaFileNames, rootSpec, checkAllTopLevel)
    val pf = new ProcessorFactory(sset)
    val isError = pf.isError // isError causes diagnostics to be created.
    val diags = pf.getDiagnostics
    def printDiags() = diags.foreach { diag => log(daffodil.util.Error(diag.toString())) }
    if (pf.isError) {
      Assert.invariant(diags.length > 0)
      log(Error("Compilation (ProcessorFactory) produced %d errors/warnings.", diags.length))
      printDiags()
    } else {
      if (diags.length > 0) {
        System.err.println("Compilation (ProcessorFactory) produced %d warnings: " + diags.length)
        printDiags()
      } else {
        log(Compile("ProcessorFactory completed with no errors."))
      }
    }
    (sset, pf)
  }

  /**
   * Just hides the schema set, and returns the processor factory only.
   */
  def compile(fNames: String*): DFDL.ProcessorFactory = compileInternal(fNames)._2

  /**
   * For convenient unit testing allow a literal XML node.
   */
  def compile(xml: Node) = {
    compileInternal(xml)._2
  }

  def compileInternal(xml: Node): (SchemaSet, ProcessorFactory) = {
    val tempSchemaFile = daffodil.xml.XMLUtils.convertNodeToTempFile(xml)
    compileInternal(List(tempSchemaFile))
  }

}

/**
 * Factory for Compiler instances
 *
 * Size and length limit constants used by the code, some of which will be tunable
 * by the user. Turning them to lower sizes/lengths may improve performance and
 * diagnostic behavior when a format does not need their full range,
 * both by reducing memory footprint, but
 * also by reducing the amount of time taken to scan to the end of what is allowed
 * and fail (and backtrack to try something else) when, for an example, a delimiter
 * is missing from the data.
 *
 * Also has many convenience methods for common test scenarios.
 */
object Compiler {

  //TODO: make tunable via setter call of compiler
  def maxFieldContentLengthInBytes: Long = 1024 // Can be as large as Int.MaxValue
  def occursCountMax: Long = 1024 // Can be as large as Int.MaxValue 
  def maxSkipLength: Long = 1024 // applicable to leadingSkip and trailingSkip
  // TODO: want to lift limit of Int.MaxValue, since these are supposed to be Long integers.
  def readerByteBufferSize: Long = 8192
  def generatedNamespacePrefixStem = "tns"

  def apply() = new Compiler()

  def stringToReadableByteChannel(s: String): DFDL.Input = {
    val bytes = s.getBytes("utf-8") // never use default charset. NEVER.
    byteArrayToReadableByteChannel(bytes)
  }

  def stringToWritableByteChannel(s: String): DFDL.Output = {
    val size = s.length() // TODO: get byte count by encoding
    byteArrayToWritableByteChannel(size)
  }

  def byteArrayToReadableByteChannel(bytes: Array[Byte]): DFDL.Input = {
    val inputStream = new ByteArrayInputStream(bytes);
    val rbc = java.nio.channels.Channels.newChannel(inputStream);
    rbc
  }

  def byteArrayToWritableByteChannel(size: Int): DFDL.Output = {
    val outputStream = new ByteArrayOutputStream(size);
    val wbc = java.nio.channels.Channels.newChannel(outputStream);
    wbc
  }

  def fileToReadableByteChannel(file: java.io.File): DFDL.Input = {
    val inputStream = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(inputStream);
    rbc
  }

  def runSchemaOnData(testSchema: Node, data: DFDL.Input) = {
    val compiler = Compiler()
    val pf = compiler.compile(testSchema)
    val isError = pf.isError
    val msgs = pf.getDiagnostics.map(_.getMessage).mkString("\n")
    if (isError) {
      throw new Exception(msgs)
    }
    val p = pf.onPath("/")
    val pIsError = p.isError
    if (pIsError) {
      throw new Exception(msgs)
    }
    val d = data
    val actual = p.parse(d)
    if (actual.isError) {
      val msgs = actual.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    actual
  }

  def testString(testSchema: Node, data: String) = {
    runSchemaOnData(testSchema, Compiler.stringToReadableByteChannel(data))
  }

  def testBinary(testSchema: Node, hexData: String) = {
    val b = hex2Bytes(hexData)
    val rbc = byteArrayToReadableByteChannel(b)
    runSchemaOnData(testSchema, rbc)
  }

  def testFile(testSchema: Node, fileName: String) = {
    runSchemaOnData(testSchema, Compiler.fileToReadableByteChannel(new java.io.File(fileName)))
  }

  def testUnparsing(testSchema: scala.xml.Elem, infoset: Node, unparseTo: String) {
    val compiler = Compiler()
    val pf = compiler.compile(testSchema)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val u = pf.onPath("/")
    if (u.isError) {
      val msgs = u.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val outputStream = new java.io.ByteArrayOutputStream()
    val out = java.nio.channels.Channels.newChannel(outputStream)
    val actual = u.unparse(out, infoset)
    if (actual.isError) {
      val msgs = actual.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val unparsed = outputStream.toString
    //    System.err.println("parsed: " + infoset)
    //    System.err.println("unparsed: " + unparsed)
    out.close()
    assertEquals(unparseTo, unparsed)
  }

  def testUnparsingBinary(testSchema: scala.xml.Elem, infoset: Node, unparseTo: Array[Byte]) {
    val compiler = Compiler()
    val pf = compiler.compile(testSchema)
    val u = pf.onPath("/")
    val outputStream = new java.io.ByteArrayOutputStream()
    val out = java.nio.channels.Channels.newChannel(outputStream)
    val actual = u.unparse(out, infoset)
    if (actual.isError) {
      val msgs = actual.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val unparsed = outputStream.toByteArray()
    //        System.err.println("parsed: " + infoset)
    //        System.err.println("unparsed: " + unparsed)
    out.close()
    assertEquals(unparsed.length, unparseTo.length)
    for (i <- 0 until unparsed.length) {
      assertEquals(unparseTo(i), unparsed(i))
    }
  }
}

