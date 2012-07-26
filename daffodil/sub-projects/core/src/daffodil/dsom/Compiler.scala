package daffodil.dsom

import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import scala.xml.Node
import scala.xml.XML
import daffodil.api.DFDL
import daffodil.exceptions.Assert
import daffodil.util.{ Logging, Info }
import daffodil.util.Misc.hex2Bytes
import junit.framework.Assert.assertEquals

class ProcessorFactory(sset: SchemaSet, rootElem: GlobalElementDecl)
  extends DiagnosticsProviding // (sset)
  with DFDL.ProcessorFactory {

  lazy val prettyName = "ProcessorFactory"
  lazy val path = ""
  lazy val diagnosticChildren = List(sset)
  // println("Creating Processor Factory")

  def onPath(xpath: String): DFDL.DataProcessor = {
    Assert.invariant(canProceed)
    Assert.notYetImplemented(xpath != "/")
    lazy val dp = new DataProcessor(this, rootElem)
    dp
  }
}

class Compiler extends DFDL.Compiler with Logging {
  var root: String = ""
  var rootNamespace: String = ""
  var debugMode = false

  def setDistinguishedRootNode(name: String, namespace: String = ""): Unit = {
    root = name
    rootNamespace = namespace
  }

  def setExternalDFDLVariable(name: String, namespace: String, value: String): Unit = {
    Assert.notYetImplemented()
  }

  def setDebugging(flag: Boolean) {
    debugMode = flag
  }

  /**
   * Controls whether we check everything in the schema, or just the element
   * we care about (and everything reachable from it.)
   *
   * You need this control, since many of the big TDML test files have many things
   * in them, some of which use unimplemented features. Each time we run exactly one
   * test from the set, we want to ignore errors in compilation of the others.
   */
  private var checkEverything = false
  def setCheckEverything(flag: Boolean) {
    checkEverything = flag
  }

  /*
   * for unit testing of front end
   */
  private[dsom] def frontEnd(xml: Node): (SchemaSet, GlobalElementDecl) = {
    // LoggingDefaults.setLoggingLevel(LogLevel.Debug)
    val elts = (xml \ "element")
    Assert.usage(elts.length != 0, "No top level element declarations found.")

    if (root == "") {
      Assert.invariant(rootNamespace == "")
      // TODO: when we generalize to multiple files, this won't work any more
      val eltLabels = (xml \ "element").map { eNode => (eNode \ "@name").text }
      Assert.usage(eltLabels.length > 0)
      root = eltLabels(0)
    }

    if (rootNamespace == "") {
      rootNamespace = (xml \ "@targetNamespace").text
    }

    val sset = if (checkEverything) {
      new SchemaSet(List(xml))
    } else {
      new SchemaSet(List(xml), rootNamespace, root)
    }
    val maybeRoot = sset.getGlobalElementDecl(rootNamespace, root)
    val res = maybeRoot match {
      case None => Assert.usageError("The document element named " + root + " was not found.")
      case Some(rootElemFactory) => {
        val rootElem = rootElemFactory.forRoot()
        (sset, rootElem)
      }
    }
    res
  }

  def reload(fileNameOfSavedParser: String) = {
    Assert.notYetImplemented()
    //      val sp = daffodil.parser.SchemaParser.readParser(fileNameOfSavedParser)
    //      backEnd(sp, Assert.notYetImplemented())
  }

  def compile(schemaFileName: String): DFDL.ProcessorFactory = {
    val schemaNode = XML.load(schemaFileName)
    compile(schemaNode)
  }

  def compile(xml: Node): DFDL.ProcessorFactory = {
    val (sset, rootElem) = frontEnd(xml) // includes middle "end" too.
    // 	 lazy val documentProd = rootElem.document
    //   lazy val parser = documentProd.parser
    //   lazy val unparser = documentProd.unparser
    lazy val pf = new ProcessorFactory(sset, rootElem)
    if (pf.isError) {
      val diags = pf.getDiagnostics
      Assert.invariant(diags.length > 0)
      log(Info("Compilation (ProcessorFactory) produced %d errors/warnings.", diags.length))
      diags.foreach { diag => log(daffodil.util.Error(diag.toString())) }
    } else {
      log(Info("ProcessorFactory completed with no errors."))
      val dataProc = pf.onPath("/").asInstanceOf[DataProcessor]
      if (dataProc.isError) {
        val diags = dataProc.getDiagnostics
        log(Info("Compilation (DataProcessor) reports %s compile errors/warnings.", diags.length))
        diags.foreach { diag => log(daffodil.util.Error(diag.toString())) }
      } else {
        log(Info("Parser = %s.", dataProc.parser.toString))
        log(Info("Unparser = %s.", dataProc.unparser.toString))
        log(Info("Compilation completed with no errors."))
      }
    }
    pf
  }
}

object Compiler {
  def apply() = new Compiler()

  def stringToReadableByteChannel(s: String) = {
    val bytes = s.getBytes()
    byteArrayToReadableByteChannel(bytes)
  }

  def stringToWritableByteChannel(s: String) = {
    val size = s.length() //TODO: get byte count by encoding
    byteArrayToWritableByteChannel(size)
  }

  def byteArrayToReadableByteChannel(bytes: Array[Byte]) = {
    val inputStream = new ByteArrayInputStream(bytes);
    val rbc = java.nio.channels.Channels.newChannel(inputStream);
    rbc
  }

  def byteArrayToWritableByteChannel(size: Int) = {
    val outputStream = new ByteArrayOutputStream(size);
    val wbc = java.nio.channels.Channels.newChannel(outputStream);
    wbc
  }

  def fileToReadableByteChannel(file: java.io.File) = {
    val inputStream = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(inputStream);
    rbc
  }

  def testString(testSchema: Node, data: String) = {
    val compiler = Compiler()
    val pf = compiler.compile(testSchema)
    if (pf.isError) {
      val msgs = pf.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val p = pf.onPath("/")
    if (p.isError) {
      val msgs = p.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    val d = Compiler.stringToReadableByteChannel(data)
    val actual = p.parse(d)
    if (actual.isError) {
      val msgs = actual.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    actual
  }

  def testBinary(testSchema: Node, hexData: String) = {
    val compiler = Compiler()
    val pf = compiler.compile(testSchema)
    val p = pf.onPath("/")
    val b = hex2Bytes(hexData)
    val rbc = byteArrayToReadableByteChannel(b)
    val actual = p.parse(rbc)
    if (actual.isError) {
      val msgs = actual.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    actual
  }

  def testFile(testSchema: Node, fileName: String) = {
    val compiler = Compiler()
    val pf = compiler.compile(testSchema)
    val p = pf.onPath("/")
    val d = Compiler.fileToReadableByteChannel(new java.io.File(fileName))
    val actual = p.parse(d)
    actual
  }

  def testUnparsing(testSchema: scala.xml.Elem, infoset: Node, unparseTo: String) {
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
    val unparsed = outputStream.toString
    //    System.err.println("parsed: " + infoset)
    //    System.err.println("unparsed: " + unparsed)
    out.close()
    assertEquals(unparseTo, unparsed)
  }
}

