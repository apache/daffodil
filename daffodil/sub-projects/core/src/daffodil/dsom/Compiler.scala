package daffodil.dsom

import java.io._
import scala.xml.Node
import scala.xml.XML
import daffodil.api.DFDL
import daffodil.exceptions._
import daffodil.util.Validator
import daffodil.xml.XMLUtils
import daffodil.grammar._
import daffodil.processors._
import daffodil.util.Misc._
import daffodil.api.Diagnostic
import daffodil.util.Misc
import daffodil.api.WithDiagnostics
import daffodil.util.Logging
import daffodil.util.Info

class ProcessorFactory(sset : SchemaSet, rootElem : GlobalElementDecl)
  extends DiagnosticsProviding // (sset)
  with DFDL.ProcessorFactory {

  lazy val prettyName = "ProcessorFactory"
  lazy val path =""
  lazy val diagnosticChildren = List(sset)
  // println("Creating Processor Factory")

  def onPath(xpath : String) : DFDL.DataProcessor = {
    Assert.invariant(canProceed)
    Assert.notYetImplemented(xpath != "/")
    lazy val dp = new DataProcessor(this, rootElem)
    dp
  }
}

class DataProcessor(pf : ProcessorFactory, rootElem : GlobalElementDecl)
  extends DiagnosticsProviding // DelegatesDiagnostics(pf)
  with DFDL.DataProcessor {
  Assert.invariant(pf.canProceed)

  lazy val prettyName = "DataProcessor"
  lazy val path = ""
  lazy val diagnosticChildren = List(pf, rootElem)
  lazy val parser = rootElem.document.parser
  
  def save(fileName : String) : Unit = {
    Assert.notYetImplemented()
  }

  def parse(input : DFDL.Input) : DFDL.ParseResult = {
    val initialState = PState.createInitialState(rootElem, input) // also want to pass here the externally set variables, other flags/settings.
    val resultState = parser.parse(initialState)
    val pr = new ParseResult(resultState, this)
    pr
  }

  def unparse(output : DFDL.Output, node : scala.xml.Node) : DFDL.UnparseResult = {
    val jdomElem = XMLUtils.elem2Element(node)
    val jdomDoc = new org.jdom.Document(jdomElem)
    val res = new UnparseResult 
    res
  }
}

class ParseResult(resultState : PState, dp : DataProcessor)
  extends DiagnosticsProviding // DelegatesDiagnostics(dp)
  with DFDL.ParseResult {
  
  lazy val diagnosticChildren = Nil
  lazy val prettyName = "ParseResult"
  lazy val path = ""
    
  val result =
    if (resultState.status == Success) {
      val jdomFakeRoot = resultState.parent
      // top node is this fake root element
      Assert.invariant(jdomFakeRoot.getName() == "_document_")
      Assert.invariant(jdomFakeRoot.getContentSize() == 1)
      val jdomElt = jdomFakeRoot.getContent(0).asInstanceOf[org.jdom.Element]
      XMLUtils.element2Elem(jdomElt)
    } else {
      <nothing/>
    }
  
  override lazy val isError = resultState.status != Success
  override lazy val getLocalDiagnostics = resultState.diagnostics 
  
}

class UnparseResult
  extends DFDL.UnparseResult {
  lazy val hasDiagnostics = true
  lazy val isError = true
  override lazy val getDiagnostics = Seq(new GeneralUnparseFailure("Unparsing is not yet implemented."))
}

class Compiler extends DFDL.Compiler with Logging {
  var root : String = ""
  var rootNamespace : String = ""
  var debugMode = false

  def setDistinguishedRootNode(name : String, namespace : String = "") : Unit = {
    root = name
    rootNamespace = namespace
  }

  def setExternalDFDLVariable(name : String, namespace : String, value : String) : Unit = {
    Assert.notYetImplemented()
  }

  def setDebugging(flag : Boolean) {
    debugMode = flag
  }

  /*
   * for unit testing of front end
   */
  private[dsom] def frontEnd(xml : Node) : (SchemaSet, GlobalElementDecl) = {
    
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

    val sset = new SchemaSet(List(xml))
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

  def reload(fileNameOfSavedParser : String) = {
    Assert.notYetImplemented()
    //      val sp = daffodil.parser.SchemaParser.readParser(fileNameOfSavedParser)
    //      backEnd(sp, Assert.notYetImplemented())
  }

  def compile(schemaFileName : String) : DFDL.ProcessorFactory = {
    val schemaNode = XML.load(schemaFileName)
    compile(schemaNode)
  }

  def compile(xml : Node) : DFDL.ProcessorFactory = {
	val (sset, rootElem) = frontEnd(xml) // includes middle "end" too.
// 	 lazy val documentProd = rootElem.document
//   lazy val parser = documentProd.parser
//   lazy val unparser = documentProd.unparser
     lazy val pf = new ProcessorFactory(sset, rootElem)
	 if (pf.isError) {
	   val diags = pf.getDiagnostics
		 log(Info("Compilation produced %d errors.", diags.length))
		 diags.foreach{System.out.println(_)}
	 } else {
	    log(Info("Compilation completed with no errors."))
	    val dataProc = pf.onPath("/").asInstanceOf[DataProcessor]
	    log(Info("Parser = %s.", dataProc.parser.toString))
	 }
     pf
  }

}

object Compiler {

  def apply() = new Compiler()

  def stringToReadableByteChannel(s : String) = {
    val bytes = s.getBytes()
    byteArrayToReadableByteChannel(bytes)
  }

  def byteArrayToReadableByteChannel(bytes : Array[Byte]) = {
    val inputStream = new ByteArrayInputStream(bytes);
    val rbc = java.nio.channels.Channels.newChannel(inputStream);
    rbc
  }

  def fileToReadableByteChannel(file : java.io.File) = {
    val inputStream = new java.io.FileInputStream(file)
    val rbc = java.nio.channels.Channels.newChannel(inputStream);
    rbc
  }

  def testString(testSchema : Node, data : String) = {
    val compiler = Compiler()
    val pf = compiler.compile(testSchema)
    val p = pf.onPath("/")
    val d = Compiler.stringToReadableByteChannel(data)
    val actual = p.parse(d)
    if (actual.isError) {
      val msgs = actual.getDiagnostics.map(_.getMessage).mkString("\n")
      throw new Exception(msgs)
    }
    actual
  }

  def testBinary(testSchema : Node, hexData : String) = {
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

  def testFile(testSchema : Node, fileName : String) = {
    val compiler = Compiler()
    val pf = compiler.compile(testSchema)
    val p = pf.onPath("/")
    val d = Compiler.fileToReadableByteChannel(new java.io.File(fileName))
    val actual = p.parse(d)
    actual
  }
}

