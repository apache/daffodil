package daffodil.dsom
import daffodil.exceptions.Assert
import daffodil.api.DFDL
import daffodil.grammar.PState
import daffodil.grammar.UState
import daffodil.dsom.OOLAG.ErrorAlreadyHandled
import daffodil.xml.XMLUtils
import daffodil.processors.Success
import daffodil.grammar.GeneralUnparseFailure
import daffodil.api.WithDiagnostics
import daffodil.dsom.OOLAG.OOLAGException
import daffodil.grammar.ParseError
import daffodil.grammar.ProcessingError
import daffodil.api.Diagnostic

/**
 * Implementation mixin - provides simple helper methods
 */
trait WithDiagnosticsImpl extends WithDiagnostics {
    
  final lazy val hasDiagnostics = {
    getDiagnostics.size > 0
  }
}

/**
 * The very last aspects of compilation, and the start of the 
 * back-end runtime.
 */
class DataProcessor(pf : ProcessorFactory, rootElem : GlobalElementDecl)
  extends DFDL.DataProcessor 
  with DiagnosticsProviding {
  Assert.usage(pf.canProceed)
  
  lazy val prettyName = "DataProcessor"
  lazy val path = ""
  lazy val diagnosticChildren = List(pf, rootElem, parser_)
  
  //
  // Last tidbits of compilation. Really this is just accessing the 
  // result of compilation
  // 
  lazy val parser = parser_.value 
  private lazy val parser_ = LV {
    rootElem.document.parser
  }

  lazy val unparser = unparser_.value 
  private lazy val unparser_ = LV {
    rootElem.document.unparser
  }
    
  def save(fileName : String) : Unit = {
    Assert.notYetImplemented()
  }

  /**
   * Here begins the parser runtime. Compiler-oriented mechanisms (OOLAG etc.) aren't used in the 
   * runtime. Instead we deal with success and failure statuses. 
   */
  def parse(input : DFDL.Input) : DFDL.ParseResult = {
    Assert.usage(!this.isError)
    
    val initialState = PState.createInitialState(rootElem, input) // TODO also want to pass here the externally set variables, other flags/settings.
    val pr = new ParseResult(this) {
      
      lazy val resultState = {
        try {
          parser.parse(initialState)
        } catch {
          // technically, runtime shouldn't throw. It's really too heavyweight a construct. And "failure" 
          // when parsing isn't exceptional, it's routine behavior. So ought not be implemented via an 
          // exception handling construct.
          //
          // But we might not catch everything inside...
          //
          case pe : ParseError => {
            // if we get one here, then someone threw instead of returning a status. 
            Assert.invariantFailed("ParseError caught. ParseErrors should be returned as failed status, not thrown. Fix please.")
          }
          case procErr : ProcessingError => {
            Assert.invariantFailed("got a processing error that was not a parse error. This is the parser!")
          }
          case sde : SchemaDefinitionError => {
            // A SDE was detected at runtime (perhaps due to a runtime-valued property like byteOrder or encoding)
            // These are fatal, and there's no notion of backtracking them, so they propagate to top level
            // here.
            initialState.failed(sde)
          }
          case e : OOLAGException => {
            Assert.invariantFailed("OOLAGException's like " + e + " are compiler stuff. This is runtime.")
          }
        }
      }
    }
    pr
  }

  def unparse(output : DFDL.Output, node : scala.xml.Node) : DFDL.UnparseResult = {
    val jdomElem = XMLUtils.elem2Element(node)
    val jdomDoc = new org.jdom.Document(jdomElem)
    val initialState = UState.createInitialState(rootElem, output, jdomDoc) // also want to pass here the externally set variables, other flags/settings.
    val resultState = unparser.unparse(initialState)
    //write unparsed result to outputStream
    resultState.outStream.write()
    val res = new UnparseResult(resultState)
    res
  }
}

abstract class ParseResult(dp : DataProcessor) 
  extends DFDL.ParseResult 
  with WithDiagnosticsImpl {

  def resultState : PState
  
//  lazy val prettyName = "ParseResult"
//  lazy val path = ""
  
  var diagnostics : List[Diagnostic] = Nil
  
  def getDiagnostics = {
    diagnostics ++ resultState.diagnostics
  }
  
  def addDiagnostic(d : Diagnostic) {
    diagnostics = d +: diagnostics
  }
    
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
  
  lazy val isError = resultState.status != Success
}

class UnparseResult(resultState: UState)
  extends DiagnosticsProviding // DelegatesDiagnostics(dp)
  with DFDL.UnparseResult {

  lazy val diagnosticChildren = Nil
  lazy val prettyName = "UnparseResult"
  lazy val path = ""

  override lazy val isError = resultState.status != Success
  override lazy val getLocalDiagnostics = resultState.diagnostics
}

