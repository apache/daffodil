//package edu.illinois.ncsa.daffodil.xml
//
//import scala.xml.parsing.MarkupHandler
//import scala.xml.parsing.MarkupParser
//import scala.io.Source
//import scala.xml._
//import scala.xml.pull.XMLEvent
//import edu.illinois.ncsa.daffodil.util.InvertControl
//import scala.xml.parsing.ExternalSources
//
///**
// * Our own XMLPullParser.
// *
// * Uses our own Coroutines library which provides for a zero-concurrency
// * implementation, which is important when libraries being used in this manner
// * aren't thread safe.
// */
//final class XMLPullParser(val input: Source) extends MarkupHandler with MarkupParser with ExternalSources with Iterator[XMLEvent] {
//
//  import scala.xml.pull._
//
//  val preserveWS = true
//  // track level for elem memory usage optimization
//  private var level = 0
//
//  // this is Parser's way to add to the queue - the odd return type
//  // is to conform to MarkupHandler's interface
//  def setEvent(es: XMLEvent*): NodeSeq = {
//    es foreach {
//      ev => iter.setNext(ev)
//    }
//    NodeSeq.Empty
//  }
//
//  private var syntaxErrCount = 0
//  val syntaxErrStream = new java.io.ByteArrayOutputStream
//  private val syntaxErrPrintStream = new java.io.PrintStream(syntaxErrStream)
//
//  override def reportSyntaxError(pos: Int, str: String) {
//    syntaxErrCount += 1
//    input.reportError(pos, str, syntaxErrPrintStream)
//  }
//
//  override def elemStart(pos: Int, pre: String, label: String, attrs: MetaData, scope: NamespaceBinding) {
//    level += 1
//    setEvent(EvElemStart(pre, label, attrs, scope))
//  }
//  override def elemEnd(pos: Int, pre: String, label: String) {
//    setEvent(EvElemEnd(pre, label))
//    level -= 1
//  }
//
//  // this is a dummy to satisfy MarkupHandler's API
//  // memory usage optimization return one <ignore/> for top level to satisfy
//  // MarkupParser.document() otherwise NodeSeq.Empty
//  private var ignoreWritten = false
//  final def elem(pos: Int, pre: String, label: String, attrs: MetaData, pscope: NamespaceBinding, empty: Boolean, nodes: NodeSeq): NodeSeq =
//    if (level == 1 && !ignoreWritten) { ignoreWritten = true; <ignore/> } else NodeSeq.Empty
//
//  def procInstr(pos: Int, target: String, txt: String) = setEvent(EvProcInstr(target, txt))
//  def comment(pos: Int, txt: String) = setEvent(EvComment(txt))
//  def entityRef(pos: Int, n: String) = setEvent(EvEntityRef(n))
//  def text(pos: Int, txt: String) = setEvent(EvText(txt))
//
//  private lazy val iter = new InvertControl[XMLEvent]({
//    curInput = input
//    this.initialize.document()
//    if (syntaxErrCount > 0) {
//      throw new scala.xml.SAXException("XML Syntax Errors: " + this.syntaxErrStream.toString())
//    }
//  })
//
//  override final def hasNext = iter.hasNext
//  override final def next = iter.next
//
//}
