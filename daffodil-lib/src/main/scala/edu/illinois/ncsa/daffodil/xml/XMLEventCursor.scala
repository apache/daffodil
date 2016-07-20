package edu.illinois.ncsa.daffodil.xml

import edu.illinois.ncsa.daffodil.util.Cursor
import edu.illinois.ncsa.daffodil.util.CursorImplMixin
import edu.illinois.ncsa.daffodil.util.Accessor
import edu.illinois.ncsa.daffodil.util.HasCpy
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.util.MStack
import edu.illinois.ncsa.daffodil.equality._

import scala.xml._
import scala.xml.parsing.MarkupHandler
import scala.xml.parsing.MarkupParser
import scala.io.Source
import scala.xml._
import edu.illinois.ncsa.daffodil.util.InvertControl
import scala.xml.parsing.ExternalSources

/**
 * These are mutable for use in Cursor/Accessor pattern, where
 * they're not being allocated and returned, but rather
 * preexisting ones that are part of a Cursor are being filled in.
 */
sealed trait XMLEvent extends HasCpy[XMLEvent] {

  /**
   * This is a general copy for any XMLEvent.
   *
   * So named because we can't overload on return type, so we can't just
   * do `def copy(): XMLEvent` because the auto-generated copy in the case classes
   * won't overload that. So we need our own generic copy that works for any derived
   * case class, and that's why this is `cpy`.
   */
  def cpy(): XMLEvent

  var pos: Int = 0
}

sealed trait XMLElementEvent extends XMLEvent {
  def pre: String
  def label: String
  protected def scope: NamespaceBinding
  final def getNamespaceStringOrNullIfNoNS: String = {
    val nsStr = scope.getURI(pre)
    if (nsStr == NoNamespace.toString || nsStr == "") null else nsStr
  }
}

object XMLEvent {
  case class EvStart(var pre: String, var label: String, var attrs: MetaData, var scope: NamespaceBinding) extends XMLElementEvent {

    override def cpy() = copy()

    def isNil: Boolean = {
      val nilValue1 = attrs(XMLUtils.XSI_NAMESPACE.toString, scope, "nil")
      val nilValue = {
        if (nilValue1 ne null) {
          nilValue1
        } else {
          // this should allocate nothing - produce only empty collections, if there are no nil attributes at all
          // which is the case we care about.
          val nilAttrs = attrs.filter(md => md.isPrefixed && md.key == "nil").filter(md => md.value.text == "true")
          val nilAttrsMap = nilAttrs.asAttrMap
          if (nilAttrsMap.isEmpty) {
            null
          } else {
            // need a diagnostic
            //
            // TODO: make this a warning not an error?
            //
            // there is at least one attribute named 'nil' with a prefix,
            // having value "true", and with the prefix unbound, or bound to an incorrect namespace.
            //
            throw new scala.xml.SAXException(
              "Attribute(s) " + nilAttrs.toString() + " found, but the namespace is incorrect.\n" +
                "It must have a namespace prefix (usually 'xsi') and the prefix must be bound to '" + XMLUtils.XSI_NAMESPACE.toString + "'.")
          }
        }
      }
      if (nilValue eq null) false
      else {
        // make sure it says xsi:nil="true" not xsi:nil="false"
        val txt = nilValue.text
        if (txt =:= "true") true
        else false
      }
    }
  } // end case class EvStart

  case class EvEnd(var pre: String, var label: String, var scope: NamespaceBinding) extends XMLElementEvent {
    override def cpy() = copy()
  }
  case class EvText(var text: String) extends XMLEvent {
    override def cpy() = copy()
  }
  case class EvEntityRef(var entity: String) extends XMLEvent {
    override def cpy() = copy()
  }
  case class EvProcInstr(var target: String, var text: String) extends XMLEvent {
    override def cpy() = copy()
  }
  case class EvComment(var text: String) extends XMLEvent {
    override def cpy() = copy()
  }
}

private[xml] class XMLAccessorParts {
  import XMLEvent._
  val start = EvStart(null, null, null, null)
  val end = EvEnd(null, null, null)
  val text = EvText(null)
  val entityRef = EvEntityRef(null)
  val procInstr = EvProcInstr(null, null)
  val comment = EvComment(null)
}

final class XMLAccessor(private var fields: Maybe[XMLAccessorParts] = Maybe(new XMLAccessorParts)) extends Accessor[XMLAccessor] {
  var event: XMLEvent = null

  def checkReadOnly {
    if (fields.isEmpty) throw new scala.xml.SAXException("Attempt to modify read-only XMLAccessor: " + this)
  }

  override def toString = {
    if (event eq null) super.toString
    else event.toString
  }

  override def cpy(): XMLAccessor = {
    val n = new XMLAccessor(Nope)
    if (this.event != null) {
      n.event = this.event.cpy()
    }
    n
  }

  override def assignFrom(other: XMLAccessor) {
    fields = other.fields
    event = other.event
  }

  def start = fields.get.start
  def end = fields.get.end
  def text = fields.get.text
  def entityRef = fields.get.entityRef
  def procInstr = fields.get.procInstr
  def comment = fields.get.comment

  def init {
    checkReadOnly
    event = null
    start.pos = 0
    start.pre = null
    start.label = null
    start.attrs = null
    start.scope = null
    end.pos = 0
    end.pre = null
    end.label = null
    end.scope = null
    text.pos = 0
    text.text = null
    entityRef.pos = 0
    entityRef.entity = null
    procInstr.target = null
    procInstr.text = null
    comment.pos = 0
    comment.text = null
  }
}

trait XMLEventCursor extends Cursor[XMLAccessor] {
  def getXMLErrorInfo(pos: Int, str: String): String
}

class XMLEventCursorFromInput(
  val input: Source,
  includeProcInstrAndComments: Boolean = false,
  override val advanceAccessor: XMLAccessor = new XMLAccessor,
  override val inspectAccessor: XMLAccessor = new XMLAccessor)
  extends MarkupHandler
  with MarkupParser
  with ExternalSources
  with XMLEventCursor
  with CursorImplMixin[XMLAccessor]
  with InvertControl[XMLAccessor] {

  override def fill: Boolean = {
    if (hasNext) {
      next()
      true
    } else
      false
  }

  private var level: Int = 0

  private val scopeStack = new MStack.Of[NamespaceBinding]

  override def elemStart(pos: Int, pre: String, label: String, attrs: MetaData, scope: NamespaceBinding) {
    level += 1
    scopeStack.push(scope)
    val start = accessor.start
    accessor.event = start
    start.pos = pos
    start.pre = pre
    start.label = label
    start.attrs = attrs
    start.scope = scope
    setNext(accessor)
  }
  override def elemEnd(pos: Int, pre: String, label: String) {
    val scope = scopeStack.pop
    val end = accessor.end
    accessor.event = end
    end.pos = pos
    end.pre = pre
    end.label = label
    end.scope = scope
    level -= 1
    setNext(accessor)
  }

  def text(pos: Int, txt: String): NodeSeq = {
    accessor.event = accessor.text
    accessor.text.pos = pos
    accessor.text.text = txt
    setNext(accessor)
    NodeSeq.Empty
  }

  def entityRef(pos: Int, entity: String): NodeSeq = {
    accessor.event = accessor.entityRef
    accessor.entityRef.pos = pos
    accessor.entityRef.entity = entity
    setNext(accessor)
    NodeSeq.Empty
  }

  override def procInstr(pos: Int, target: String, txt: String): NodeSeq = {
    if (this.includeProcInstrAndComments) {
      accessor.event = accessor.procInstr
      accessor.procInstr.pos = pos
      accessor.procInstr.target = target
      accessor.procInstr.text = txt
      setNext(accessor)
    }
    NodeSeq.Empty
  }

  def comment(pos: Int, txt: String): NodeSeq = {
    if (this.includeProcInstrAndComments) {
      accessor.event = accessor.comment
      accessor.comment.pos = pos
      accessor.comment.text = txt
      setNext(accessor)
    }
    NodeSeq.Empty
  }

  /**
   * We're using InvertControl here just for the co-routine control flow.
   * What's being passed back and forth is NOT the XML event objects, because
   * we're trying to avoid allocating those at all.
   *
   * Just singleton objects indicating request/reply are moving back and forth.
   *
   * The call-back handlers in the other side of the inverted control (the other coroutine)
   * are explicitly filling in the accessors, so the XML information moves by
   * side-effect on the accessors.
   */
  def body {
    curInput = input // this is how you give a MarkupParser access to the data source. Ugh.
    try {
      initialize.document() // and... away we go. This does the SAX parse that starts the calling back.
    } catch {
      case e: scala.xml.SAXException =>
        setFinal(e) // send SAXExceptions over to the consumer side.
      case th: Throwable =>
        throw th // good place for a breakpoint
    } finally {
      if (syntaxErrCount > 0) {
        val e = new scala.xml.SAXException("XML Syntax Errors: " + syntaxErrStream.toString())
        setFinal(e)
      }
    }
  }

  val preserveWS = true
  private var syntaxErrCount = 0
  val syntaxErrStream = new java.io.ByteArrayOutputStream
  private val syntaxErrPrintStream = new java.io.PrintStream(syntaxErrStream)

  override def reportSyntaxError(pos: Int, str: String) {
    syntaxErrCount += 1
    input.reportError(pos, str, syntaxErrPrintStream)
  }

  override def getXMLErrorInfo(pos: Int, str: String) = {
    reportSyntaxError(pos, str)
    syntaxErrPrintStream.flush()
    val errStr = syntaxErrStream.toString()
    errStr
  }

  // this is a dummy to satisfy MarkupHandler's API
  // memory usage optimization return one <ignore/> for top level to satisfy
  // MarkupParser.document() otherwise NodeSeq.Empty
  private var ignoreWritten = false
  final def elem(pos: Int, pre: String, label: String, attrs: MetaData, pscope: NamespaceBinding, empty: Boolean, nodes: NodeSeq): NodeSeq =
    if (level == 1 && !ignoreWritten) { ignoreWritten = true; <ignore/> } else NodeSeq.Empty

}
