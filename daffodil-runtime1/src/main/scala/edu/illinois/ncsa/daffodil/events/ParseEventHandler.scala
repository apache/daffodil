package edu.illinois.ncsa.daffodil.events

import edu.illinois.ncsa.daffodil.processors.PState
import edu.illinois.ncsa.daffodil.processors.Parser
import edu.illinois.ncsa.daffodil.processors.unparsers.UState
import edu.illinois.ncsa.daffodil.processors.unparsers.Unparser

/**
 * The Daffodil parser generates events as the parse is performed.
 *
 * These events can be used to synthesize output as XML or JSON
 * or they can be used to stop the parse at a breakpoint.
 */

trait EventHandler {

  /**
   * Parser Events
   */
  def init(processor: Parser) {}

  def before(state: PState, processor: Parser) {}

  def after(after: PState, processor: Parser) {}

  def beforeRepetition(state: PState, processor: Parser) {}

  def afterRepetition(after: PState, processor: Parser) {}

  def startElement(state: PState, processor: Parser) {}

  def endElement(state: PState, processor: Parser) {}

  def startArray(state: PState, processor: Parser) {}

  def endArray(state: PState, processor: Parser) {}

  def fini(processor: Parser) {}

  /**
   * Unparser Events
   */
  def init(processor: Unparser) {}

  def before(state: UState, processor: Unparser) {}

  def after(after: UState, processor: Unparser) {}

  def beforeRepetition(state: UState, processor: Unparser) {}

  def afterRepetition(after: UState, processor: Unparser) {}

  def startElement(state: UState, processor: Unparser) {}

  def endElement(state: UState, processor: Unparser) {}

  def startArray(state: UState, processor: Unparser) {}

  def endArray(state: UState, processor: Unparser) {}

  def fini(processor: Unparser) {}
}

/**
 * Allow for more than one event handler with this meta-handler.
 *
 * This makes for an awkward call like pstate.before(pstate, this)
 * But by making this implement EventHandler we can never forget to
 * properly hook up a new event call that gets added to EventHandler
 */
trait MultipleEventHandler extends EventHandler with Serializable {

  @transient protected final var handlers_ : Seq[EventHandler] = Nil

  final def handlers = {
    if (handlers_ == null) handlers_ = Nil
    handlers_
  }

  final def addEventHandler(h: EventHandler) {
    if (!handlers.contains(h))
      handlers_ = h +: handlers
  }

  /**
   * Parser Events
   */
  override def init(processor: Parser) { if (!(handlers eq Nil)) handlers.foreach { _.init(processor) } }

  override def before(state: PState, processor: Parser) { if (!(handlers eq Nil)) handlers.foreach { _.before(state, processor) } }

  override def after(state: PState, processor: Parser) { if (!(handlers eq Nil)) handlers.foreach { _.after(state, processor) } }

  override def beforeRepetition(state: PState, processor: Parser) { if (!(handlers eq Nil)) handlers.foreach { _.beforeRepetition(state, processor) } }

  override def afterRepetition(state: PState, processor: Parser) { if (!(handlers eq Nil)) handlers.foreach { _.afterRepetition(state, processor) } }

  override def startElement(state: PState, processor: Parser) {
    if (!(handlers eq Nil)) handlers.foreach {
      _.startElement(state, processor)
    }
  }

  override def endElement(state: PState, processor: Parser) {
    if (!(handlers eq Nil)) handlers.foreach {
      _.endElement(state, processor)
    }
  }

  override def startArray(state: PState, processor: Parser) { if (!(handlers eq Nil)) handlers.foreach { _.startArray(state, processor) } }

  override def endArray(state: PState, processor: Parser) { if (!(handlers eq Nil)) handlers.foreach { _.endArray(state, processor) } }

  override def fini(processor: Parser) { if (!(handlers eq Nil)) handlers.foreach { _.fini(processor) } }

  /**
   * Unparser Events
   */
  override def init(processor: Unparser) { if (!(handlers eq Nil)) handlers.foreach { _.init(processor) } }

  override def before(state: UState, processor: Unparser) { if (!(handlers eq Nil)) handlers.foreach { _.before(state, processor) } }

  override def after(state: UState, processor: Unparser) { if (!(handlers eq Nil)) handlers.foreach { _.after(state, processor) } }

  override def beforeRepetition(state: UState, processor: Unparser) { if (!(handlers eq Nil)) handlers.foreach { _.beforeRepetition(state, processor) } }

  override def afterRepetition(state: UState, processor: Unparser) { if (!(handlers eq Nil)) handlers.foreach { _.afterRepetition(state, processor) } }

  override def startElement(state: UState, processor: Unparser) { if (!(handlers eq Nil)) handlers.foreach { _.startElement(state, processor) } }

  override def endElement(state: UState, processor: Unparser) { if (!(handlers eq Nil)) handlers.foreach { _.endElement(state, processor) } }

  override def startArray(state: UState, processor: Unparser) { if (!(handlers eq Nil)) handlers.foreach { _.startArray(state, processor) } }

  override def endArray(state: UState, processor: Unparser) { if (!(handlers eq Nil)) handlers.foreach { _.endArray(state, processor) } }

  override def fini(processor: Unparser) { if (!(handlers eq Nil)) handlers.foreach { _.fini(processor) } }
}
