package edu.illinois.ncsa.daffodil.processors.unparsers

import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.processors._
import scala.collection.mutable
import edu.illinois.ncsa.daffodil.xml.NamedQName
import edu.illinois.ncsa.daffodil.util.IteratorWithPeekImpl
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.util.MStack
import edu.illinois.ncsa.daffodil.util.IteratorWithPeek

/**
 * Takes an infoset by way of a Infoset Source, and augments it with defaultable
 * and computed elements.
 *
 * Stateful, not thread safe. One per thread.
 *
 * TODO: PERFORMANCE: This could be combined with synthesizing the infoset event stream from the XML events, at the cost
 * of then having different code paths when an infoset comes from XML and when an infoset is constructed
 * first and then walked as a tree to generate the infoset events.
 *
 * Advantage of this whole layer that
 * augments the infoset and infoset events is that it doesn't care if the infoset came from XML events
 * or it came from a tree walk of an Infoset tree.
 */
class UnparserAugmentingInfosetSource(
  rootERD: ElementRuntimeData,
  initialUnaugmentedSource: InfosetSource) extends InfosetSource {

  //
  // Mutable state
  //
  private val arrayIndexStack = new MStack.OfInt
  arrayIndexStack.push(0)

  private def arrayPos0b = arrayIndexStack.top

  private def advanceArrayPos = {
    val top = arrayIndexStack.top
    arrayIndexStack.pop
    arrayIndexStack.push(top + 1)
  }

  /**
   * Augmenting inserts events between the anchor and the indicator. This method
   * fills a buffer with a few events that are the augmentation events
   * plus the corresponding nodes are inserted to the infoset tree.
   *
   * Does not put the anchor or incoming into the addedEvents.
   */
  private def augment2(anchor: InfosetEvent, incoming: InfosetEvent, addedEvents: mutable.ArrayBuffer[InfosetEvent]): Unit = {
    val anchorERD = anchor.erd
    val maybeAugmenter = anchor match {
      case Start(c: DIComplex) => anchor.erd.maybeChildInfosetAugmenter
      case _ => anchor.erd.maybeLaterSiblingInfosetAugmenter
    }
    if (maybeAugmenter.isEmpty) {
      // ok
    } else {
      incoming match {
        case Start(e: DIElement) if (anchorERD =:= e.runtimeData) => {
          //
          // This incoming event is for another start of this same element as the anchor
          //
          // So no augmentation to do. Even if the item is defaultable, we're not
          // defaulting here, because this event will carry the value.
          //
        }
        case Start(a: DIArray) => // ok
        case End(s: DISimple) => // ok
        case _ => {
          //
          // all other cases we have to at least consider augmenting.
          //
          // for an array with minOccurs > 0, we may or may not augment
          // on an End(a: DIArray) event, depending on the arrayPos0b.
          //
          // The compiler will choose an augmenter object based on
          // whether this is defaultable, computed, defaultable array, etc.
          //
          val augmenter =
            if (maybeAugmenter.isDefined) maybeAugmenter.get
            else Assert.invariantFailed("must be an augmenter at this point")
          augmenter.augment(incoming, arrayPos0b, addedEvents)
        }
      }
    }
  }

  /**
   * Given an unaugmented infoset iterator, fill a buffer with augmented infoset elements.
   */
  private def augment(src: IteratorWithPeek[InfosetEvent], addedEvents: mutable.ArrayBuffer[InfosetEvent]): Unit = {
    if (src.isEmpty) {
      // ok. Do nothing
    } else {
      val anchor = src.next // anchor element event

      anchor match {
        case Start(a: DIArray) => arrayIndexStack.push(0)
        case _ => // ok
      }

      if (src.isEmpty) {
        // ok
      } else {
        val incoming = src.peek
        augment2(anchor, incoming, addedEvents)
        advanceArrayPos // even if incoming is a scalar element, advancing this won't hurt anything
        //
        // if augmentation inserted an element start/end for
        // a dfdl:outputValueCalc element. That element will be considered
        // in turn as anchor and so any insertible element between that OVC element and
        // a later sibling or end-of-element are handled during the next
        // call to this method.
        //
      }
      addedEvents.insert(0, anchor) // put the anchor back.
      anchor match {
        case End(a: DIArray) => arrayIndexStack.pop
        case _ => // ok
      }
    }
  }

  private val addedEvents = new mutable.ArrayBuffer[InfosetEvent]
  private val augmentedIterator = new IteratorWithPeekImpl(initialUnaugmentedSource, addedEvents)

  /**
   * Indicates whether this anchor event has already been augmented (based on the next incoming event)
   * or if Nope, then that must still be done for the next element (which will be the anchor), and
   * subsequent event (the "incoming") event.
   *
   * This is a Maybe[InfosetEvent] for debugging purposes rather than just a Boolean.
   */
  private var augmentedAnchor: Maybe[InfosetEvent] = Nope

  private def fill {
    if (augmentedAnchor.isEmpty) {
      //
      // we do not have an anchor indicating that we've augmented the infoset events
      // relative to that anchor. So we must augment.
      //
      augment(augmentedIterator, addedEvents)
      if (augmentedIterator.hasNext) {
        augmentedAnchor = One(augmentedIterator.next)
      } else {
        augmentedAnchor = Nope
      }
    }
  }

  def peek = {
    fill
    augmentedAnchor.get
  }

  def next = {
    fill
    val res = augmentedAnchor.get
    augmentedAnchor = Nope
    res
  }

  def hasNext = {
    fill
    augmentedAnchor.isDefined
  }

}

/**
 * The schema compiler computes an instance of this for each element.
 *
 * This is for use augmenting the Daffodil Infoset and Infoset Event stream
 * with Infoset elements corresponding to computed and defaulted elements.
 *
 * Must be thread safe - as in cannot have state. These are stored on the
 * elementRuntimeData, and so are objects that are shared by all unparser threads that access
 * that elementRuntimeData.
 */
trait InfosetAugmenter extends Serializable {
  def augment(incomingInfosetEvent: InfosetEvent, arrayPos0b: Int, addedEvents: mutable.ArrayBuffer[InfosetEvent]): Unit
}

/**
 * Breaks out infoset augmenter to methods for individual event types. Default behavior
 * for any individual event is to error out. I.e., a derived class must override
 * the method for any event type that is possible, given the schema.
 */
abstract class InfosetAugmenterBase extends InfosetAugmenter {

  final override def augment(infosetEvent: InfosetEvent, arrayPos0b: Int, addedEvents: mutable.ArrayBuffer[InfosetEvent]): Unit = {
    val erd = infosetEvent.erd
    infosetEvent match {
      case Start(c: DIComplex) => startComplex(erd, infosetEvent, arrayPos0b, addedEvents)
      case End(c: DIComplex) => endComplex(erd, infosetEvent, arrayPos0b, addedEvents)
      case End(a: DIArray) => endArray(erd, infosetEvent, arrayPos0b, addedEvents)
      case Start(s: DISimple) => startSimple(erd, infosetEvent, arrayPos0b, addedEvents)
      case _ => Assert.invariantFailed("event should not be passed to this method. " + infosetEvent)
    }
  }

  private def err(erd: ElementRuntimeData, ev: InfosetEvent) = UnparseError(One(erd.schemaFileLocation), Nope, "Unexpected Infoset event for node: %s", ev)

  /**
   * Override the expected events depending on the kind of derived class.
   *
   * Ex: the InfosetNodeConstructor for a ComplexType element will not override startSimple or endSimple.
   */
  protected[unparsers] def startComplex(erd: ElementRuntimeData, ev: InfosetEvent, arrayPos0b: Int, addedEvents: mutable.ArrayBuffer[InfosetEvent]): Unit = err(erd, ev)
  protected[unparsers] def endComplex(erd: ElementRuntimeData, ev: InfosetEvent, arrayPos0b: Int, addedEvents: mutable.ArrayBuffer[InfosetEvent]): Unit = err(erd, ev)
  protected[unparsers] def endArray(erd: ElementRuntimeData, ev: InfosetEvent, arrayPos0b: Int, addedEvents: mutable.ArrayBuffer[InfosetEvent]): Unit = err(erd, ev)
  protected[unparsers] def startSimple(erd: ElementRuntimeData, ev: InfosetEvent, arrayPos0b: Int, addedEvents: mutable.ArrayBuffer[InfosetEvent]): Unit = err(erd, ev)

}

/**
 * Augmenter for when an element is followed by a OVC either as child of a complex element
 * or a later sibling.
 *
 * The isOVCElementPossibleFirstChild boolean is true if the OVC element is potentially the first
 * child of a complex type element, and false (sibling case) otherwise.
 */

abstract class InsertableAugmenterBase(
  erdOfInsertableElement: ElementRuntimeData,
  shouldAugmentParentOfIndicator: Boolean) extends InfosetAugmenter {

  final def erd = erdOfInsertableElement

  override def toString = Misc.getNameFromClass(this) + "(" + erd.namedQName + ")"

  /**
   * Determines the complex element to which we will add augmentation.
   *
   * If we're augmenting based on arrival of a sibling event to the insertable,
   * then we need to insert into the parent of that indicator.
   *
   * If we're augmenting based on arrival of an End(DIComplex) for the enclosing
   * element, then we insert directly into the node of that event, because it IS
   * the enclosing element.
   *
   * This is DIElement specific since we shouldn't encounter OVC in array context.
   */
  private def nodeToAugment(indicatorEvent: InfosetEvent): DIComplex =
    if (shouldAugmentParentOfIndicator) {
      // indicator was a later sibling
      val Start(node: DIElement) = indicatorEvent
      node.parent.asInstanceOf[DIComplex]
    } else {
      val End(node: DIComplex) = indicatorEvent
      node
    }

  protected def setNodeValue(insertableNode: DISimple): Unit

  override def augment(indicatorEvent: InfosetEvent, arrayPos0b: Int, addedEvents: mutable.ArrayBuffer[InfosetEvent]): Unit = {
    // arrayPos0b is ignored.
    val parent = nodeToAugment(indicatorEvent)
    val insertableNode = new DISimple(erdOfInsertableElement)
    parent.addChild(insertableNode)
    addedEvents.insert(0, Start(insertableNode), End(insertableNode)) // appending at the start.
    setNodeValue(insertableNode)
  }
}

final class OutputValueCalcAugmenter(
  erdOfOutputValueCalcElement: ElementRuntimeData,
  shouldAugmentParentOfIndicator: Boolean)
  extends InsertableAugmenterBase(erdOfOutputValueCalcElement, shouldAugmentParentOfIndicator) {

  override def setNodeValue(insertableNode: DISimple) {
    val erdOfInsertableElement = insertableNode.runtimeData
    //
    // If the expression is a constant, then we insert the value
    //
    // We could - in theory - also know if it was a backward-only expression
    // and in that case evaluate on the spot here as well. However, we
    // don't have a UState here, so some refactoring would have to occur
    // in order for us to call ovcExpr.evaluate(...)
    // That is, we would need an evaluate method that takes an infoset and DState only,
    // and not an entire ParseOrUnparseState.
    //
    val ovcExpr = erdOfInsertableElement.outputValueCalcExpr.getOrElse(
      Assert.invariantFailed("ERD lacks OVC information"))
    if (ovcExpr.isConstant) {
      insertableNode.setDataValue(ovcExpr.constant) // Should be right type. Compilation of expression would fail if it was wrong.
    }
  }
}

final class DefaultableScalarAugmenter(
  erd: ElementRuntimeData,
  shouldAugmentParentOfIndicator: Boolean)
  extends InsertableAugmenterBase(erd, shouldAugmentParentOfIndicator) {

  override def setNodeValue(insertableNode: DISimple) {
    val dv = erd.optDefaultValue.get
    if (dv =:= UseNilForDefault) {
      Assert.invariant(erd.nilledXML.isDefined)
      insertableNode.setNilled()
    } else {
      insertableNode.setDataValue(dv)
    }
  }
}

final class GeneralAugmenter(augMap: Map[NamedQName, InfosetAugmenter]) extends InfosetAugmenterBase {

  /**
   * For unit testing
   */
  def augmenterMap = augMap

  private def maybeAugment(ev: InfosetEvent, arrayPos0b: Int, addedEvents: mutable.ArrayBuffer[InfosetEvent]): Unit = {
    val eventName = ev.namedQName
    val optAugmenter = augMap.get(eventName)
    if (optAugmenter.isEmpty) Stream()
    else {
      val aug = optAugmenter.get
      aug.augment(ev, arrayPos0b, addedEvents)
    }
  }

  override protected[unparsers] def startComplex(erd: ElementRuntimeData, ev: InfosetEvent, arrayPos0b: Int, addedEvents: mutable.ArrayBuffer[InfosetEvent]): Unit =
    maybeAugment(ev, arrayPos0b, addedEvents)
  override protected[unparsers] def startSimple(erd: ElementRuntimeData, ev: InfosetEvent, arrayPos0b: Int, addedEvents: mutable.ArrayBuffer[InfosetEvent]): Unit =
    maybeAugment(ev, arrayPos0b, addedEvents)
  override protected[unparsers] def endComplex(erd: ElementRuntimeData, ev: InfosetEvent, arrayPos0b: Int, addedEvents: mutable.ArrayBuffer[InfosetEvent]): Unit =
    maybeAugment(ev, arrayPos0b, addedEvents)

}
