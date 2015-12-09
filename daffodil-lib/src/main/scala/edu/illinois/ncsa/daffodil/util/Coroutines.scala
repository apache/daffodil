package edu.illinois.ncsa.daffodil.util

import java.util.concurrent.ArrayBlockingQueue
import edu.illinois.ncsa.daffodil.exceptions.Assert
import edu.illinois.ncsa.daffodil.exceptions.UnsuppressableException
import Maybe._

/**
 * General purpose Co-routines.
 *
 * Some design concerns: if these are used along with lazy vals and other things
 * that make use of synchronized methods, there could be interactions.
 *
 * Definition of Coroutine - separate stacks, but NO CONCURRENCY. Only one
 * of a set of coroutines is running at any given time.
 *
 * Note: T cannot be derived from Throwable.
 */
trait Coroutine[T <: AnyRef] extends CoroutineAny {

  def waitForResume: T = waitForResumeAny.asInstanceOf[T]
  def resume(toResume: CoroutineAny, in: T): T = resumeAny(toResume, in).asInstanceOf[T]
  def resumeFinal(toResume: CoroutineAny, in: T): T = resumeAnyFinal(toResume, in).asInstanceOf[T]
}

trait CoroutineAny {

  private val queueCapacity: Int = 1
  private val inboundQueue = new ArrayBlockingQueue[AnyRef](queueCapacity)

  private val self = this

  /**
   * Override this in the main thread to be
   *
   * `override final def isMain = true`
   *
   * This suppresses creation of a thread for when the main
   * thread is itself one of the co-routines.
   */
  protected def isMain: Boolean = false

  private var thread_ : Option[Thread] = None

  protected def init {
    if (!isMain && thread_.isEmpty) {
      val thr = new Thread {
        override def run() = self.run()
      }
      thread_ = Some(thr)
      thr.start
    }
  }

  /**
   * Call when a co-routine resumes another (to provide a result of some sort)
   * and then terminates. The coroutine calling this must return from the run()
   * method immediately after calling this.
   *
   * Pass a throwable as the value of the in argument, to cause the resumed thread
   * to throw.
   */
  final def resumeAnyFinal(coroutine: CoroutineAny, in: AnyRef): Unit = {
    coroutine.init
    val q = coroutine.inboundQueue
    q.put(in) // allows other to run  final
  }

  /**
   * Call when one co-routine wants to resume another, transmitting a
   * argument value to it.
   *
   * If the argument is Throwable, it will be thrown by the resumed co-routine.
   *
   * The current co-routine will be suspended until it is resumed later.
   */
  final def resumeAny(coroutine: CoroutineAny, in: AnyRef): AnyRef = {
    resumeAnyFinal(coroutine, in)
    val res = waitForResumeAny // blocks until it is resumed
    res
  }

  private var thrown: Throwable = null

  protected def wasTerminatedByThrow = thrown != null

  private def throwFailure(x: AnyRef) = {
    Assert.invariant(x ne null)
    x match {
      case th: Throwable => {
        thrown = th
        throw th
      }
      case _ => // ok
    }
  }

  final def waitForResumeAny: AnyRef = {
    val q = this.inboundQueue
    val obj = q.take
    throwFailure(obj)
    obj
  }

  protected def run(): Unit
}

/**
 * Convert something that has callbacks (e.g., SAX-like parser that calls back on events)
 * into a pull-style API aka Iterator.
 *
 * Uses Coroutines to run the callback-generator on a separate 'thread', so this
 * will allocate a thread. However, there is no concurrency here.
 *
 * Exceptions are reported on the thread doing the pulling, aka the consumer.
 *
 * Rules that this design requires/enforces/allows:
 * (1) It is assumed that you have no access to the thing that generates call-backs other than
 * you can start it. It can be an opaque library you cannot modify.
 * (2) This call-back generating code does not have to be thread safe. Hence, only one thread can
 * be executing at a time here. There MUST BE NO CONCURRENCY. Two threads are necessary here,
 * but only one will be executing at a time.
 * (3) Finite storage - no building up of an arbitrary list/stream.
 *
 * Concepts adapted from
 * https://gist.github.com/dportabella/5766099
 * and
 * https://scalaenthusiast.wordpress.com/2013/06/12/transform-a-callback-function-to-an-iteratorlist-in-scala/
 *
 * There are two coroutines here. The main thread becomes the thread of the "InvertControl" coroutine, and
 * a second coroutine runs the event producer, and hence a second thread is allocated to support
 * this co-routine.
 */

final class InvertControl[S <: AnyRef](body: => Unit) extends IteratorWithPeek[S] with Coroutine[S] {

  /**
   * The producer will run the body function, and from within it,
   * calls to setNext(value) will
   * produce the values for the consumer. The consumer (main thread)
   * just uses ordinary Iterator hasNext/next calls to get the values.
   *
   * After the last value is produced, the consumer is resumed with EndOfData
   * and the producer terminates.
   */
  class Producer(val consumer: Coroutine[S]) extends Coroutine[S] {
    override final def run() {
      try {
        waitForResume
        body
        resumeAnyFinal(consumer, endOfData)
      } catch {
        case s: scala.util.control.ControlThrowable =>
          throw s
        case u: UnsuppressableException =>
          throw u
        case r: RuntimeException =>
          resumeAnyFinal(consumer, r)
        case e: Exception =>
          resumeAnyFinal(consumer, e)
      }
    }

    final def setNext(e: S) {
      resume(consumer, e)
    }
  }

  final def setNext(s: S) =
    producer.setNext(s)

  private val producer = new Producer(this)

  override final def isMain = true

  private case object endOfData { override def toString = "endOfData" }
  private case object request { override def toString = "request" }

  private var currentItem: Maybe[AnyRef] = Nope

  private def isFetched: Boolean = currentItem.isDefined

  private def fetch {
    if (isFetched || wasTerminatedByThrow) return
    currentItem = Nope
    val x = resumeAny(producer, request) // producer isn't sent anything. It's just resumed to get another value.
    if (x != endOfData)
      currentItem = One(x.asInstanceOf[AnyRef])
  }

  override def hasNext = {
    fetch
    isFetched
  }

  override def next = {
    fetch
    if (!isFetched) throw new NoSuchElementException()
    val res = currentItem.get.asInstanceOf[S]
    currentItem = None
    res
  }

  override def peek = {
    fetch
    if (!isFetched) throw new NoSuchElementException()
    currentItem.get.asInstanceOf[S]
  }

  override def run() {
    Assert.invariantFailed("Main thread co-routine run method should not be called.")
  }

}
