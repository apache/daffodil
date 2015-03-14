package edu.illinois.ncsa.daffodil.util

import java.util.concurrent.ArrayBlockingQueue
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import edu.illinois.ncsa.daffodil.exceptions.Assert

/**
 * General purpose Co-routines.
 *
 * Some design concerns: if these are used along with lazy vals and other things
 * that make use of synchronized methods, there could be interactions.
 *
 * Definition of Coroutine - separate stacks, but NO CONCURRENCY. Only one
 * of a set of coroutines is running at any given time.
 */
trait Coroutine[T] {

  private val queueCapacity: Int = 1
  private val inboundQueue = new ArrayBlockingQueue[Try[T]](queueCapacity)

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

  private final def init {
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
   */
  final def resumeFinal(coroutine: Coroutine[T], in: Try[T]): Unit = {
    coroutine.init
    val q = coroutine.inboundQueue
    q.put(in) // allows other to run  final 
  }

  /**
   * Call when one co-routine wants to resume another, tranmitting a
   * argument value to it.
   *
   * The current co-routine will be suspended until it is resumed later.
   */
  final def resume(coroutine: Coroutine[T], in: Try[T]): Try[T] = {
    resumeFinal(coroutine, in)
    val res = waitForResume // blocks until it is resumed
    res
  }

  final def waitForResume: Try[T] = {
    val q = this.inboundQueue
    q.take
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

final class InvertControl[S](body: => Unit) extends Iterator[S] with Coroutine[S] {

  private object EndMarker extends Throwable
  private val EndOfData = Failure(EndMarker)

  /**
   * The producer will run the body function, and from within it,
   * calls to setNext() will
   * produce the values for the consumer. The consumer (main thread)
   * just uses ordinary next/hasNext calls to get the values.
   *
   * After the last value is produced, the consumer is resumed with EndOfData
   * and the producer terminates.
   */
  class Producer(val consumer: Coroutine[S]) extends Coroutine[S] {
    override final def run() {
      try {
        waitForResume
        body
        resumeFinal(consumer, EndOfData)
      } catch {
        case r: RuntimeException => {
          resumeFinal(consumer, Failure(r))
          // throw r
        }
        case e: Exception => resumeFinal(consumer, Failure(e))
      }
    }

    final def setNext(e: S) {
      resume(consumer, Success(e))
    }
  }

  final def setNext(s: S) =
    producer.setNext(s)

  private val producer = new Producer(this)

  override final def isMain = true

  private var failed = false

  private val dummy: Try[S] = Success(null.asInstanceOf[S])

  private def gen: Stream[S] = {
    val x = resume(producer, dummy) // producer isn't sent anything. It's just resumed to get another value.
    x match {
      case EndOfData => Stream.Empty
      case Success(v) => v #:: gen
      case Failure(e) => {
        failed = true
        throw e
      }
    }
  }

  private lazy val iterator = gen.toIterator

  override def hasNext = {
    if (failed) false
    else iterator.hasNext
  }
  override def next = {
    if (failed) throw new IllegalStateException()
    else iterator.next
  }

  override def run() {
    Assert.invariantFailed("Main thread co-routine run method should not be called.")
  }

}
