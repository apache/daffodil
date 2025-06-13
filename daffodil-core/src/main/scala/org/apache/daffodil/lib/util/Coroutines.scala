/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.lib.util

import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.apache.daffodil.lib.exceptions.UnsuppressableException

object Coroutine {

  /**
    * This execution context should be used when creating Coroutine threads to
    * improve performance. Creating threads has high overhead, but getting
    * threads from a thread pool we reduce some of that overhead.
    *
    * This thread pool is a cached thread pool, which means unused threads are
    * closed after 60 seconds of unuse. This also places no limits on the
    * number of threads that will be created, so if many coroutines are created
    * at the same time we could potentially starve the system of
    * threads/processing. However, we only create one thread per coroutine, and
    * we currently only create one coroutine per SAX unparse, and it is
    * hopefully unlikely that a user will create enough parallel SAX unparse
    * calls to cause issues.
    */
  val executionContext = new ExecutionContext {
    private val threadPool = Executors.newCachedThreadPool()
    def execute(runnable: Runnable): Unit = threadPool.submit(runnable)

    // $COVERAGE-OFF$
    def reportFailure(t: Throwable): Unit = {} // do nothing
    // $COVERAGE-ON$
  }
}

/**
  * General purpose Co-routines.
  *
  * Some design concerns: if these are used along with lazy vals and other things
  * that make use of synchronized methods, there could be interactions.
  *
  * Definition of Coroutine - separate stacks, but NO CONCURRENCY. Only one
  * of a set of coroutines is running at any given time.
 *
  * The queueCapacity being set to 1 ensures the NO CONCURRENCY property of this trait, so to reduce
  * the context-switching overhead is implementation specific, and can be done by passing in a
  * larger data structure containing multiple event for the Coroutine generic Type, rather than
  * enlarging the queue size
  *
  * The T type parameter defines the type of data that this Courtine is expected to
  * receive from its peer coroutine (i.e. the return type when this calls resume).
  * This T type parameter does not need to be the same for both coroutines, allowing
  * two coroutines to send and receive different types of data from one another.
  */
trait Coroutine[T] {

  private val queueCapacity: Int = 1
  private val inboundQueue = new ArrayBlockingQueue[T](queueCapacity)

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

  private var thread_ : Option[Future[Unit]] = None

  private final def init(): Unit = {
    if (!isMain && thread_.isEmpty) {
      val thr = Future {
        self.run()
      }(Coroutine.executionContext)
      thread_ = Some(thr)
    }
  }

  /**
    * Call when a co-routine resumes another (to provide a result of some sort)
    * and then terminates. The coroutine calling this must return from the run()
    * method immediately after calling this.
    *
    * @tparam R The type of objects transmitted to the other coroutine. These need
    *           not be the same type as the kind transmitted back to this calling
    *           coroutine.
    */
  final def resumeFinal[R](coroutine: Coroutine[R], in: R): Unit = {
    coroutine.init()
    coroutine.inboundQueue.put(in) // allows other to run final
  }

  /**
    * Call when one co-routine wants to resume another, transmitting a
    * argument value to it.
    *
    * The current co-routine will be suspended until it is resumed later.
    * @tparam R The type of objects transmitted to the other coroutine. These need
    *           not be the same type as the kind transmitted back to this calling
    *           coroutine.
    */
  final def resume[R](coroutine: Coroutine[R], in: R): T = {
    resumeFinal(coroutine, in)
    val res = waitForResume() // blocks until it is resumed
    res
  }

  final def waitForResume(): T = {
    inboundQueue.take()
  }

  protected def run(): Unit
}

/**
  * Convenience class, since many Coroutines systems have this as
  * the main coroutine definition
  * @tparam T The value type returned to this main coroutine when it is
  *           resumed by other coroutines.
  */
class MainCoroutine[T] extends Coroutine[T] {
  final override def isMain = true
  // $COVERAGE-OFF$
  override protected def run(): Unit = {
    throw new Error("Main thread co-routine run method should not be called.")
  }
  // $COVERAGE-ON$
}

/**
  * Convert something that has callbacks (e.g., SAX-like parser that calls back on events)
  * into a pull-style API aka Iterator.
  *
  * Exceptions are reported on the thread doing the pulling, aka the consumer.
  *
  * Rules:
  * (1) you have no access to the thing that generates call-backs other than
  * you can start it. It can be an opaque library you cannot modify.
  * (2) the generator code does not have to be thread safe. Hence, only one thread can
  * be executing at a time here. There MUST BE NO CONCURRENCY. Two threads are necessary here,
  * but only one will be executing at a time.
  * (3) Finite storage - no building up of an arbitrary list/stream.
  *
  * Concepts adapted from
  * https://gist.github.com/dportabella/5766099
  * and
  * https://scalaenthusiast.wordpress.com/2013/06/12/transform-a-callback-function-to-an-iteratorlist-in-scala/
  */

final class InvertControl[S](body: => Unit) extends MainCoroutine[Try[S]] with Iterator[S] {

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
  class Producer(val consumer: Coroutine[Try[S]]) extends Coroutine[Try[S]] {
    override final def run(): Unit = {
      try {
        waitForResume()
        body
        resumeFinal(consumer, EndOfData)
      } catch {
        case s: scala.util.control.ControlThrowable => throw s
        case u: UnsuppressableException => throw u
        case e: Exception => resumeFinal(consumer, Failure(e))
      }
    }

    final def setNext(e: S): Unit = {
      resume(consumer, Success(e))
    }
  }

  def setNext(s: S): Unit =
    producer.setNext(s)

  private val producer = new Producer(this)

  private var failed = false

  private val dummy: Try[S] = Success(null.asInstanceOf[S])

  private def gen: LazyList[S] = {
    val x = resume(
      producer,
      dummy
    ) // producer isn't sent anything. It's just resumed to get another value.
    x match {
      // TODO: no test coverage
      case EndOfData => LazyList.empty
      case Success(v) => v #:: gen
      case Failure(e) => {
        failed = true
        throw e
      }
    }
  }

  private lazy val eventIterator = gen.iterator

  override def hasNext: Boolean = {
    // TODO: no test coverage
    !failed && eventIterator.hasNext
  }
  override def next(): S = {
    if (failed) throw new IllegalStateException()
    // TODO: no test coverage
    else eventIterator.next()
  }

}
