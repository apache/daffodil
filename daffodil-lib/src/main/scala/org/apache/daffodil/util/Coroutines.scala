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

 package org.apache.daffodil.util

 import org.apache.daffodil.exceptions.UnsuppressableException

 import java.util.concurrent.ArrayBlockingQueue
 import scala.util.Failure
 import scala.util.Success
 import scala.util.Try

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

   private var thread_ : Option[Thread] = None

   private final def init(): Unit = {
     if (!isMain && thread_.isEmpty) {
       val thr = new Thread {
         override def run(): Unit = self.run()
       }
       thread_ = Some(thr)
       thr.start()
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

 final class InvertControl[S](body: => Unit)
   extends MainCoroutine[Try[S]]
     with Iterator[S] {

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

   override def hasNext: Boolean = {
     !failed && iterator.hasNext
   }
   override def next(): S = {
     if (failed) throw new IllegalStateException()
     else iterator.next()
   }

 }
