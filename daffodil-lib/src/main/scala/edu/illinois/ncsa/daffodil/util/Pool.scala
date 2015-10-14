package edu.illinois.ncsa.daffodil.util

import edu.illinois.ncsa.daffodil.equality._
import edu.illinois.ncsa.daffodil.exceptions.Assert

/**
 * A pool is a collection of objects that are manually recycled usually via
 * some sort of a stack discipline.
 *
 * The point of it is to avoid excessive allocation of little objects.
 *
 * This is not thread safe.
 *
 * Either a pool becomes part of a state block that is separate per thread,
 * or it must be made using a ThreadLocal to get one per thread automatically.
 */
trait Pool[T <: AnyRef] {

  private val pool = new MStack.Of[T]

  private var numOutstanding: Int = 0

  protected def allocate: T

  final def getFromPool: T = {
    numOutstanding += 1
    if (pool.isEmpty) {
      allocate
    } else {
      pool.pop.asInstanceOf[T]
    }
  }

  final def returnToPool(thing: T) {
    numOutstanding -= 1
    pool.push(thing)
  }

  /**
   * Call this at end of execution to be sure all pooled items
   * have been returned.
   *
   * This is to help find resource leaks where items are taken from
   * the pool, but then dropped.
   */
  final def finalCheck {
    Assert.invariant(numOutstanding =#= 0)
  }

}
