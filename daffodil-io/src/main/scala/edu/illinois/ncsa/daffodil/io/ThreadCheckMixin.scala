package edu.illinois.ncsa.daffodil.io

import edu.illinois.ncsa.daffodil.exceptions.Assert

/**
 * Mixin to classes that are supposed to exist 1 to 1 with threads.
 * Such as DataInputStream derived classes and DataOutputStream derived classes.
 */
trait ThreadCheckMixin {

  private lazy val myFirstThread = Thread.currentThread()

  protected final def threadCheck() {
    Assert.invariant(Thread.currentThread eq myFirstThread)
  }
}