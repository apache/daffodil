package edu.illinois.ncsa.daffodil.util

/**
 * Things that are named need some common methods.
 *
 * These are is isolated in a separate trait because it is hard to mix
 * together all the various overrides of these without
 * shared inheritance from a common trait.
 */
trait NamedMixinBase {

  /**
   * For diagnostics/trace/debug purposes
   */
  lazy val diagnosticDebugName: String = Misc.getNameFromClass(this)
}
