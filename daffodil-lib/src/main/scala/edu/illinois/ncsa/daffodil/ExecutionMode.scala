package edu.illinois.ncsa.daffodil
import scala.util.DynamicVariable
import edu.illinois.ncsa.daffodil.exceptions.Assert

/**
 * This uses a scala DynamicVariable to create a dynamically scoped binding of
 * the Execution mode information (are we compiling, or are we at runtime).
 *
 * This works as a thread-local variable. So different threads can have independent dynamic
 * bindings.
 *
 * Supposedly this inherits so if one thread creates another, then it will inherit
 * the current value of the dynamic variable. (So, if we use parallelism using say, futures,
 * which is a natural idiom for functional programming, then threads they create would
 * work right.)
 *
 */
object ExecutionMode {

  private sealed class ExecutionModeBase

  // The point of unknown mode is that it allows us to catch
  // situations where we neglected to wrap usingCompilerMode or 
  // usingRuntimeMode around something.

  private case object UnknownMode extends ExecutionModeBase
  private object CompileMode extends ExecutionModeBase
  private object RuntimeMode extends ExecutionModeBase

  private val executionMode = new DynamicVariable[ExecutionModeBase](UnknownMode)

  /**
   * Use this by doing:
   * <pre>
   *     usingCompilerMode {
   *     ... everything in this dynamic scope can call
   *     requireCompilerMode, and it will not do anything
   *     but requireRuntimeMode will abort.
   *     ...
   *     }
   * </pre>
   * Symmetrically for runtime mode.
   */

  final def usingCompilerMode[S] = executionMode.withValue[S](CompileMode) _
  final def usingRuntimeMode[S] = executionMode.withValue[S](RuntimeMode) _

  private final def isCompilerMode = executionMode.value == CompileMode
  private final def isRuntimeMode = executionMode.value == RuntimeMode
  private final def isUnknownMode = executionMode.value == UnknownMode

  private def notUnknown = {
    // val msg = "Warning code is not wrapped with either usingCompilerMode or usingRuntimeMode"
    // Assert.usageErrorUnless(!isUnknownMode, msg)
    // if (isUnknownMode) System.err.println(msg)
    !isUnknownMode
  }

  final def requireCompilerMode = {
    if (notUnknown)
      Assert.invariant(isCompilerMode)
    // if (!isCompilerMode) System.err.println("Doing a compile time thing at runtime!")
  }

  final def requireRuntimeMode = {
    if (notUnknown)
      Assert.invariant(isRuntimeMode)
  }

}
