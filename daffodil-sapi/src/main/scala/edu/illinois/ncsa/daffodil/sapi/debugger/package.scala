package edu.illinois.ncsa.daffodil.sapi

/**
 * Provides the classes necessary to perform parse tracing or create a custom debugger
 *
 * <h3>Overview</h3>
 *
 * Daffodil comes with one prebuilt debugger, the [[TraceDebuggerRunner]], which outputs
 * verbose information during the parsing processes, which can be used to aid
 * in debugging a DFDL schema. For example, the [[TraceDebuggerRunner]] can be use like so:
 *
 * {{{
 * val tdr = new TraceDebuggerRunner()
 * Daffodil.setDebugger(tdr)
 * }}}
 *
 * Additionally, one may create their own debugger runner by implementing the
 * methods in the [[DebuggerRunner]].
 * <p>
 * Once the debugger is set, it must then be turned on, like so:
 *
 * {{{
 * Daffodil.setDebugging(true);
 * }}}
 */

package object debugger

