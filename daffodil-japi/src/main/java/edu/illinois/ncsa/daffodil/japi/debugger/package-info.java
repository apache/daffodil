/**
 * Provides the classes necessary to perform parse tracing or create a custom debugger
 *
 * <h3>Overview</h3>
 *
 * Daffodil comes with one prebuilt debugger, the {@link
 * edu.illinois.ncsa.daffodil.japi.debugger.TraceDebuggerRunner}, which outputs
 * verbose information during the parsing processes, which can be used to aid
 * in debugging a DFDL schema. For example, the {@link
 * edu.illinois.ncsa.daffodil.japi.debugger.TraceDebuggerRunner} can be use like so:
 *
 * <pre>
 * {@code
 * TraceDebuggerRunner tdr = new TraceDebuggerRunner();
 * Daffodil.setDebugger(tdr);
 * }</pre>
 *
 * Additionally, one may create their own debugger runner by implementing the
 * methods in the {@link
 * edu.illinois.ncsa.daffodil.japi.debugger.DebuggerRunner}.
 * <p>
 * Once the debugger is set, it must then be turned on, like so:
 *
 * <pre>
 * {@code
 * Daffodil.setDebugging(true);
 * }</pre>
 */

package edu.illinois.ncsa.daffodil.japi.debugger;

