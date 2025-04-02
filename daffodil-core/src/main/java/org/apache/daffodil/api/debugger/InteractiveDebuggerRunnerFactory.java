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

package org.apache.daffodil.api.debugger;

import org.apache.daffodil.runtime1.debugger.InteractiveDebuggerRunnerImpl;
import org.apache.daffodil.runtime1.debugger.TraceDebuggerRunner;

import java.io.PrintStream;

/**
 * Factory for Interactive Debugger Runners
 */
public class InteractiveDebuggerRunnerFactory {

  /**
   * private constructor to disable initialization. use InteractiveDebuggerRunnerFactory.get(...)
   * instead
   */
  private InteractiveDebuggerRunnerFactory() {
  }

  /**
   * Factory method to get an instance of InteractiveDebuggerRunnerImpl
   *
   * @param dr debugger runner
   * @return instance of InteractiveDebuggerRunnerImpl
   */
  public static InteractiveDebuggerRunner get(DebuggerRunner dr) {
    return new InteractiveDebuggerRunnerImpl(dr);
  }

  /**
   * Factory method to get an instance of TraceDebuggerRunner
   *
   * @param out stream to print trace to
   * @return instance of TraceDebuggerRunner
   */
  public static org.apache.daffodil.api.debugger.TraceDebuggerRunner getTraceDebuggerRunner(PrintStream out) {
    return new TraceDebuggerRunner(out);
  }
}
