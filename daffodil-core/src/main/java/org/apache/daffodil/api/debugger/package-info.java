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

/**
 * Provides the classes necessary to perform parse tracing or create a custom debugger
 *
 * <h2>Overview</h2>
 * <p>
 * Daffodil comes with one prebuilt debugger, the trace {@link org.apache.daffodil.api.debugger.Debugger},
 * which outputs verbose information during the parsing processes, which can be used to aid
 * in debugging a DFDL schema. For example, the trace {@link
 * org.apache.daffodil.api.debugger.Debugger} can be used like so:
 *
 * <pre>
 * {@code
 * Debugger td = Daffodil.newTraceDebugger(System.out);
 * dp.setDebugger(td);
 * }</pre>
 * <p>
 * Additionally, one may create their own debugger by creating a class that implements the {@link
 * org.apache.daffodil.api.debugger.DaffodilDebuggerRunner} interface and then calling
 * {@code Daffodil.newDaffodilDebugger(customRunner)} to get a debugger. 
 * Or they can create a class that implements the {@link org.apache.daffodil.api.debugger.Debugger} 
 * interface.
 * 
 * Then with either, they may call {@code DataProcessor.withDebugger(debugger)} to set the debugger.
 */

package org.apache.daffodil.api.debugger;
