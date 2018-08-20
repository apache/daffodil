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

package org.apache.daffodil.sapi

/**
 * Provides the classes necessary to receive logging messages from Daffodil.
 *
 * <h3>Overview</h3>
 *
 * Daffodil comes with three prebuilt log writers:
 * <ul>
 *  <li>[[ConsoleLogWriter]] - writes all log messages to stdout</li>
 *  <li>[[FileLogWriter]] - writes all log messages to file</li>
 *  <li>[[NullLogWriter]] - drop all log messages</li>
 *</ul>
 *
 * To use one of these log writers, create and instance of it and pass it to
 * [[Daffodil.setLogWriter]]. For example, to write all logs to {@code /var/log/daffodil.log}:
 *
 * {{{
 * val lw = new FileLogWriter(new File("/var/log/daffodil.log"))
 * Daffodil.setLogWriter(lw)
 * }}}
 *
 * One may also change the log level using [[Daffodil.setLoggingLevel]], which
 * defaults to [[LogLevel.Info]] if not set. For example, to change the log
 * level to [[LogLevel.Warning]]:
 * {{{
 * Daffodil.setLoggingLevel(LogLevel.Warning);
 * }}}
 *
 */
package object logger

