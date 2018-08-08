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
 * Provides the classes necessary to receive logging messages from Daffodil.
 *
 * <h3>Overview</h3>
 *
 * Daffodil comes with three prebuilt log writers:
 * <ul>
 *  <li>{@link org.apache.daffodil.japi.logger.ConsoleLogWriter} - writes all log messages to stdout</li>
 *  <li>{@link org.apache.daffodil.japi.logger.FileLogWriter} - writes all log messages to file</li>
 *  <li>{@link org.apache.daffodil.japi.logger.NullLogWriter} - drop all log messages</li>
 *</ul>
 *
 * To use one of these log writers, create and instance of it and pass it to
 * {@link org.apache.daffodil.japi.Daffodil#setLogWriter}. For example, to write all logs to {@code /var/log/daffodil.log}:
 *
 * <pre>
 * {@code
 * FileLogWriter lw = new FileLogWriter(new File("/var/log/daffodil.log"));
 * Daffodil.setLogWriter(lw);
 * }</pre>
 *
 * One may also change the log level using {@link org.apache.daffodil.japi.Daffodil#setLoggingLevel}, which defaults to {@link org.apache.daffodil.japi.logger.LogLevel#Info} if not set. For example, to change the log level to {@link org.apache.daffodil.japi.logger.LogLevel#Warning}:
 * <pre>
 * {@code
 * Daffodil.setLoggingLevel(LogLevel.Warning);
 * }</pre>
 *
 */

package org.apache.daffodil.japi.logger;

