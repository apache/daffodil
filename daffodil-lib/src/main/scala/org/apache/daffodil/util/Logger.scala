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

package org.apache.daffodil.util

import org.apache.logging.log4j.scala.Logging

/**
 * The log4j.scala.Logging trait adds a log4j.scala.Logger member val called
 * 'logger' to whatever class mixes it in. Classes that mixin this trait can
 * then just call logger.warn/info/debug/etc to log a message, which are macros
 * to minimize overhead.
 *
 * However, the Logger class is not serializable, and so all classes that might
 * need to be serialized, (e.g. runtime objects), cannot take this approach.
 *
 * Instead, we have a single Logger object that mixes in this trait. This
 * object is never serialized and so can be used anywhere, regardless of
 * serializablility. For simplicity, consistency, and to avoid potential
 * serialization issues, all logging should be done via the logger in this
 * object, rather than mixing in the Logging trait, for example:
 *
 *   Logger.log.info("Message to log")
 *
 * The downside to this is that it breaks the ability to use Log4j's feature to
 * configure different log levels for different namespaces or classes or have
 * useful line numbers, because all logging comes from and is associated with
 * this single Logger in the 'org.apache.daffodil.util.Logger' class.
 */
object Logger extends Logging {
  def log = this.logger  
}
