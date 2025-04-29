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

package org.apache.daffodil.lib.util

import com.typesafe.scalalogging.{ Logger => ScalaLogger }

/**
 * At this time, we do not need loggers specific to each class. Instead we do
 * the lookup once to get the and "org.apache.daffodil" logger via the
 * scala-logging/SLF4J API and store that logger in this object. For simplicity
 * and consistency, all logging should be done via the logger in this object,
 * for example:
 *
 *   Logger.log.info("Message to log")
 *
 */
object Logger {
  val log = ScalaLogger("org.apache.daffodil")
}
