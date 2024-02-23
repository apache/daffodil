/*
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.apache.daffodil.runtime1.layers

import java.net.URI
import java.net.URISyntaxException
import java.util.Objects

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.xml.NS
import org.apache.daffodil.lib.xml.QName

object LayerUtils {

  def spiName(local: String, namespace: String): String = spiName(local, NS(namespace))
  def spiName(local: String, ns: NS): String = QName.toPrettyString(None, local, ns)

  def requireJavaIdCompatible(s: String, what: String): Unit = {
    Objects.requireNonNull(s)
    Assert.usage(s.nonEmpty)
    if (
      !Character.isJavaIdentifierStart(s.head) ||
      s.tail.exists { !Character.isJavaIdentifierPart(_) }
    )
      throw new IllegalArgumentException("Arg " + what + " must be a legal Java identifier.")
  }

  def requireURICompatible(s: String, what: String): Unit = {
    Objects.requireNonNull(s)
    // parse s as a URI, catch any exception it throws and report illegal argument if so.
    try new URI(s)
    catch {
      case e: URISyntaxException =>
        throw new IllegalArgumentException(
          "Arg " + what + " must be compatible with a URI syntax.",
        )
    }
  }

}
