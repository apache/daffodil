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

object DPathUtil {

  /**
   * Whether a string is a DFDL expression. The definition of a DFDL expression
   * is a string that starts with an unescaped curly brace. If this is true,
   * then we want to consider this an expression and likely try to compile it
   * later and check for validity.
   */
  def isExpression(expression: String): Boolean = {
    val trimmed = expression.trim
    trimmed.startsWith("{") && !trimmed.startsWith("{{")
  }

}
