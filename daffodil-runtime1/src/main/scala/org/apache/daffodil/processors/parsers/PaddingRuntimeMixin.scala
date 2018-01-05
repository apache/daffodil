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

package org.apache.daffodil.processors.parsers

import org.apache.daffodil.processors.TextJustificationType
import org.apache.daffodil.util.MaybeChar

trait PaddingRuntimeMixin {
  protected def justificationTrim: TextJustificationType.Type
  protected def parsingPadChar: MaybeChar

  private def removeRightPadding(str: String): String =
    if (parsingPadChar.isEmpty) str else str.reverse.dropWhile(c => c == parsingPadChar.get).reverse
  private def removeLeftPadding(str: String): String =
    if (parsingPadChar.isEmpty) str else str.dropWhile(c => c == parsingPadChar.get)
  private def removePadding(str: String): String = removeRightPadding(removeLeftPadding(str))

  def trimByJustification(str: String): String = {
    val result = justificationTrim match {
      case TextJustificationType.None => str
      case TextJustificationType.Right => removeLeftPadding(str)
      case TextJustificationType.Left => removeRightPadding(str)
      case TextJustificationType.Center => removePadding(str)
    }
    result
  }
}