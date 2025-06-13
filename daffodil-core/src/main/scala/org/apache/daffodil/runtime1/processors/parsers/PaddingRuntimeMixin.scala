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

package org.apache.daffodil.runtime1.processors.parsers

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.MaybeChar
import org.apache.daffodil.runtime1.processors.TextJustificationType

trait PaddingRuntimeMixin {
  protected def justificationTrim: TextJustificationType.Type
  protected def parsingPadChar: MaybeChar

  private def removeRightPadding(str: String): String = {
    // must have a pad character if we are removing padding
    Assert.invariant(parsingPadChar.isDefined)
    val padChar = parsingPadChar.get
    var index = str.length - 1
    while (index >= 0 && str(index) == padChar) {
      index -= 1
    }
    str.substring(0, index + 1)
  }

  private def removeLeftPadding(str: String): String = {
    // must have a pad character if we are removing padding
    Assert.invariant(parsingPadChar.isDefined)
    val padChar = parsingPadChar.get
    var index = 0
    while (index < str.length && str(index) == padChar) {
      index += 1
    }
    str.substring(index)
  }

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
