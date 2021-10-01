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

package org.apache.daffodil.runtime2.generators

import org.apache.daffodil.dsom.ElementBase

trait BinaryIntegerKnownLengthCodeGenerator extends BinaryValueCodeGenerator {

  // Called by Runtime2CodeGenerator to generate C code for an integer element
  def binaryIntegerKnownLengthGenerateCode(e: ElementBase, lengthInBits: Long, signed: Boolean, cgState: CodeGeneratorState): Unit = {
    // Use an unusual memory bit pattern (magic debug value) to mark our field
    // as uninitialized in case parsing or unparsing fails to set the field.
    val initialValue = lengthInBits match {
      case 8 => "0x77"
      case 16 => "0x7777"
      case 32 => "0x77777777"
      case 64 => "0x7777777777777777"
      case _ => e.SDE("Integer lengths other than 8, 16, 32, or 64 bits are not supported.")
    }
    val primType = if (signed) s"int${lengthInBits}" else s"uint${lengthInBits}"
    val addField = valueAddField(e, initialValue, primType, _, cgState)
    val validateFixed = valueValidateFixed(e, _, cgState)

    binaryValueGenerateCode(e, addField, validateFixed)
  }
}
