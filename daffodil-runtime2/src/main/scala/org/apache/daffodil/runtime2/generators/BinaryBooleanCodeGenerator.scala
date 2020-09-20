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
import org.apache.daffodil.exceptions.Assert
import passera.unsigned.ULong

trait BinaryBooleanCodeGenerator extends BinaryAbstractCodeGenerator {

  def binaryBooleanGenerateCode(e: ElementBase, cgState: CodeGeneratorState): Unit = {
    Assert.invariant(e.binaryBooleanTrueRep.isEmpty || e.binaryBooleanTrueRep.getULong >= ULong(0))
    Assert.invariant(e.binaryBooleanFalseRep >= ULong(0))
    Assert.invariant(e.elementLengthInBitsEv.isConstant)

    val lengthInBits = e.elementLengthInBitsEv.constValue.get
    val initialValue = lengthInBits match {
      case 8 | 16 | 32 => "true"
      case _ => e.SDE("Boolean lengths other than 8, 16, or 32 bits are not supported.")
    }
    val prim = s"bool$lengthInBits"
    val trueRep = if (e.binaryBooleanTrueRep.isDefined) e.binaryBooleanTrueRep.getULong else -1
    val falseRep = e.binaryBooleanFalseRep
    val parseArgs = s"$trueRep, $falseRep, pstate"
    val unparseTrueRep = if (e.binaryBooleanTrueRep.isDefined) s"$trueRep" else s"~$falseRep"
    val unparseArgs = s"$unparseTrueRep, $falseRep, ustate"

    binaryAbstractGenerateCode(e, initialValue, prim, parseArgs, unparseArgs, cgState)
  }
}
