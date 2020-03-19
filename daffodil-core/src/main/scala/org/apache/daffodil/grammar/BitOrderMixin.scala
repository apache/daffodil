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

package org.apache.daffodil.grammar
import org.apache.daffodil.dsom.Term
import org.apache.daffodil.equality.TypeEqual
import org.apache.daffodil.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.processors.CheckByteAndBitOrderEv
import org.apache.daffodil.processors.CheckBitOrderAndCharsetEv
import org.apache.daffodil.util.Maybe
import org.apache.daffodil.dsom.{ Binary, NoText }
import org.apache.daffodil.dsom.ElementBase

trait BitOrderMixin extends GrammarMixin with ByteOrderAnalysisMixin { self: Term =>

  protected final lazy val optDefaultBitOrder: Option[BitOrder] = {
    val bitOrd =
      if (tunable.requireBitOrderProperty) {
        Some(bitOrder)
      } else {
        optionBitOrder
      }
    bitOrd
  }

  final lazy val defaultBitOrder = optDefaultBitOrder.getOrElse(BitOrder.MostSignificantBitFirst)
}
