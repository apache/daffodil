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
import org.apache.daffodil.runtime1.processors.RuntimeData

trait PStateUsingMacrosMixin {
  this: org.apache.daffodil.runtime1.processors.parsers.PState =>

  inline def withPointOfUncertainty[B](id: String, ctx: RuntimeData)(
    inline func: PState.Mark => B
  ): B = {
    // create our new point of uncertainty
    val pou = createPointOfUncertainty(id, ctx)
    try {
      // evaluate the using this new point of uncertainty
      func(pou)
    } finally {
      // if the pou still exists at this point, then simply discard it.
      // This is likely because an exception occurred, or just because we
      // do not require that parsers discard PoUs, with the understanding
      // that it will always happen here
      if (!isPointOfUncertaintyResolved(pou)) {
        discardPointOfUncertainty(pou)
      }
    }
  }
}
