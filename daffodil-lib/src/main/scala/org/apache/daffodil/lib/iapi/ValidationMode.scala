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

package org.apache.daffodil.lib.iapi

import org.apache.daffodil.lib.util.Enum

object ValidationMode extends Enum {
  sealed abstract class Type protected (val mode: Int)
    extends EnumValueType
    with Ordered[Type]
    with Serializable {
    def compare(that: ValidationMode.Type) = this.mode - that.mode
  }
  case object Off extends Type(10)
  case object Limited extends Type(20)
  case object Full extends Type(30)

  case class Custom(v: Validator) extends Type(100)

  def fromString(str: String): ValidationMode.Type = {
    str match {
      case "on" => Full
      case "limited" => Limited
      case "off" => Off
    }
  }
}
