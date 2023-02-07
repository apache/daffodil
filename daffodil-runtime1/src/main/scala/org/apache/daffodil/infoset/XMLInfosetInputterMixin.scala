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

package org.apache.daffodil.infoset

import org.apache.daffodil.api.XMLConversionControl
import org.apache.daffodil.xml.RemapPUAToXMLIllegalChar

/**
 * Centralized remapper in case we need to parameterize
 * the charset remapping.
 *
 * Not necessary right now, but this is symmetric
 * with XMLInfosetInputterMixin.
 */
trait XMLInfosetInputterMixin {

  def xmlConversionControl: XMLConversionControl

  private val remapper: RemapPUAToXMLIllegalChar =
    new RemapPUAToXMLIllegalChar()

  def remapped(dataValueAsString: String) =
    remapper.remap(dataValueAsString)
}
