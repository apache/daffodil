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

package org.apache.daffodil.runtime1.infoset

/**
 * XMLTextEscapeStyle determines how to wrap values of elements of type xs:string
 *
 * Standard - Special characters (quotation mark, ampersand, less-than, greater-than)
 * in the text of xs:string elements are escaped, while non-special characters are
 * written as is.
 *
 * CDATA - The text of xs:string elements are wrapped in CDATA tags if the string
 * contains whitespace or special characters (quotation mark, ampersand, less-than,
 * greater-than)
 */
object XMLTextEscapeStyle extends Enumeration {
  type XMLTextEscapeStyle = Value
  val Standard, CDATA = Value
}
