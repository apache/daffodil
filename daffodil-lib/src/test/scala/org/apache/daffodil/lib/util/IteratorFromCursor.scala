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

/**
 * Many tests are written using an Iterator pattern.
 *
 * Create an iterator (with peek), or a stream, given a Cursor.
 *
 * This copies the accessors - so that the result is truly safe, and
 * as would normally be expected it is a separate object.
 *
 * This defeats the purpose of cursors though, which is to populate
 * accessors, not copy them.
 */

class IteratorFromCursor[ItemType <: HasCpy[ItemType], AccessorType <: Accessor[AccessorType]](
  cursor: Cursor[AccessorType],
  converter: AccessorType => ItemType
) extends IteratorWithPeek[ItemType] {

  override def hasNext = cursor.inspect
  override def next() = converter(cursor.advanceMaybe.get).cpy()
  override def peek = converter(cursor.inspectMaybe.get).cpy()
}
