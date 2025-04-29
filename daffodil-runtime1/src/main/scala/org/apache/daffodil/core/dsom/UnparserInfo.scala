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

package org.apache.daffodil.core.dsom

/**
 * Information needed specifically for unparsing.
 */
object UnparserInfo {

  sealed trait InfosetEventBehavior // i.e., relative to the infoset event stream, what behavior does this element have?

  sealed trait Defaultable
    extends InfosetEventBehavior // may or may not appear in infoset events

  case object ScalarDefaultable
    extends Defaultable // scalar that may or may not appear in infoset events

  case object ArrayDefaultable
    extends Defaultable // array element that may or may not appear in infoset events (need array index to determine whether to default it or not)

  case object Computed
    extends InfosetEventBehavior // Cannot appear in the infoset events - always created

  case object MustExist
    extends InfosetEventBehavior // corresponding element must appear in the infoset event stream.

  case object Optional
    extends InfosetEventBehavior // optional element or array with all-optional occurrences

}
