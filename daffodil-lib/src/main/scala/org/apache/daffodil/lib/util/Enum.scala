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
 * Enum Idiom
 *
 * Note that we looked online at Enum idioms. We ended up having all sorts
 * of subtle initialization errors.
 *
 * We ended up back at just case objects inside a packaging object.
 *
 * About all this accomplishes is to insist that this is how the enums work.
 * <pre>
 * object Suits extends Enum {
 *   abstract sealed trait Type extends EnumValueType
 *   case object Clubs extends Type
 *   case object Hearts extends Type
 *   ...
 *   }
 *
 * ...
 *   def getClubs: Suits.Type == Suits.Clubs
 *   // must use Suits.Type to refer to the type.
 *   // and use Suites.Clubs to refer to an enum value.
 * </pre>
 *
 * If you want ordered, then make Type extend Ordered[Type] add an
 * integer id field, etc.
 *
 * How to use: search the source for " extends Enum" and you'll find a few examples.
 *
 * Note: There is a different enum idiom used in the DFDL properties code
 * that is laid down by the code generator. This one is a small improvement
 * on that one, but no point in changing that one and modifying the code generator
 * and all, until Scala has a language-supported enum idiom (which it will
 * some day, as it is being discussed anyway.)
 */
abstract class Enum {

  type Type <: EnumValueType

  // Base class for enum values
  protected trait EnumValueType

}
