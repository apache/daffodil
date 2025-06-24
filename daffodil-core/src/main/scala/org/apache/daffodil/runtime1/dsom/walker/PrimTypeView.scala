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
package org.apache.daffodil.runtime1.dsom.walker

/**
 * This is a series of View Mixins similar to the ones defined in AbstractDSOMWalker.
 * Since these traits don't have any attributes, their only purpose is for pattern matching
 * simple types during a walk.
 *
 * They had to be defined in runtime-1 because all of the PrimType enums in NodeInfo extend them,
 * and NodeInfo is part of runtime-1.
 */

trait PrimTypeView
trait StringView extends PrimTypeView
trait BooleanView extends PrimTypeView
trait ByteView extends PrimTypeView
trait ShortView extends PrimTypeView
trait IntView extends PrimTypeView
trait LongView extends PrimTypeView
trait UnsignedByteView extends PrimTypeView
trait UnsignedShortView extends PrimTypeView
trait UnsignedIntView extends PrimTypeView
trait UnsignedLongView extends PrimTypeView
trait NonNegativeIntegerView extends PrimTypeView
trait IntegerView extends PrimTypeView
trait FloatView extends PrimTypeView
trait DoubleView extends PrimTypeView
trait DecimalView extends PrimTypeView
trait HexBinaryView extends PrimTypeView
trait AnyURIView extends PrimTypeView
trait DateTimeView extends PrimTypeView
trait DateView extends PrimTypeView
trait TimeView extends PrimTypeView
