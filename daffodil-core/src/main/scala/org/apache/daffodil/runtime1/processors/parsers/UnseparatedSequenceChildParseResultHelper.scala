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

import org.apache.daffodil.lib.schema.annotation.props.EmptyElementParsePolicy
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.ModelGroupRuntimeData

/**
 * TODO: Consider merge of these helpers into the parser classes so we don't have
 * two distinct object taxonomies with mixins etc.
 */
trait UnseparatedSequenceChildParseResultHelper extends SequenceChildParseResultHelper {

  //  final override protected def anyTypeElementFailedParseAttemptStatus(pstate: PState,
  //    isZL: Boolean, requiredOptional: RequiredOptionalStatus): ParseAttemptStatus = {
  //    ParseAttemptStatus.Missing
  //  }
}

class ScalarElementUnseparatedSequenceChildParseResultHelper(
  override val erd: ElementRuntimeData,
  override val emptyElementParsePolicy: EmptyElementParsePolicy,
  override val isEmptyRepZeroLength: Boolean,
  override val isEmptyRepNonZeroLength: Boolean
) extends UnseparatedSequenceChildParseResultHelper
  with ScalarElementSequenceChildParseResultHelper
  with NonPositionalLikeElementSequenceChildParseResultMixin

class RepElementUnseparatedSequenceChildParseResultHelper(
  override val erd: ElementRuntimeData,
  override val emptyElementParsePolicy: EmptyElementParsePolicy,
  override val isEmptyRepZeroLength: Boolean,
  override val isEmptyRepNonZeroLength: Boolean
) extends UnseparatedSequenceChildParseResultHelper
  with RepElementSequenceChildParseResultHelper
  with NonPositionalLikeElementSequenceChildParseResultMixin

final class GroupUnseparatedSequenceChildParseResultHelper(
  override val mgrd: ModelGroupRuntimeData,
  override val isModelGroupRepPossiblyZeroLength: Boolean,
  override val isModelGroupRepNonZeroLength: Boolean
) extends UnseparatedSequenceChildParseResultHelper
  with ModelGroupSequenceChildParseResultHelper
  with PositionalLikeGroupSequenceChildParseResultMixin {}
