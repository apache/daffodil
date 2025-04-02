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

package org.apache.daffodil.core.grammar

import org.apache.daffodil.core.dsom.SchemaSet
import org.apache.daffodil.core.runtime1.SchemaSetRuntime1Mixin
import org.apache.daffodil.lib.iapi.ParseUnparsePolicyTunable
import org.apache.daffodil.lib.schema.annotation.props.gen.ParseUnparsePolicy

trait SchemaSetGrammarMixin extends SchemaSetRuntime1Mixin { self: SchemaSet =>

  lazy val (generateParser, generateUnparser) = {
    val (context, policy) = tunable.parseUnparsePolicy match {
      case ParseUnparsePolicyTunable.FromRoot => (Some(root), root.rootParseUnparsePolicy)
      case ParseUnparsePolicyTunable.ParseOnly => (None, ParseUnparsePolicy.ParseOnly)
      case ParseUnparsePolicyTunable.UnparseOnly => (None, ParseUnparsePolicy.UnparseOnly)
      case ParseUnparsePolicyTunable.Both => (None, ParseUnparsePolicy.Both)
    }
    root.checkParseUnparsePolicyCompatibility(context, policy)
    policy match {
      case ParseUnparsePolicy.Both => (true, true)
      case ParseUnparsePolicy.ParseOnly => (true, false)
      case ParseUnparsePolicy.UnparseOnly => (false, true)
    }
  }

}
