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

package org.apache.daffodil.dsom

/**
 * Mixin for all SchemaComponents
 */

trait SchemaComponentIncludesAndImportsMixin
  extends CommonContextMixin { self: SchemaComponent =>

  protected final val orElseURL: String = "file:??"

  /**
   * Used in diagnostic messages; hence, valueOrElse to avoid
   * problems when this can't get a value due to an error.
   */
  override def uriString: String = uriString_.toOption.getOrElse(orElseURL)
  private def uriString_ = LV('fileName) {
    xmlSchemaDocument.uriString
  }

}
