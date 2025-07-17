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

package org.apache.daffodil.api.infoset;

/**
 * InfosetOutputter whose result is represented as an org.jdom2.Document
 */
public abstract class JDOMInfosetOutputter extends InfosetOutputter {

  /**
   * Get the jdom Document representing the infoset created during a parse
   * <p>
   * This function should only be called if {@code ParseResult.isError()} returns false
   *
   * @return jdom document
   */
  public abstract org.jdom2.Document getResult();
}
