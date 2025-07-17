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

package org.apache.daffodil.api;

/**
 * Factory object to create a {@link Compiler}
 */
public class Daffodil {

  /**
   * use Daffodil.compiler() instead
   */
  private Daffodil() {
  }

  /**
   * Create a new object used to compile DFDL schemas
   *
   * @return new object to compile DFDL schemas
   */
  public static org.apache.daffodil.api.Compiler compiler() {
    return org.apache.daffodil.core.compiler.Compiler.apply(true);
  }
}
