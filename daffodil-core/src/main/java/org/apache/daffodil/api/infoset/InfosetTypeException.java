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
 * Thrown if you try to access a simple type but the value of the
 * InfosetSimpleElement is not convertible to that type.
 */
public class InfosetTypeException extends Exception {

  /**
   * constructor for string and throwable
   *
   * @param msg   error message
   * @param cause error throwable
   */
  public InfosetTypeException(String msg, Throwable cause) {
    super(msg, cause);
  }

  /**
   * constructor for string only
   *
   * @param msg error message
   */
  public InfosetTypeException(String msg) {
    super(msg);
  }

  /**
   * constructor for sthrowable only
   *
   * @param cause error throwable
   */
  public InfosetTypeException(Throwable cause) {
    super(cause);
  }
}
