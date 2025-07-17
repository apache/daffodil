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

package org.apache.daffodil.api.exceptions;

import org.apache.daffodil.api.DaffodilUnparseContentHandler;
import org.xml.sax.SAXException;

/**
 * This exception is thrown when and unexpected error occurs while unparsing an
 * infoset with an XMLReader and a {@link DaffodilUnparseContentHandler}. If caught,
 * the {@code DaffodilUnparseContentHandler.getUnparseResult} returns null. This most
 * likely represents a bug in Daffodil.
 */
public class DaffodilUnhandledSAXException extends SAXException {
  /**
   * constructor for error message only
   *
   * @param message error message
   */
  public DaffodilUnhandledSAXException(String message) {
    super(message);
  }

  /**
   * constructor for error message and cause
   *
   * @param message error message
   * @param cause   error cause
   */
  public DaffodilUnhandledSAXException(String message, Exception cause) {
    super(message, cause);
  }

  /**
   * constructor for error cause only
   *
   * @param cause error cause
   */
  public DaffodilUnhandledSAXException(Exception cause) {
    super(cause);
  }
}
