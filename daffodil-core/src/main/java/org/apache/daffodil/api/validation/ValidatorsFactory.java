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

package org.apache.daffodil.api.validation;

import org.apache.daffodil.lib.validation.DaffodilLimitedValidator$;
import org.apache.daffodil.lib.validation.NoValidator$;
import org.apache.daffodil.lib.validation.XercesValidatorFactory;


import java.net.URI;

/**
 * Factory for creating validators
 */
public class ValidatorsFactory {
  private ValidatorsFactory() {
  }

  public static Validator getLimitedValidator() {
    return DaffodilLimitedValidator$.MODULE$;
  }

  public static Validator getXercesValidator(String mainSchemaForFullValidation) {
    return XercesValidatorFactory.makeValidator(mainSchemaForFullValidation);
  }

  public static Validator getXercesValidator(URI uri) {
    return getXercesValidator(uri.toString());
  }

  public static Validator fromValidationMode(String m, URI uri) {
    return fromValidationMode(m, uri.toString());
  }

  public static Validator fromValidationMode(String m, String uri) {
    Validator v = null;
    switch (m) {
      case "on":
        v = getXercesValidator(uri);
        break;
      case "limited":
        v = getLimitedValidator();
        break;
      case "off":
        v = NoValidator$.MODULE$;
        break;
      default:
        throw new IllegalArgumentException("Unsupported validation mode supplied: " + m);
    }
    return v;
  }
}
