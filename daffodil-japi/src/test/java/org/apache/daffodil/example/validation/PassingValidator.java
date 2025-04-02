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

package org.apache.daffodil.example.validation;

import org.apache.daffodil.lib.iapi.ValidationFailure;
import org.apache.daffodil.lib.iapi.ValidationResult;
import org.apache.daffodil.lib.iapi.ValidationWarning;
import org.apache.daffodil.lib.iapi.Validator;

import java.io.InputStream;
import java.util.Collection;
import java.util.Collections;

public class PassingValidator implements Validator {
    public static final String name = "passing-japi-validator";

    @Override
    public ValidationResult validateXML(InputStream document) {
        return new Result();
    }

    public static class Result implements ValidationResult {
        public int magicNumberIs() {
            return 42;
        }

        @Override
        public Collection<ValidationWarning> warnings() {
            return Collections.emptyList();
        }

        @Override
        public Collection<ValidationFailure> errors() {
            return Collections.emptyList();
        }
    }
}
