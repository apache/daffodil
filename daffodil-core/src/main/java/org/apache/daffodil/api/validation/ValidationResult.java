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

import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * Results of a validation execution
 */
public class ValidationResult {
  private Collection<ValidationWarning> warnings;
  private Collection<ValidationFailure> errors;

  public static final ValidationResult empty = new ValidationResult();

  public ValidationResult() {
    this.warnings = Collections.emptyList();
    this.errors = Collections.emptyList();
  }

  public ValidationResult(List<ValidationWarning> warnings, List<ValidationFailure> errors) {
    this.warnings = warnings;
    this.errors = errors;
  }

  public Collection<ValidationWarning> getWarnings() {
    return warnings;
  }

  public void setWarnings(Collection<ValidationWarning> warnings) {
    this.warnings = warnings;
  }

  public Collection<ValidationFailure> getErrors() {
    return errors;
  }

  public void setErrors(Collection<ValidationFailure> errors) {
    this.errors = errors;
  }
}
