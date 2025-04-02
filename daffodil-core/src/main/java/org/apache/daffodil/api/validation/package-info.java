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

/**
 * We provide 4 different types of built-in validators for use with
 * {@link org.apache.daffodil.api.DataProcessor#withValidator(org.apache.daffodil.api.validation.Validator)}:
 * <ul>
 *   <li>{@link org.apache.daffodil.validation.NoValidator} - for no validation</li>
 *   <li>{@link org.apache.daffodil.validation.DaffodilLimitedValidator} - for limited/daffodil schema constraints validation</li>
 *   <li>{@link org.apache.daffodil.validation.XercesValidator} - for full validation using Xerces schema validation</li>
 *   <li>see org.apache.daffodil.validation.schematron.SchematronValidator - for schematron validation</li>
 * </ul>
 * <p>
 * It is also possible to create a custom validator by doing the following:
 * <ol>
 *  <li>create a custom Validator class that implements the
 *  {@link org.apache.daffodil.api.validation.Validator} interface</li>
 *  <li>create a custom ValidatorFactory class that implements the
 *  {@link org.apache.daffodil.api.validation.ValidatorFactory} interface, whose make method
 *  returns the custom validator from the previous step</li>
 *  <li>Register the created custom Validator Factory by creating a {@link org.apache.daffodil.api.validation.ValidatorFactory}
 *  file in {@code META-INF/services/}, its contents being the fully qualified name of the custom validator factory</li>
 * </ol>
 * <p>
 * To get an instance of one of the provided validators, one will need:
 * <ul>
 *   <li> access to registered {@link org.apache.daffodil.api.validation.ValidatorFactory}s
 *   via the {@link org.apache.daffodil.api.validation.Validators#get(java.lang.String)} command</li>
 *   <li> a {@link java.util.Properties} object with the appropriate keys/values (see below for specifics for each kind of validator)</li>
 *   <li> the name of the validator (ex: off, limited, xerces or schematron) </li>
 * </ul>
 * <p>
 * To geta specific Validator factory instance, do the below:
 * <pre>
 * {@code
 *  org.apache.daffodil.api.validation.ValidatorFactory vf = org.apache.daffodil.api.validation.Validators.get("xerces")
 * }
 * </pre>
 * To get the validator, certain properties are required to be passed into the make function,
 * depending on the validator.
 * <ul>
 *   <li>{@link org.apache.daffodil.validation.NoValidator}
 *    <ul>
 *      <li>none; empty Properties object or null</li>
 *    </ul>
 *   </li>
 *   <li>{@link org.apache.daffodil.validation.DaffodilLimitedValidator}
 *    <ul>
 *      <li>none; empty Properties object or null</li>
 *    </ul>
 *   </li>
 *   <li>{@link org.apache.daffodil.validation.XercesValidator}
 *    <ul>
 *      <li>{@code daffodil.rootSchema=schema_file_uri_string}</li>
 *      <li>{@code xerces=schema_file_uri_string} - if not found, Daffodil will attempt to use
 *      the above instead. So one or both must be present, but this will always take precedence over
 *      daffodil.rootSchema if both are present.</li>
 *    </ul>
 *   </li>
 *   <li>org.apache.daffodil.validation.schematron.SchematronValidator
 *    <ul>
 *      <li>{@code schematron=uri_string_to_schematron_file} - if not found, Daffodil will attempt to use
 *      the daffodil.rootSchema instead. So one or both must be present, but this will always take precedence over
 *      daffodil.rootSchema if both are present. </li>
 *      <li>{@code schematron.svrl.file=uri_string_to_output_file}</li>
 *      <li>{@code daffodil.rootSchema=schema_file_uri_string}</li>
 *    </ul>
 *   </li>
 * </ul>
 * <p>
 * And finally get the validator using make as below:
 * <pre>
 * {@code
 *  Properties props = new Properties();
 *  props.setProperty(XercesValidator.name, "/path/to/validating/schema.xsd");
 *  org.apache.daffodil.validation.Validator v = vf.make(props);
 * }
 * </pre>
 * <p>
 * or one can load the below properties file as follows:
 * <pre>
 *   {@code
 *   // schematron.properties
 *   schematron=/path/to/schema.xsd
 *   schematron.svrl.file=/path/to/validation/output.txt
 *   }
 * </pre>
 *
 * <pre>
 * {@code
 *  Properties props = new Properties();
 *  props.load(new FileInputStream("schematron.properties"));
 *  org.apache.daffodil.validation.Validator v = vf.make(props);
 * }
 * </pre>
 */

package org.apache.daffodil.api.validation;
