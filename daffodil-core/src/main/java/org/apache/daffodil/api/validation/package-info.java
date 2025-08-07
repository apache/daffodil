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
 * Daffodil Validation API package
 *
 * <p>
 * Daffodil provides a number of built-in validators for use with {@link
 * org.apache.daffodil.api.DataProcessor#withValidation(String, URI)}. For each
 * built-in validator, the following contains the validator name, a
 * description, and validator specific properties. The {@code String} parameter
 * should be the name of the validator. If the URI parameter ends in
 * <i>.conf</i> or <i>.properties</i> then it is treated as a {@link
 * java.util.Properties} file that provides validator properties. Otherwise,
 * the URI is set to a property with the same name as the validator. If a
 * validator does not require properties, the URI parameter can be set to
 * {@code null} or excluded.
 * </p>
 *
 * <dl>
 *   <dt><span style="font-size: large;"><b>off</b></span></dt>
 *   <dd>
 *     <p><b>Description:</b> disable validation</p>
 *     <p><b>Properties:</b> none</p>
 *     <p><b>Example:</b></p>
 *     <pre>{@code
 * // explicitly disable validation
 * dataProcessor.withValidation("off")
 * }</pre>
 *   </dd>
 *
 *   <dt><span style="font-size: large;"><b>daffodil</b></span></dt>
 *   <dd>
 *     <p><b>Description:</b> XML schema validation using Daffodil</p>
 *     <p><b>Properties:</b> none</p>
 *     <p><b>Example:</b></p>
 *     <pre>{@code
 * // enable XML schema validation using Daffodil
 * dataProcessor.withValidation("daffodil")
 * }</pre>
 *   </dd>
 *
 *   <dt><span style="font-size: large;"><b>xerces</b></span></dt>
 *   <dd>
 *     <p><b>Description:</b> XML Schema validation using Xerces library</p>
 *     <p><b>Properties:</b></p>
 *     <dl>
 *       <dt>{@code xerces}</dt>
 *       <dd>absolute URI to a XSD file</dd>
 *     </dl>
 *     <p><b>Example:</b></p>
 *     <pre>{@code
 * // enable XML schema validation, setting the "xerces" property to the schema.xsd file
 * dataProcessor.withValidation("xerces", URI.create("file:///path/to/schema.xsd"))
 * }</pre>
 *   </dd>
 *
 *   <dt><span style="font-size: large;"><b>schematron</b></span></dt>
 *   <dd>
 *     <p><b>Description:</b> schematron validation using Saxon-HE library</p>
 *     <p><b>Properties:</b></p>
 *     <dl>
 *       <dt>{@code schematron}</dt>
 *       <dd>
 *         absolute URI to a file containing schematron rules. If the URI
 *         ends with {@code .sch} then the file is treated as a schematron file.
 *         Otherwise, it is treated as an XSD file containing embedded
 *         schematron rules.
 *       </dd>
 *       <dt>{@code schematron.svrl.file}</dt>
 *       <dd>
 *         absolute URI to a file to write the results of schematron
 *         validation in SVRL format. This property is not thread safe and
 *         will overwrite existing files--it is intended primarily for
 *         debugging with single file validation.
 *       </dd>
 *     </dl>
 *     <p><b>Example:</b></p>
 *     <pre>{@code
 * // enable schematron validation, setting the "schematron" property to the schematron.sch file
 * dataProcessor.withValidation("schematron", URI.create("file:///path/to/schematron.sch"))
 *
 * // use schematron validation, reading the schematron.properties file to set "schematron" or other schematron properties
 * dataProcessor.withValidation("schematron", URI.create("file:///path/to/schematron.properties"))
 * }</pre>
 *   </dd>
 * </dl>
 *
 * <h2>Custom Validators</h2>
 * <p>
 * Daffodil also supports custom validators. To make a custom validator available to Daffodil, follow these steps:
 * <ol>
 *  <li>Create a custom class that implements the
 *  {@link org.apache.daffodil.api.validation.Validator} interface</li>
 *  <li>Create a custom class that implements the
 *  {@link org.apache.daffodil.api.validation.ValidatorFactory} interface, whose {@code make} method
 *  returns the custom Validator from the previous step</li>
 *  <li>Register the custom ValidatorFactory by creating a {@link org.apache.daffodil.api.validation.ValidatorFactory}
 *  file in {@code META-INF/services/}, its contents being the fully qualified name of the custom validator factory</li>
 *  <li>Call the {@link org.apache.daffodil.api.DataProcessor#withValidation(String, URI)} function, providing the name
 *  of the validator and optional URI.</li>
 * </ol>
 */

package org.apache.daffodil.api.validation;
