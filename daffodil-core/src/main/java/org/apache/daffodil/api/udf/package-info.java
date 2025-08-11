/*
Licensed to the Apache Software Foundation (ASF) under one or more
contributor license agreements.  See the NOTICE file distributed with
  this work for additional information regarding copyright ownership.
The ASF licenses this file to You under the Apache License, Version 2.0
  (the "License"); you may not use this file except in compliance with
the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
  */
/**
 * <h2>User Defined Functions</h2>
 *
 * <h3>Introduction</h3>
 * <p>
 * Apache Daffodil allows execution of Java/Scala external (user defined) functions in DFDL expressions.
 * </p>
 *
 * <h3>Getting Started</h3>
 * <p>
 * Provide a JAR on the classpath (or Daffodil classpath) that contains at least two kinds of classes:
 * a <em>provider</em> class and its associated user-defined function (UDF) class(es). All providers must be
 * registered in the file
 * {@code META-INF/services/org.apache.daffodil.api.udf.UserDefinedFunctionProvider}, regardless of how many
 * providers or UDFs are included.
 * </p>
 *
 * <h4>UDF Implementation</h4>
 *
 * <h5>User Defined Function Provider Classes</h5>
 * <p>
 * The provider class must extend Daffodil's {@code UserDefinedFunctionProvider}. It must implement the
 * {@code getUserDefinedFunctionClasses()} abstract method and return every User Defined Function the provider supplies. The base provider offers
 * {@code createUserDefinedFunction(namespaceURI, name)} to look up and instantiate UDFs that have a no‑argument
 * constructor. If a UDF requires constructor arguments, override the lookup to construct it appropriately.
 * </p>
 * <p>
 * Providers participate in Java’s {@link java.util.ServiceLoader} mechanism and therefore <strong>must</strong> include
 * a file named {@code META-INF/services/org.apache.daffodil.api.udf.UserDefinedFunctionProvider} listing the
 * fully qualified names of the provider class(es) contained in the JAR. Without this file, neither the provider nor
 * its UDFs will be visible to Daffodil.
 * </p>
 *
 * <h5>User Defined Function Classes</h5>
 * <p>
 * Each UDF class must extend Daffodil's {@code UserDefinedFunction}. It contains the functionality to be used in DFDL
 * expressions and must be annotated with {@code UserDefinedFunctionIdentification}, supplying the UDF invocation
 * {@code name} and {@code namespaceURI}. A method named {@code evaluate(...)} must be implemented; it is what Daffodil
 * calls to execute the UDF. <strong>Overloaded</strong> or <strong>void</strong> {@code evaluate} methods are not
 * supported.
 * </p>
 *
 * <h5>User Defined Function Exceptions</h5>
 * <ul>
 *   <li>{@code UserDefinedFunctionProcessingError}: throw to request backtracking during parsing.</li>
 *   <li>{@code UserDefinedFunctionFatalException}: throw to abort processing entirely.</li>
 * </ul>
 * <p>
 * Any other exception type is treated as {@code UserDefinedFunctionFatalException}.
 * </p>
 *
 * <h4>UDF Registration</h4>
 * <p>Typical project layouts:</p>
 *
 * <p><strong>Scala (sbt-style):</strong></p>
 * <pre>{@code
 * src/
 *   main/
 *     scala/
 *       org/
 *         sgoodudfs/
 *           example/
 *             StringFunctions/
 *               StringFunctionsProvider.scala  // UDF provider (may contain UDF classes or define them separately)
 *     resources/
 *       META-INF/
 *         services/
 *           org.apache.daffodil.api.udf.UserDefinedFunctionProvider
 * }</pre>
 *
 * <p><strong>Java (generic):</strong></p>
 * <pre>{@code
 * src/
 *   org/
 *     jgoodudfs/
 *       example/
 *         StringFunctions/
 *           StringFunctionsProvider.java  // UDF provider
 *           Compare.java                  // UDF class
 *           Replace.java                  // UDF class
 *   META-INF/
 *     services/
 *       org.apache.daffodil.api.udf.UserDefinedFunctionProvider
 * }</pre>
 *
 * <p>
 * Each UDF is registered by listing the provider’s fully qualified class name in the text file
 * <code>META-INF/services/org.apache.daffodil.api.udf.UserDefinedFunctionProvider</code>. The <code>META-INF</code>
 * folder must be discoverable from the classpath root or the ServiceLoader will not find it.
 * </p>
 *
 * <h4>UDF Usage</h4>
 * <p>
 * In your DFDL schema, define an xsd namespace or set the default namespace to match the UDF’s annotation {@code namespaceURI}, then
 * invoke using the annotated {@code name}. For example:
 * </p>
 * <pre><code>
 * // annotation on the UDF class
 *  UserDefinedFunctionIdentification(  
 *    name = "replace",
 *    namespaceURI = "http://example.com/ext/stringfunctions") 
 * 
 * // within the schema tag
 * xmlns:sdf="http://example.com/ext/stringfunctions"
 *   
 * // within the DFDL expression
 * ..."{ sdf:replace(., ' ', '_') }"...
 * </code></pre>
 *
 *
 * <h3>Supported Types</h3>
 * <ul>
 *   <li>BigDecimal (Java)</li>
 *   <li>BigInteger (Java)</li>
 *   <li>Boolean (boxed and primitive)</li>
 *   <li>Byte (boxed, primitive, array of primitive)</li>
 *   <li>Double (boxed and primitive)</li>
 *   <li>Float (boxed and primitive)</li>
 *   <li>Integer (boxed and primitive)</li>
 *   <li>Long (boxed and primitive)</li>
 *   <li>Short (boxed and primitive)</li>
 *   <li>String</li>
 * </ul>
 *
 * <h3>Restrictions</h3>
 * <ul>
 *   <li>Overloading unsupported</li>
 *   <li>Void {@code evaluate} functions unsupported</li>
 * </ul>
 */
package org.apache.daffodil.api.udf;
