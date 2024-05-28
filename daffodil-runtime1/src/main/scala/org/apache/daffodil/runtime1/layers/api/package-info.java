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
 * <h2>Daffodil Runtime1 Layers API Package</h2>
 *
 * This package provides base classes for creating Layers, which provide a means of
 * algorithmic operations on the data stream that cannot be expressed using
 * regular DFDL properties.
 * <p>
 * There are two kinds of layers:
 * <ul><li>transforming layers - such as a base64 decoder/encoder for parts of a textual format</li>
 * <li>checksum layers - such as computing CRC32 over a message header</li>
 * </ul>
 * Both run an algorithm over a part of the data stream.
 * <p>
 * Layers are implemented in Scala or Java as small (usually) Jar files that are dynamically loaded from the CLASSPATH in
 * response to a DFDL schema that uses that layer via the {@code dfdlx:layer} property.
 * <p>
 * See the <a href="http://daffodil.apache.org/layers">Layer Usage Documentation</a> for how a layer is
 * used from a Daffodil DFDL schema.
 * <p>
 * This API documentation is focused on the programming required to create a new layer implementation
 * as a custom plug-in for Daffodil.
 * <p>
 * {@link org.apache.daffodil.runtime1.layers.api.Layer} is the general abstract base class
 * used to define any layer, but most commonly it is used for transforming layers.
 * <p>
 * {@link org.apache.daffodil.runtime1.layers.api.ChecksumLayer} is an abstract base class
 * derived from {@link org.apache.daffodil.runtime1.layers.api.Layer}, and further specialized for defining
 * checksum layers.
 *
 * <h3> About Testing </h3>
 *
 * The Daffodil test code base includes tests for many ways that
 * an API user can goof up the definition of a layer class.
 * For example there are tests for calling {@code processingError}, {@code runtimeSchemaDefinitionError}, and throwing
 * an {@code Exception} from every place a custom-defined Layer could cause these.
 * Processing errors cause the parser to backtrack in all sensible cases.
 * That is to say that if a layer is parsing data, and the data it encounters is not a match for that layer, then
 * a properly written layer will issue a processing error, and the parser will backtrack, allowing the format to try
 * other alternatives for parsing that data.
 *
 * <h3>Compatibility with Daffodil 3.7.0 and prior versions of Apache Daffodil</h3>
 *
 * This new Layer API is entirely incompatible with schemas or layer code
 * from Daffodil 3.7.0 and all prior versions of Daffodil.
 * <p>
 * The layer feature was just an experimental feature in earlier versions of Daffodil, so we reserved
 * the right to change it, and for Daffodil 3.8.0 it has changed radically based on what we learned from the
 * earlier experimentation.
 * <p>
 * It is our intention that this Layer API (introduced in Daffodil 3.8.0) will prove to be stable
 * and supportable long term.
 */
package org.apache.daffodil.runtime1.layers.api;
