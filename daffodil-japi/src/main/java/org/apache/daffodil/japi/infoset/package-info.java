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
 * Defines various classes used control the representation of the infoset for
 * parse and unparse. Classes that extend {@link org.apache.daffodil.japi.infoset.InfosetOutputter} are provided to
 * the {@link org.apache.daffodil.japi.DataProcessor#parse(org.apache.daffodil.japi.io.InputSourceDataInputStream, org.apache.daffodil.japi.infoset.InfosetOutputter)} method to deteremine how to output an infoset.
 * These classes are not guaranteed to be thread-safe. Classes that extend
 * {@link org.apache.daffodil.japi.infoset.InfosetInputter} are provided to the {@link org.apache.daffodil.japi.DataProcessor#unparse(org.apache.daffodil.japi.infoset.InfosetInputter, java.nio.channels.WritableByteChannel)} method to
 * determine how to read in an infoset. A new InfosetOutputter is required for
 * each call to unparse().
 */

package org.apache.daffodil.japi.infoset;
