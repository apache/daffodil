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
// package org.apache.daffodil.runtime2.parser

/*

Note: This whole file is commented out, as a decision to make Runtime 2 be a C++ code generator
instead of Java was made.

That may change, and we might put more than one generator in place since some people do want to generate
Java POJO also.

 */

/**
 * A very basic Java POJO-style parser runtime.
 *
 * The Daffodil runtime2 code generator for Java generates calls
 * to this to actually do parsing.
 *
 * This class is assumed to be one per thread. The data structures
 * reachable from this object do not have to be thread safe as
 * the entire runtime operates on the assumption that each thread
 * is given its own independent such object.
 */
//
//class PState(val dataInputStream: DataInputStream) {
//
//  /**
//   * The currentObject of the infoset is just a POJO.
//   */
//  private var currentObject: Maybe[AnyRef] = Maybe.Nope
//
//  def infoset = {
//    Assert.invariant(currentObject.isDefined)
//    currentObject
//  }
//
//}
//
//class DataInputStream(rbc: java.io.InputStream) {
//
//  def getInt32_BigEndian_MSBF_byteAligned() = ???
//
//}
