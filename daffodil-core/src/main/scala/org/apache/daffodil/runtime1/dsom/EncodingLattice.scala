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

package org.apache.daffodil.runtime1.dsom

/**
 * Technique for analyzing and combining charset encoding
 * information uses a lattice. Top of lattice means
 * "conflicting" information. Bottom of lattice is "no information"
 * and points in between have specific amounts of information.
 * Basic operation is combine two values of the lattice to
 * move up the lattice toward the Top.
 */

sealed abstract class EncodingLattice

/**
 * This is the Top value for the lattice of knowledge
 * about encodings. Mixed as in multiple different encodings
 * or a mixture of binary and text data, or some things
 * not known until runtime.
 */
case object Mixed extends EncodingLattice

/**
 * Contains binary data (only)
 */
case object Binary extends EncodingLattice

/**
 * Means the encoding is determined via a runtime expression.
 */
case object Runtime extends EncodingLattice

/**
 * NoText is the bottom of the lattice. We have no information
 * here. Means the item could have text, but just so happens to not
 * have any, so regardless of what it's encoding is, it doesn't
 * interfere with contained children and parents having a
 * common encoding. Example: A sequence with no alignment region,
 * and no delimiters. It's not binary, it has no text. It's
 * nothing really.
 */
case object NoText extends EncodingLattice

/**
 * Means we have a named encoding.
 */
case class NamedEncoding(name: String) extends EncodingLattice
