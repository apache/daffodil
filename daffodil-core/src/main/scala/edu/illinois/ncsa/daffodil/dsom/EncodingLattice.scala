package edu.illinois.ncsa.daffodil.dsom

/* Copyright (c) 2012-2013 Tresys Technology, LLC. All rights reserved.
 *
 * Developed by: Tresys Technology, LLC
 *               http://www.tresys.com
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal with
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 * 
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 * 
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *  3. Neither the names of Tresys Technology, nor the names of its contributors
 *     may be used to endorse or promote products derived from this Software
 *     without specific prior written permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
 * SOFTWARE.
 */
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