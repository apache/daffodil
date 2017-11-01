/* Copyright (c) 2016 Tresys Technology, LLC. All rights reserved.
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

package org.apache.daffodil.util

import org.junit.Test
import org.junit.Assert._

class TestMisc {

  @Test def testIsAllUpper() {
    assertTrue(Misc.isAllUpper("A", 0))
    assertFalse(Misc.isAllUpper("a", 0))
    assertTrue(Misc.isAllUpper("AB", 0))
    assertFalse(Misc.isAllUpper("Ab", 0))
    assertTrue(Misc.isAllUpper("ABC", 0))
    assertFalse(Misc.isAllUpper("ABc", 0))

    assertTrue(Misc.isAllUpper("AB", 1))
    assertFalse(Misc.isAllUpper("Ab", 1))
    assertTrue(Misc.isAllUpper("ABC", 1))
    assertFalse(Misc.isAllUpper("ABc", 1))
  }

  @Test def testToInitialLowerUnlessAllUpper() {
    assertEquals("fooBar", Misc.toInitialLowerCaseUnlessAllUpperCase("FooBar"))
    assertEquals("FOOBAR", Misc.toInitialLowerCaseUnlessAllUpperCase("FOOBAR"))
    assertEquals("fOOBAR", Misc.toInitialLowerCaseUnlessAllUpperCase("fOOBAR"))
    assertEquals("foobar", Misc.toInitialLowerCaseUnlessAllUpperCase("Foobar"))
  }
}
