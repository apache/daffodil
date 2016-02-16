/* Copyright (c) 2012-2015 Tresys Technology, LLC. All rights reserved.
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

package edu.illinois.ncsa.daffodil.externalvars

import edu.illinois.ncsa.daffodil.util._

import junit.framework.Assert._
import org.junit.Test
import edu.illinois.ncsa.daffodil.Implicits._; object INoWarn2 { ImplicitsSuppressUnusedImportWarning() }
import org.junit.Test
import edu.illinois.ncsa.daffodil.xml._
import scala.util.Success

class TestExternalVariablesNew extends Logging {

  @Test def testQNameForIndividualVars() = {
    // This test just verifies that we're getting back
    // the right values for the namespace and variable name.
    //
    // This is required functionality for when individual vars
    // are passed in via the CLI using the -D option.
    //
    val varWithNS = "{someNS}varWithNS"
    val varNoNS = "{}varNoNS"
    val varGuessNS = "varGuessNS"

    val Success(qWithNS) = QName.refQNameFromExtendedSyntax(varWithNS)
    val Success(qNoNS) = QName.refQNameFromExtendedSyntax(varNoNS)
    val Success(qGuessNS) = QName.refQNameFromExtendedSyntax(varGuessNS)

    assertEquals(NS("someNS"), qWithNS.namespace)
    assertEquals("varWithNS", qWithNS.local)
    assertEquals(varWithNS, qWithNS.toString)

    assertEquals(NoNamespace, qNoNS.namespace)
    assertEquals("varNoNS", qNoNS.local)
    assertEquals(varNoNS, qNoNS.toString)

    assertEquals(UnspecifiedNamespace, qGuessNS.namespace)
    assertEquals("varGuessNS", qGuessNS.local)
  }

}
