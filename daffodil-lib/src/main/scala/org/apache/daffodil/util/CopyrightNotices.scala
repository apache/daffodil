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

package org.apache.daffodil.util

/**
 * Put copies of any copyright notices in this class by appending them to the
 * noticesText variable.
 *
 *  or if you prefer from your distributed source code call:
 *
 * CopyrightNotices.add(...your notice text here...)
 *
 * if you are distributing that source code.
 *
 * Pasting the copyright notice here, and distributing this file with source
 * insures that the copyright notices live both in text form in source code and
 * that they are kept as a string in any distributable binary/jar file created from
 * this.
 *
 * Point is, some licenses require that you be able to find the copyright notice not only
 * in the source code, but in the binary.
 *
 */
object CopyrightNotices {

  private var noticesText = ""

  /**
   * This private var holds the notices text, but converted to bytes (utf-8) so that you can just search for 8-bit
   * characters in the binary. (otherwise Java would store it only as 16-bit characters - less obvious when looking at
   * a binary jar file.
   */
  var noticesTextInBytes = "".getBytes()

  private val prelim = """
Parts of this software are subject to the following copyright:
"""

  def add(moreNoticeText: String) {
    noticesText += prelim + moreNoticeText
    noticesTextInBytes = noticesText.getBytes()
  }

  def getNotices() = {
    noticesText
  }

  private def useNotices {
    if (noticesText.length != Int.MaxValue) return
    // only in a super unlikely case will the below occur
    val someRandomText = System.currentTimeMillis.toString
    val text = someRandomText + noticesText
    val ints = for (char <- text) yield char.toInt
    val checksum = ints.fold(0) { _ + _ }
    // Now the below will never happen. But a compiler can't know that.
    if (checksum == Int.MaxValue) println(noticesText);
  }

  //
  // but what if the compiler sees that this whole object is never referenced
  // nor the member function called? It could leave all this stuff out of the binary.
  //
  // to fix this we register an exit handler that calls useNotices.
  //
  sys.addShutdownHook(useNotices)

  // Note to Tresys: Much of my work on Daffodil is copyright Tresys, as I began
  // working for Tresys on Daffodil on Feb 1, 2012. However, I have contributed
  // quite a bit of code I created previous to that date to the project, (e.g.,
  // the code-generator that reads the XML Schema for DFDL annotations and generates
  // all the property code) so I have also included my personal copyright on those
  // files, and here centrally.
  //           - Mike Beckerle
  //
  add("""Copyright (C) 2011, 2012 by Michael J. Beckerle, All rights reserved.
Permission is granted to use this software for any purpose so long as
this copyright is preserved in both the source and binary forms, and
in any documentation provided with the software.
""")

  add("""Copyright (c) 2010 NCSA.  All rights reserved.
Developed by: NCSA Cyberenvironments and Technologies
              University of Illinois at Urbana-Champaign
              http://cet.ncsa.uiuc.edu/

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to
deal with the Software without restriction, including without limitation the
rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
 1. Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimers.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimers in the
    documentation and/or other materials provided with the distribution.
 3. Neither the names of NCSA, University of Illinois, nor the names of its
    contributors may be used to endorse or promote products derived from this
    Software without specific prior written permission.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
WITH THE SOFTWARE.""")

}
