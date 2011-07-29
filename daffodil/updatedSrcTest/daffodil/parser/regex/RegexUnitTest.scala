package daffodil.parser.regex

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite

/**
 * Copyright (c) 2010 NCSA.  All rights reserved.
 * Developed by: NCSA Cyberenvironments and Technologies 
 *               University of Illinois at Urbana-Champaign
 *               http://cet.ncsa.uiuc.edu/
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal with the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimers.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimers in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the names of NCSA, University of Illinois, nor the names of its 
 *     contributors may be used to endorse or promote products derived from this 
 *     Software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 * CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * WITH THE SOFTWARE.
 *
 */
 
/* 
 * Developer: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu > 
 * Date: 2010
 */

class RegexUnitTest  extends FunSuite with ShouldMatchers {

  test ("non-optional non-repeating") {

    val p = new Regex

    //ab(c|d|ef)g

    p addAlternatives (List("a"),false,false)

    p addAlternatives (List("b"),false,false)

    p addAlternatives (List("c","d","ef"),false,false)

    p addAlternatives (List("g"),false,false)

    p addAlternatives (List("h"),false,false)

    val q = p compile (false)

    q.matches("abcgh") should be (true)

    q.matches("abdgh") should be (true)

    q.matches("abefgh") should be (true)

    q.matches("abgh") should be (false)

    q.matches("acgh") should be (false)

    q.matches("axcgh") should be (false)

  }

  test ("optional non-repeating") {

    val p = new Regex

    //ab(c|d|ef)?g

    p addAlternatives (List("a"),false,false)

    p addAlternatives (List("b"),false,false)

    p addAlternatives (List("1","2","34","5"),true,false)

    p addAlternatives (List("c"),false,false)

    val q = p compile (false)

    q.matches("ab1c") should be (true)

    q.matches("ab2c") should be (true)

    q.matches("ab34c") should be (true)

    q.matches("ab5c") should be (true)

    q.matches("ab1") should be (false)

    q.matches("a1c") should be (false)

    q.matches("ax1c") should be (false)


    q.matches("abc") should be (true)

    q.matches("ab11c") should be (false)

  }

  test ("optional repeating") {

    val p = new Regex

    //ab(c|d|ef)*g

    p addAlternatives (List("a"),false,false)

    p addAlternatives (List("b"),false,false)

    p addAlternatives (List("c","d","ef"),true,true)

    p addAlternatives (List("g"),false,false)

    val q = p compile (false)

    q.matches("abcg") should be (true)

    q.matches("abdg") should be (true)

    q.matches("abefg") should be (true)

    q.matches("abc") should be (false)

    q.matches("acg") should be (false)

    q.matches("axcg") should be (false)


    q.matches("abg") should be (true)

    q.matches("abccg") should be (true)

    q.matches("abcefg") should be (true)

    q.matches("abcefdg") should be (true)

  }

  test ("non-optional repeating") {

    val p = new Regex

    //ab(c|d|ef)+g

    p addAlternatives (List("a"),false,false)

    p addAlternatives (List("b"),false,false)

    p addAlternatives (List("c","d","ef"),false,true)

    p addAlternatives (List("g"),false,false)

    val q = p compile (false)

    q.matches("abcg") should be (true)

    q.matches("abdg") should be (true)

    q.matches("abefg") should be (true)

    q.matches("abc") should be (false)

    q.matches("acg") should be (false)

    q.matches("axcg") should be (false)


    q.matches("abg") should be (false)

    q.matches("abccg") should be (true)

    q.matches("abcefg") should be (true)

    q.matches("abcefdg") should be (true)

  }


  test ("consecutive classes non-optional non-repeating") {

    val p = new Regex

    //ab(c|d)(e|f)g

    p addAlternatives (List("a"),false,false)

    p addAlternatives (List("b"),false,false)

    p addAlternatives (List("c","d"),false,false)

    p addAlternatives (List("e","f"),false,false)

    p addAlternatives (List("g"),false,false)

    val q = p compile (false)

    q.matches("abceg") should be (true)

    q.matches("abcfg") should be (true)

    q.matches("abdeg") should be (true)

    q.matches("abdfg") should be (true)

    q.matches("aceg") should be (false)

    q.matches("abfg") should be (false)

    q.matches("abdg") should be (false)

    q.matches("abdg") should be (false)

    q.matches("abcdg") should be (false)

    q.matches("abcceg") should be (false)


  }

  test ("consecutive classes optional repeating") {

    val p = new Regex

    //ab(c|d)*(e|f)*g

    p addAlternatives (List("a"),false,false)

    p addAlternatives (List("b"),false,false)

    p addAlternatives (List("c","d"),true,true)

    p addAlternatives (List("e","f"),true,true)

    p addAlternatives (List("g"),false,false)

    val q = p compile (false)

    q.matches("abceg") should be (true)

    q.matches("abcfg") should be (true)

    q.matches("abdeg") should be (true)

    q.matches("abdfg") should be (true)


    q.matches("abcceg") should be (true)

    q.matches("abcffg") should be (true)

    q.matches("abddeg") should be (true)

    q.matches("abddfffg") should be (true)


    q.matches("abcecg") should be (false)

    q.matches("abefg") should be (true)

    q.matches("abcg") should be (true)

    q.matches("abfcg") should be (false)



    q.matches("abddfffg") should be (true)
    
  }

  test ("last char is optional") {

    val p = new Regex

    //ab(c|d)*(e|f)*

    p addAlternatives (List("a"),false,false)

    p addAlternatives (List("b"),false,false)

    p addAlternatives (List("c","d"),true,true)

    p addAlternatives (List("e","f"),true,true)

    val q= p compile (false)

    q.matches("ab") should be (true)

    q.matches("abc") should be (true)

    q.matches("abcc") should be (true)

    q.matches("abdd") should be (true)

    q.matches("abe") should be (true)

    q.matches("abee") should be (true)

    q.matches("abff") should be (true)

    q.matches("abccee") should be (true)


    q.matches("abb") should be (false)

    q.matches("a") should be (false)

    q.matches("abX") should be (false)

    q.matches("abcec") should be (false)

  }


  test ("starts with 1") {

    val p = new Regex

    //ab(c|d|ef)g

    p addAlternatives (List("a"),false,false)

    p addAlternatives (List("b"),false,false)

    p addAlternatives (List("c","d","ef"),false,false)

    p addAlternatives (List("g"),false,false)

    p addAlternatives (List("h"),false,false)

    val q = p compile (false)

    q.startsWith("abc") should be (true)

    q.startsWith("abd") should be (true)

    q.startsWith("abg") should be (false)

    q.startsWith("abcgh") should be (true)

    q.startsWith("abcghx") should be (false)

  }
}