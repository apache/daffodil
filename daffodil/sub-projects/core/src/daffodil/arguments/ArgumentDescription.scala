package daffodil.arguments

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
 * Created by: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu >
 * Date: 2010
 */

/**
 * A description of an argument to be parsed by ArgumentParser
 * @see ArgumentParser
 * @param key the name or id of the argument
 * @param longName the long version of the argument to be passed in the command line
 * @param shortName the short version (single letter) version of the argument to be passed in the command line
 * @param argument whether this argument takes a parameter in the command line
 * @param number whether this argument can be specified multiple 0, 1 or multiple times in the command line
 *
 * @author Alejandro Rodriguez
 * @version 1
 */
class ArgumentDescription(val key:String,val longName:String,
                          val shortName:String,val argument:Boolean,val number:ArgumentNumber) {

  /** The default value for this argument */
  var default:Option[String] = None


  /**A description of an argument to be parsed by ArgumentParser
   * @see ArgumentParser
   * @param key the name or id of the argument
   * @param longName the long version of the argument to be passed in the command line
   * @param shortName the short version (single letter) version of the argument to be passed in the command line
   * @param argument whether this argument takes a parameter in the command line
   * @param number whether this argument can be specified multiple 0, 1 or multiple times in the command line
   * @param default the default value for this argument
   */
  def this(key:String,longName:String,shortName:String,argument:Boolean,number:ArgumentNumber,default:String) = {
    this(key,longName,shortName,argument,number)
    this.default = Some(default)
  }

}