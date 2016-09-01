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

package edu.illinois.ncsa.daffodil.dpath

import com.ibm.icu.text.SimpleDateFormat

class DFDLDateFormat(format: SimpleDateFormat, expectsTZ: Boolean = false) {
  def getFormat = format
  def expectsTimeZone = expectsTZ
}

trait DateFormatters {
  @transient lazy val defaultFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("uuuu-MM-ddxxxxx"), true)
      format
    }
  }
  @transient lazy val withoutTimezoneFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("uuuu-MM-dd"))
      format
    }
  }
}

trait TimeFormatters {
  @transient lazy val defaultFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("HH:mm:ss.SSSSSSxxxxx"), true)
      format
    }
  }

  @transient lazy val noTimeZoneFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("HH:mm:ss.SSSSSS"))
      format
    }
  }

  @transient lazy val noTimeZoneNoFractFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("HH:mm:ss"))
      format
    }
  }

  @transient lazy val withTimeZoneNoFractFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("HH:mm:ssxxxxx"), true)
      format
    }
  }
}

trait DateTimeFormatters {
  @transient lazy val defaultFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxxxx"), true)
      format
    }
  }

  @transient lazy val withFractNoTimeZoneFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ss.SSSSSS"))
      format
    }
  }

  @transient lazy val withTimeZoneNoFractFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ssxxxxx"), true)
      format
    }
  }

  @transient lazy val noTimeZoneNoFractFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ss"))
      format
    }
  }

  @transient lazy val withTimeZoneFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("uuuu-MM-ddxxxxx"), true)
      format
    }
  }

  @transient lazy val dateOnlyFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("uuuu-MM-dd"))
      format
    }
  }
}
