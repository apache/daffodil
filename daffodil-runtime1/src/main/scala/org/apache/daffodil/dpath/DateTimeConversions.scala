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

package org.apache.daffodil.dpath

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
