package edu.illinois.ncsa.daffodil.dpath

import com.ibm.icu.text.SimpleDateFormat

class DFDLDateFormat(format: SimpleDateFormat, expectsTZ: Boolean = false){
  def getFormat = format
  def expectsTimeZone = expectsTZ
}

trait DateFormatters {
  lazy val defaultFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("uuuu-MM-ddxxxxx"), true)
      format
    }
  }
  lazy val withoutTimezoneFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("uuuu-MM-dd"))
      format
    }
  }
}

trait TimeFormatters {
  lazy val defaultFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("HH:mm:ss.SSSSSSxxxxx"), true)
      format
    }
  }

  lazy val noTimeZoneFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("HH:mm:ss.SSSSSS"))
      format
    }
  }

  lazy val noTimeZoneNoFractFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("HH:mm:ss"))
      format
    }
  }

  lazy val withTimeZoneNoFractFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("HH:mm:ssxxxxx"), true)
      format
    }
  }
}

trait DateTimeFormatters {
  lazy val defaultFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ss.SSSSSSxxxxx"), true)
      format
    }
  }

  lazy val withFractNoTimeZoneFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ss.SSSSSS"))
      format
    }
  }

  lazy val withTimeZoneNoFractFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ssxxxxx"), true)
      format
    }
  }

  lazy val noTimeZoneNoFractFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("uuuu-MM-dd'T'HH:mm:ss"))
      format
    }
  }

  lazy val withTimeZoneFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("uuuu-MM-ddxxxxx"), true)
      format
    }
  }

  lazy val dateOnlyFormatter = new ThreadLocal[DFDLDateFormat] {
    override def initialValue = {
      val format = new DFDLDateFormat(new SimpleDateFormat("uuuu-MM-dd"))
      format
    }
  }
}
