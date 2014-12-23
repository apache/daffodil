package edu.illinois.ncsa.daffodil.dpath

import edu.illinois.ncsa.daffodil.processors._
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import edu.illinois.ncsa.daffodil.exceptions._
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._
import edu.illinois.ncsa.daffodil.xml.RefQName
import edu.illinois.ncsa.daffodil.util.Misc
import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.xml.XMLUtils
import edu.illinois.ncsa.daffodil.util.OnStack
import edu.illinois.ncsa.daffodil.util.PreSerialization
import com.ibm.icu.text.SimpleDateFormat
import com.ibm.icu.util.Calendar
import scala.math.BigDecimal.RoundingMode
import edu.illinois.ncsa.daffodil.util.Bits
import edu.illinois.ncsa.daffodil.compiler.DaffodilTunableParameters
import java.text.ParsePosition
import com.ibm.icu.util.DFDLCalendar
import com.ibm.icu.util.SimpleTimeZone
import com.ibm.icu.util.TimeZone
import java.nio.ByteBuffer
import com.ibm.icu.util.DFDLDateTime
import com.ibm.icu.util.DFDLDate
import com.ibm.icu.util.DFDLTime
import AsIntConverters._

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
