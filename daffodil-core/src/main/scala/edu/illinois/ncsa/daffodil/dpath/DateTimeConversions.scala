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

trait DateFormatters {
  lazy val defaultFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("uuuu-MM-ddxxxxx")
      format
    }
  }
  lazy val withoutTimezoneFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("uuuu-MM-dd")
      format
    }
  }
}

trait TimeFormatters {
  lazy val defaultFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("HH:mm:ss.SSSSSSxxxxx")
      format
    }
  }

  lazy val noTimeZoneFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("HH:mm:ss.SSSSSS")
      format
    }
  }

  lazy val noTimeZoneNoFractFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("HH:mm:ss")
      format
    }
  }

  lazy val withTimeZoneNoFractFormatter = new ThreadLocal[SimpleDateFormat] {
    override def initialValue = {
      val format = new SimpleDateFormat("HH:mm:ssxxxxx")
      format
    }
  }
}
