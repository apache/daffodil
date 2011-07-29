package daffodil.xml

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


/**
 * Copied and slightly modified from Heinz M. Kabutz tutorial at
 * http://www.roseindia.net/javatutorials/OutOfMemoryError_Warning_System.shtml
 * 
 */

/* 
 * Created By: Alejandro Rodriguez < alejandr @ ncsa . uiuc . edu > 
 * Date: 2010
 */
import scala.collection.JavaConversions.asBuffer

import javax.management._
import java.lang.management._
import java.text.DecimalFormat

/**
 * This memory warning system will call a listener when we
 * exceed the percentage of available memory specified.
 */
object MemoryWarningSystem {

  private var listeners:List[Listener] = List()

  private val tenuredGenPool = findTenuredGenPool;

  private val df = new DecimalFormat("#.###")

  /**
   * A function to be called when the memory usage threshold is exceeded. Takes as input current memory usage
   * (in bytes) and maximum memory available (in bytes).
   */
  type Listener = (Long,Long) => Unit

  val mbean = ManagementFactory getMemoryMXBean
  val emitter = mbean.asInstanceOf[NotificationEmitter]
  

  emitter addNotificationListener(new NotificationListener() {
      override def handleNotification(n:Notification, hb:Any) {
        if (n.getType() == MemoryNotificationInfo.MEMORY_THRESHOLD_EXCEEDED) {
          val maxMemory = tenuredGenPool.getUsage().getMax
          val usedMemory = tenuredGenPool.getUsage().getUsed
          for (listener <- listeners) 
            listener(usedMemory, maxMemory)
        }
      }
    }, null, null);


  /**
   * Adds a new listener to be notified when the memory usage threshold is exceeded
   */
  def addListener(listener:Listener) =
    listeners = listener :: listeners

  def removeListener(listener:Listener) = 
    listeners = listeners filterNot { _ ==  listener }

  /**
   * Sets the memory usage threshold as a percentage of the maximum amount of memory available.  
   */
  def setPercentageUsageThreshold(percentage:Double) = {
    if (percentage <= 0.0 || percentage > 1.0) 
      throw new IllegalArgumentException("Percentage not in range")
    
    val maxMemory = tenuredGenPool.getUsage.getMax
    val warningThreshold = (maxMemory * percentage).toLong
    tenuredGenPool.setUsageThreshold(warningThreshold)
  }

  /**
   * Returns the amount of memory in used (in bytes)
   */
  def getMemoryUsage = mbean getHeapMemoryUsage

  /**
   * Request the execution of the garbage collector
   */
  def gc = mbean gc

  /**
   * A Tenured Space Pool can be determined by it being of type
   * HEAP and by it being possible to set the usage threshold.
   */
  private def findTenuredGenPool:MemoryPoolMXBean = {
    for (pool <- ManagementFactory.getMemoryPoolMXBeans)
      // I don't know whether this approach is better, or whether
      // we should rather check for the pool name "Tenured Gen"?
      if (pool.getType() == MemoryType.HEAP &&
          pool.isUsageThresholdSupported()) 
        return pool;
    throw new AssertionError("Could not find tenured space");
  }

  def toMb(x:Long) =
     (x.toDouble /(1024*1024)) +"MB"
  
}
