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

package org.apache.daffodil.util

import scala.reflect.macros.blackbox.Context

object TimeTrackerMacros {

  def trackMacro(c: Context)(name: c.Tree)(body: c.Tree) = {
    import c.universe._

    val startTime = TermName(c.freshName)
    val endTime = TermName(c.freshName)
    val childrenTime = TermName(c.freshName)
    val timeTaken = TermName(c.freshName)
    val selfTime = TermName(c.freshName)
    val sectionTime = TermName(c.freshName)
    val result = TermName(c.freshName)
    val key = TermName(c.freshName)

    q"""
    {
      val $startTime = System.nanoTime
      TimeTracker.childrenTimeStack.push(0)
      
      val $result = try {
        $body
      } finally {
        val $endTime = System.nanoTime
        val $timeTaken = $endTime - $startTime
        val $childrenTime = TimeTracker.childrenTimeStack.pop()
        val $selfTime = $timeTaken - $childrenTime

        val $key = $name
        val $sectionTime = TimeTracker.sectionTimes.get($key)
        if ($sectionTime == null) {
          TimeTracker.sectionTimes.put($key, new TimeTracker.SectionTime($selfTime, 1))
        } else {
          $sectionTime.time += $selfTime
          $sectionTime.count += 1
        }

        if (!TimeTracker.childrenTimeStack.isEmpty) {
          TimeTracker.childrenTimeStack.push(TimeTracker.childrenTimeStack.pop + $timeTaken) 
        }
      }

      $result
    }
    """
  }
}
