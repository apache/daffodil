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

package org.apache.daffodil.lib.util

trait TimeTrackerUsingMacrosMixin {

  /**
   * Used to measure a section of code that might get called multiple times.
   * This keeps track of the total amount of time spent in that section and how
   * many times that section was run. A name is provided by the caller to
   * identify the section. Additionally, if a section that is being tracked
   * calls another section that is being tracked (i.e. nested TimeTracker.track
   * calls) the time spent in the inner section is not included in the outer
   * section. Note that because of this feature, the use of the track function
   * is not thread safe.
   *
   * For example,
   *
   * TimeTracker.track("code section") {
   * // code to track goes here
   * }
   *
   * Once processing is complete, a call to logTimes will display the stats
   * about the tracked sections.
   *
   * inline so as to avoid the overhead of allocating a closure for the body
   * lazy arg.
   */
  def track[A](name: String)(body: => A): A = macro TimerMacros.trackMacro
}
