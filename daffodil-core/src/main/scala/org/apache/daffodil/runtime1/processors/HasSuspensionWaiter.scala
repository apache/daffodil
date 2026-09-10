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

package org.apache.daffodil.runtime1.processors

/**
 * Mixed into any object a Suspension can register a targeted wake-up
 * against: lazily allocates its SuspensionWaiter, since most instances of
 * any such host are never suspended on, and exposes the notify/clear
 * operations without forcing that allocation just to find it unneeded.
 */
trait HasSuspensionWaiter {

  private var _suspensionWaiter: SuspensionWaiter = null

  def suspensionWaiter: SuspensionWaiter = {
    if (_suspensionWaiter eq null) {
      _suspensionWaiter = new SuspensionWaiter
    }
    _suspensionWaiter
  }

  protected def notifySuspensionWaiterIfAllocated(): Unit = {
    if (_suspensionWaiter ne null) {
      _suspensionWaiter.notifySuspensions()
    }
  }

  protected def clearSuspensionWaiterIfAllocated(): Unit = {
    if (_suspensionWaiter ne null) {
      _suspensionWaiter.clear()
    }
  }
}
