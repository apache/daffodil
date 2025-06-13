/*
 * Copyright (c) 2011-2013, Nate Nystrom
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * Redistributions in binary form must reproduce the above copyright notice, this
 * list of conditions and the following disclaimer in the documentation and/or
 * other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package passera.test

import passera.unsigned._

object UnsignedPerf {
  def main(args: Array[String]) = {

    def time[A](body: => A) = {
      val t0 = System.nanoTime
      val r = body
      val t1 = System.nanoTime
      println(((t1 - t0) / 1e3).toString + " us")
      r
    }

    println {
      time {
        val n = 100000000
        val a = 328923.toUInt
        val b = 713480.toUInt
        var c = 0.toUInt

        var i = 0
        while (i < n) {
          i += 1
          c += a
          c *= b
          c /= a
        }

        c.intValue
      }
    }

    println {
      time {
        val n = 100000000
        val a = 328923
        val b = 713480
        var c = 0

        var i = 0
        while (i < n) {
          i += 1
          c += a
          c *= b
          c /= a
        }

        c
      }
    }
  }
}
