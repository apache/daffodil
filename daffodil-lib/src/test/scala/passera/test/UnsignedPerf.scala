package passera.test

import passera.unsigned._

object UnsignedPerf {
  def main(args: Array[String]) = {

    def time[A](body: => A) = {
      val t0 = System.nanoTime
      val r = body
      val t1 = System.nanoTime
      println((t1 - t0) / 1e3 + " us")
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
