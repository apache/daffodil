Passera
=======

This is an implementation of unsigned integers for Scala.
This is still very much a work in progress.  
Much of it was written a while ago as an exercise in learning Scala.

The library provides:
- UInt
- ULong
- UByte
- UShort

The classes are implemented by boxing Int, Long, Byte, Short, respectively.

To use:

    scala> import passera.unsigned._

    scala> (-1).toUInt
    res0: passera.unsigned.UInt = 4294967295

    scala> res0 + 1
    res1: Int = 0


Credits
-------

The code was written by Nate Nystrom (nate.nystrom@usi.ch).

TODO
----

Add support for value classes.

Refactor code to remove redundant ULong code.

More tests.
