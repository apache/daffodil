package edu.illinois.ncsa.daffodil.util

/**
 *  Using Scala 2.10's Value Classes to make a Some/None style option type
 *  which does not allocate a boxed object.
 *
 *  To tell the difference these two items are called One(v) and Nope.
 */
final class Maybe[+T] private (val v: Any) extends AnyVal {
  import Maybe._
  def get: T = if (isDefined) value.asInstanceOf[T] else throw new NoSuchElementException("Nope.get")
  @inline def value: T = v.asInstanceOf[T]

  // ?? I don't know how isEmpty can work. It requires the value to be a reference
  // or a value, but there is no way to tell the difference without 
  // storing more than just the value. 
  // 
  @inline def isEmpty: Boolean = NopeValue eq value.asInstanceOf[AnyRef]
  @inline def isDefined: Boolean = !isEmpty
  @inline def nonEmpty = isDefined
  @inline def contains[U >: T](elem: U): Boolean = !isEmpty && value == elem
  @inline def exists(p: T => Boolean): Boolean = !isEmpty && p(get)
  @inline def forall(p: T => Boolean): Boolean = isEmpty || p(get)
  @inline def orElse[U >: T](alternative: => Maybe[U]): Maybe[U] = if (isEmpty) alternative else this
  @inline def collect[U](pf: PartialFunction[T, U]): Maybe[U] = if (!isEmpty && pf.isDefinedAt(get)) One(pf(get)) else Nope
  @inline def iterator: Iterator[T] = if (isEmpty) collection.Iterator.empty else collection.Iterator.single(get)
  @inline def toList: List[T] = if (isEmpty) List() else new ::(get, Nil)
  def toRight[X](left: => X) = if (isEmpty) Left(left) else Right(value)
  def toLeft[X](right: => X) = if (isEmpty) Right(right) else Left(value)
  def getOrElse[U >: T](default: => U): U = if (isEmpty) default else get
  def orNull[U >: T](implicit ev: Null <:< U): U = get
  def filter(p: T => Boolean): Maybe[T] = if (isEmpty || p(get)) this else Nope
  def filterNot(p: T => Boolean): Maybe[T] = if (isEmpty || !p(get)) this else Nope
  def withFilter(f: T => Boolean): Maybe[T] = filter(f)
  def map[U](f: T => U): Maybe[U] = if (isEmpty) Nope else One(f(get))
  def flatMap[U](f: T => Maybe[U]): Maybe[U] = if (isEmpty) Nope else f(get)
  def foreach[U](f: T => U): Unit = if (!isEmpty) f(get)
  def fold[U](ifEmpty: => U)(f: T => U): U = if (isEmpty) ifEmpty else f(get)
  def flatten[U](implicit ev: T <:< Maybe[U]): Maybe[U] = if (isEmpty) Nope else ev(get)
  def toScalaOption: scala.Option[T] = if (isEmpty) scala.None else scala.Some(get)
  override def toString = if (isEmpty) "Nope" else "One(" + get + ")"
}

object Maybe {

  @inline
  def apply[T](value: T) = if (value == null) Nope else some(value)

  @inline
  def some[T](value: T) = new Maybe[T](value)

  def empty[T] = new Maybe[T](NopeValue)

  val Nope = new Maybe[Nothing](NopeValue)

  object One {
    @inline
    def apply[T](value: T) = Maybe(value)

    // If the pattern matching is going to box an object then this is hardly
    // worth using. 
    //
    def unapply[T](value: Maybe[T]) = if (value.isDefined) scala.Some(value.get) else scala.None
  }
  private object NopeValue
}

/**
 * Below is a performance study of Int vs. Maybe[Int] vs. Option[Int]
 *
 * Maybe is faster than Some/None
 */
//private object Tak extends App {
//  import Maybe._
//
//  def calibrate = {
//    if (takeons == 0.0) {
//      testTak
//    }
//  }
//
//  var takeons = 0.0 // value on Mike Beckerle's laptop
//
//  var callCount: Long = 0
//
//  // Original Tak function
//  def tak1(x: Int, y: Int, z: Int): Int = {
//    callCount += 1
//    if (y < x)
//      tak1(
//        tak1(x - 1, y, z),
//        tak1(y - 1, z, x),
//        tak1(z - 1, x, y))
//    else
//      z
//  }
//
//  // Tak, but passing Scala Option type Some objects.
//  def taks(sx: Option[Int], sy: Option[Int], sz: Option[Int]): Option[Int] = {
//    val x = sx.get
//    val y = sy.get
//    val z = sz.get
//    callCount += 1
//    if (y < x)
//      taks(
//        taks(Some(x - 1), Some(y), Some(z)),
//        taks(Some(y - 1), Some(z), Some(x)),
//        taks(Some(z - 1), Some(x), Some(y)))
//    else
//      sz
//  }
//
//  // Tak, but passing scala 2.10 maybe One "value class" objects
//  // which should be unboxed.
//  def tak(sx: Maybe[Int], sy: Maybe[Int], sz: Maybe[Int]): Maybe[Int] = {
//    val x = sx.value
//    val y = sy.value
//    val z = sz.value
//    callCount += 1
//    if (y < x)
//      tak(
//        tak(One(x - 1), One(y), One(z)),
//        tak(One(y - 1), One(z), One(x)),
//        tak(One(z - 1), One(x), One(y)))
//    else
//      sz
//  }
//
//  def testTak() {
//    println("Calibrating takeon units")
//    callCount = 0
//    val x = 21
//    val y = 3
//    val z = 21
//    var nanos = Timer.getTimeNS { tak1(x, y, z) }
//    println("tak call count = " + callCount + " in " + nanos + "ns")
//    takeons = (1.0 * nanos) / callCount
//    println("Under current load, 1 CPU of this system executes " + takeons + " nanoseconds per tak call.")
//    println("So on this system, currently, 1 takeon = " + takeons + "ns")
//    println("Done calibrating")
//    var t0 = System.nanoTime
//    tak(One(x), One(y), One(z))
//    var t1 = System.nanoTime
//    val maybenanos = t1 - t0
//    println("maybe objects are %s times slower".format(maybenanos.toDouble / nanos))
//
//    t0 = System.nanoTime
//    taks(Some(x), Some(y), Some(z))
//    t1 = System.nanoTime
//    val somenanos = t1 - t0
//    println("Some objects are %s times slower".format(somenanos.toDouble / nanos))
//  }
//
//  val res = testTak()
//}