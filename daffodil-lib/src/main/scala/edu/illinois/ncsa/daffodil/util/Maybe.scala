package edu.illinois.ncsa.daffodil.util

/**
 *  Using Scala 2.10's Value Classes to make a Some/None style option type
 *  which does not allocate a boxed object.
 *
 *  To tell the difference these two items are called One(v) and Nope.
 */
final class Maybe[+T] @inline private (val v: Any) extends AnyVal {
  import Maybe._
  @inline final def get: T = if (isDefined) value.asInstanceOf[T] else noneGet
  @inline final def value: T = v.asInstanceOf[T]
  final def noneGet = throw new NoSuchElementException("Nope.get")
  // ?? I don't know how isEmpty can work. It requires the value to be a reference
  // or a value, but there is no way to tell the difference without 
  // storing more than just the value. 
  // 
  @inline final def isEmpty: Boolean = NopeValue eq value.asInstanceOf[AnyRef]
  @inline final def isDefined: Boolean = !isEmpty
  @inline final def nonEmpty = isDefined
  @inline final def contains[U >: T](elem: U): Boolean = !isEmpty && value == elem
  @inline final def exists(p: T => Boolean): Boolean = !isEmpty && p(get)
  @inline final def forall(p: T => Boolean): Boolean = isEmpty || p(get)
  @inline final def orElse[U >: T](alternative: => Maybe[U]): Maybe[U] = if (isEmpty) alternative else this
  @inline final def collect[U](pf: PartialFunction[T, U]): Maybe[U] = if (!isEmpty && pf.isDefinedAt(get)) One(pf(get)) else Nope
  @inline final def iterator: Iterator[T] = if (isEmpty) collection.Iterator.empty else collection.Iterator.single(get)
  @inline final def toList: List[T] = if (isEmpty) List() else new ::(get, Nil)
  final def toRight[X](left: => X) = if (isEmpty) Left(left) else Right(value)
  final def toLeft[X](right: => X) = if (isEmpty) Right(right) else Left(value)
  final def getOrElse[U >: T](default: => U): U = if (isEmpty) default else get
  final def orNull[U >: T](implicit ev: Null <:< U): U = get
  final def filter(p: T => Boolean): Maybe[T] = if (isEmpty || p(get)) this else Nope
  final def filterNot(p: T => Boolean): Maybe[T] = if (isEmpty || !p(get)) this else Nope
  final def withFilter(f: T => Boolean): Maybe[T] = filter(f)
  final def map[U](f: T => U): Maybe[U] = if (isEmpty) Nope else One(f(get))
  final def flatMap[U](f: T => Maybe[U]): Maybe[U] = if (isEmpty) Nope else f(get)
  final def foreach[U](f: T => U): Unit = if (!isEmpty) f(get)
  final def fold[U](ifEmpty: => U)(f: T => U): U = if (isEmpty) ifEmpty else f(get)
  final def flatten[U](implicit ev: T <:< Maybe[U]): Maybe[U] = if (isEmpty) Nope else ev(get)
  final def toScalaOption: scala.Option[T] = if (isEmpty) scala.None else scala.Some(get)
  override final def toString = if (isEmpty) "Nope" else "One(" + get + ")"
}

object Maybe {

  import scala.language.implicitConversions

  /**
   *  implicitly treat as iterator/sequence/list (Scala's Option type has this)
   */
  implicit def toIterator[T](m: Maybe[T]) = m.toList

  /**
   * implicitly convert Option type to Maybe type.
   *
   * The conversion the other way must be explicit by calling toScalaOption
   */
  implicit def toMaybe[T](o: Option[T]) = o match {
    case None => Nope
    case Some(x) => One(x)
  }

  @inline
  final def apply[T](value: T) = if (value == null) Nope else some(value)

  @inline
  final def some[T](value: T) = new Maybe[T](value)

  def empty[T] = Nope

  val Nope = new Maybe[Nothing](NopeValue)

  object One {
    @inline
    final def apply[T](value: T) = Maybe(value)

    // If the pattern matching is going to box an object then this is hardly
    // worth using. 
    //
    // def unapply[T](value: Maybe[T]) = if (value.isDefined) scala.Some(value.get) else scala.None
  }
  private object NopeValue extends Serializable {
    override def toString = "Nope"
  }
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
//  def tak1(x: Long, y: Long, z: Long): Long = {
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
//  def taks(sx: Option[Long], sy: Option[Long], sz: Option[Long]): Option[Long] = {
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
//  def tak(sx: Maybe[Long], sy: Maybe[Long], sz: Maybe[Long]): Maybe[Long] = {
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
//    val x = 20L
//    val y = 4L
//    val z = 20L
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
// }