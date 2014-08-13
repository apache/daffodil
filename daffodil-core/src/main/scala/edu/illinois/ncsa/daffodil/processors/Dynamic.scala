package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom.CompiledExpression
import edu.illinois.ncsa.daffodil.util.Maybe
import edu.illinois.ncsa.daffodil.util.Maybe._

trait Dynamic {

  type CachedDynamic[A] = Either[CompiledExpression, A]

  // Returns an Either, with Right being the value of the constant, and the
  // Left being the a non-constant compiled expression. The conv variable is
  // used to convert the constant value to a more usable form, and perform and
  // SDE checks. This should be called during initialization/compile time. Not
  // during runtime.
  def cacheConstantExpression[A](e: CompiledExpression)(conv: (Any) => A): CachedDynamic[A] = {
    if (e.isConstant) {
      val v: Any = e.constant
      Right(conv(v))
    } else {
      Left(e)
    }
  }

  def cacheConstantExpression[A](oe: Maybe[CompiledExpression])(conv: (Any) => A): Maybe[CachedDynamic[A]] = {
    //oe.map { e => cacheConstantExpression[A](e)(conv) }
    if (oe.isDefined) One(cacheConstantExpression[A](oe.get)(conv))
    else Nope
  }

  def cacheConstantExpression[A](listOfE: List[CompiledExpression])(conv: (Any) => A): List[CachedDynamic[A]] = {
    listOfE.map { e => cacheConstantExpression[A](e)(conv) }
  }

  // For any expression that couldn't be evaluated in cacheConstantExpression,
  // this evaluates that. This is used to evaluate only runtime expressions.
  // This also carries along PState that is modified during expression
  // evaluation.
  def evalWithConversion[A](s: PState, e: CachedDynamic[A])(conv: (PState, Any) => A): (PState, A) = {
    e match {
      case Right(r) => (s, r)
      case Left(l) => {
        val (aAsAny, newVMap) = l.evaluate(s)
        val a: A = conv(s, aAsAny)
        (s.withVariables(newVMap), a)
      }
    }
  }

  def evalWithConversion[A](s: PState, oe: Maybe[CachedDynamic[A]])(conv: (PState, Any) => A): (PState, Maybe[A]) = {
    //    oe match {
    //      case Some(e) => {
    //        val (s1, a) = evalWithConversion[A](s, e)(conv)
    //        (s1, Some(a))
    //      }
    //      case None => (s, None)
    //    }
    if (oe.isDefined) {
      val (s1, a) = evalWithConversion[A](s, oe.get)(conv)
      (s1, One(a))
    } else (s, Nope)
  }

  def evalWithConversion[A](s: PState, oe: List[CachedDynamic[A]])(conv: (PState, Any) => A): (PState, List[A]) = {
    var state = s
    val listE = oe.map(e => {
      val (newState, exp) = evalWithConversion[A](state, e)(conv)
      state = newState
      exp
    })
    (state, listE)
  }

  // With an property that can potentially be compiled, this returns an Option,
  // which is either Some(s) if the value of the property is static, or None
  // otherwise
  //  def getStatic[A](e: CachedDynamic[A]): Option[A] = {
  //    e match {
  //      case Left(l) => None
  //      case Right(r) => Some(r)
  //    }
  //  }

  def getStatic[A](e: CachedDynamic[A]): Maybe[A] = {
    e match {
      case Left(l) => Nope
      case Right(r) => One(r)
    }
  }

  //  def getStatic[A](oe: Option[CachedDynamic[A]]): Option[A] = {
  //    oe match {
  //      case Some(e) => getStatic(e)
  //      case None => None
  //    }
  //  }

  def getStatic[A](oe: Maybe[CachedDynamic[A]]): Maybe[A] = {
    if (oe.isDefined) getStatic(oe.get)
    else Nope
  }
}