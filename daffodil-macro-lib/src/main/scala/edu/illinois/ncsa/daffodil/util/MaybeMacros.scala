package edu.illinois.ncsa.daffodil.util

// import scala.reflect.macros.blackbox.Context

//object MaybeMacros {
//
//  def maybeMapMacro(c: Context)(fn: c.Tree) = {
//    import c.universe._
//    q"""
//    {
//      val f = $fn
//      if (this.isDefined) One(f.apply(this.value)) else Nope
//    }
//    """
//  }
//}
