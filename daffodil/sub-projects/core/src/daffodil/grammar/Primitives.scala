package daffodil.grammar

import daffodil.dsom._

case class UnicodeByteOrderMark(e: GlobalElementDecl, guard : Boolean = true) extends Terminal(e, guard)

case class Delimiter(e: SchemaComponent, guard : Boolean = true) extends Terminal(e, guard)
case class startGroup(ct: ComplexTypeBase, guard : Boolean = true) extends Terminal(ct, guard)
case class groupPosGreaterThan(n: Long)(sq: Sequence, guard : Boolean = true) extends Terminal(sq, guard)
case class endGroup(ct: ComplexTypeBase, guard : Boolean = true) extends Terminal(ct, guard)
case class startArray(e: LocalElementBase, guard : Boolean = true) extends Terminal(e, guard)
case class endArray(e: LocalElementBase, guard : Boolean = true) extends Terminal(e, guard)
case class noValue(e: GlobalElementDecl, guard : Boolean = true) extends Terminal(e, guard)
case class saveInputStream(e: GlobalElementDecl, guard : Boolean = true) extends Terminal(e, guard)
case class setEmptyInputStream(e: GlobalElementDecl, guard : Boolean = true) extends Terminal(e, guard)
case class restoreInputStream(e: GlobalElementDecl, guard : Boolean = true) extends Terminal(e, guard) 