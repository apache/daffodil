package daffodil.grammar

import daffodil.dsom._
import daffodil.exceptions.Assert
import daffodil.schema.annotation.props._
import daffodil.schema.annotation.props.gen._

//
// This file is 100% Stubs
//
// These get replaced by real implementations which implement the Terminal interface
//

abstract class Primitive(e: SchemaComponent, guard: Boolean = true) 
extends Terminal(e, guard) {
    def parser: Parser = DummyParser
  }

case class StandardTextIntPrim(e : ElementBaseMixin) extends Terminal(e, true) {
   def parser : Parser = DummyParser
}
case class StringFixedLengthPrim(e : ElementBaseMixin) extends Terminal(e, true) {
   def parser: Parser = DummyParser
}

case class ZonedTextIntPrim(e : ElementBaseMixin) extends Primitive(e, false)
case class RegularBinaryIntPrim(e : ElementBaseMixin) extends Primitive(e, false)
case class PackedIntPrim(e : ElementBaseMixin) extends Primitive(e, false)
case class BCDIntPrim(e : ElementBaseMixin) extends Primitive(e, false)

case class Delimiter(e: SchemaComponent, guard: Boolean = true) extends Primitive(e, guard)

case class StartGroup(ct: ComplexTypeBase, guard: Boolean = true) extends Primitive(ct, guard) 

case class GroupPosGreaterThan(n: Long)(sq: Sequence, guard: Boolean = true) extends Primitive(sq, guard)

case class EndGroup(ct: ComplexTypeBase, guard: Boolean = true) extends Primitive(ct, guard) 
  
case class StartArray(e: LocalElementBase, guard: Boolean = true) extends Primitive(e, guard) 
  
case class EndArray(e: LocalElementBase, guard: Boolean = true) extends Primitive(e, guard) 

case class NoValue(e: GlobalElementDecl, guard: Boolean = true) extends Primitive(e, guard) 
 
case class SaveInputStream(e: ElementBaseMixin, guard: Boolean = true) extends Primitive(e, guard) 
 
case class SetEmptyInputStream(e: ElementBaseMixin, guard: Boolean = true) extends Primitive(e, guard) 

case class RestoreInputStream(e: ElementBaseMixin, guard: Boolean = true) extends Primitive(e, guard) 

//case class Value(e: SchemaComponent, guard: Boolean = true) extends Primitive(e, guard) 

case class NotStopValue(e: LocalElementBase) extends Primitive(e, e.hasStopValue) 

case class StopValue(e: LocalElementBase) extends Primitive(e, e.hasStopValue) 

case class TheDefaultValue(e: ElementBaseMixin) extends Primitive(e, e.isDefaultable) 

case class LiteralNilValue(e: ElementBaseMixin) extends Primitive(e, e.isNillable) 

case class LogicalNilValue(e: ElementBaseMixin) extends Primitive(e, e.isNillable) 

case class LeadingSkipRegion(e: ElementBaseMixin) extends Primitive(e, e.leadingSkip > 0) 

case class AlignmentFill(e: ElementBaseMixin) extends Primitive(e, e.alignment != AlignmentType.Implicit)

case class TrailingSkipRegion(e: ElementBaseMixin) extends Primitive(e, e.trailingSkip > 0)

case class PrefixLength(e:ElementBaseMixin) extends Primitive(e, e.lengthKind == LengthKind.Prefixed)

case class UnicodeByteOrderMark(e: GlobalElementDecl) extends Primitive(e, false)

case class FinalUnusedRegion(e: LocalElementBase) extends Primitive(e, false) 