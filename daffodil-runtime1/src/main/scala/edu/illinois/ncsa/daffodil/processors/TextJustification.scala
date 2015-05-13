package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.util._

object TextJustificationType extends Enum {
  sealed abstract trait Type extends EnumValueType
  case object None extends Type
  case object Left extends Type
  case object Right extends Type
  case object Center extends Type
}