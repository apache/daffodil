package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.dpath.NodeInfo.PrimType

trait SimpleTypeBase
  extends TypeBase
  with TypeChecks {
  // def context: SchemaComponent
  def primitiveType: PrimType
}