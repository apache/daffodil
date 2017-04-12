package edu.illinois.ncsa.daffodil.dsom

import edu.illinois.ncsa.daffodil.exceptions.Assert

trait NestingMixin {

  /** The lexically enclosing schema component */
  def parent: SchemaComponent

  /**
   * Define this for schema components that have back-references to ref 
   * objects.
   * So group def to group ref, globalelementdecl to element ref,
   * type to element, base type to derived type.
   * 
   * Not for format annotations however. We don't backpoint those to 
   * other format annotations that ref them.
   */
  protected def enclosingComponentDef: Option[SchemaComponent]

  /**
   * The enclosing component, and follows back-references
   *  from types to their elements, from globalElementDef to
   *  elementRefs, from simpleType defs to derived simpletype defs,
   *  from global group defs to group refs
   *
   *  Note: the enclosing component of a global element or global group
   *  referenced from a element ref or group ref, is NOT the ref object, but the
   *  component that contains the ref object
   */
  final lazy val enclosingComponent: Option[SchemaComponent] = {
    val optECD = enclosingComponentDef
    optECD match {
      case Some(eref: ElementRef) => eref.enclosingComponent
      case Some(gref: GroupRef) => gref.enclosingComponent
      case _ => optECD
    }
  }
}

/**
 * Mixin for all schema factories and schema components with no
 * backpointers, just a lexical parent. This means all the non-global
 * schema components.
 */
trait NestingLexicalMixin
  extends NestingMixin {

  override protected def enclosingComponentDef =
    if (parent eq null) None else Some(parent)

}

/**
 * Mixin for all global schema components
 */
trait NestingTraversesToReferenceMixin
  extends NestingMixin {

  def referringComponent: Option[SchemaComponent]

  final override protected def enclosingComponentDef: Option[SchemaComponent] = {
    Assert.invariant(parent.isInstanceOf[SchemaDocument]) // global things have schema documents as their parents.
    referringComponent
  }

}
