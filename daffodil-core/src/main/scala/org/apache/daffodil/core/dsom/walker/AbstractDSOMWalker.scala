/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.core.dsom.walker

import org.apache.daffodil.runtime1.dsom.walker._

/**
 * Below is a series of "View" Mixins defined such that particular simple attributes
 * of the core DSOM classes are exposed as a Walk is performed.
 *
 * Several of them have a walkDSOM method.  By default, this method is called as the walk is performed,
 * but a user implementing AbstractDSOMWalker can override this behavior by overriding walkFromRoot
 * and implementing their own event handlers for each of the DSOM types.
 */

trait CommonContextView {
  def namespaces: scala.xml.NamespaceBinding
}

trait TermView extends CommonContextView {
  def isArray: Boolean
  def isOptional: Boolean
}

trait TypeView
trait SimpleTypeView extends TypeView {
  def primType: PrimTypeView
}
trait ComplexTypeView extends TypeView {
  def group: ModelGroupView
}

trait ModelGroupView extends TermView {
  def groupMembers: Seq[TermView]
}

trait ChoiceView extends ModelGroupView
trait SequenceView extends ModelGroupView
trait GroupRefView extends ModelGroupView {
  def isHidden: Boolean
}

trait ElementDeclView extends CommonContextView {
  def isSimpleType: Boolean
  def isComplexType: Boolean
  def simpleType: SimpleTypeView
  def complexType: ComplexTypeView
}

trait ElementBaseView extends ElementDeclView with TermView {
  def name: String
}

trait RootView extends ElementBaseView

/**
 * A class designed to walk the internal representation of a DFDL Schema File.
 *
 * There are 2 main event handlers an implementing class has to worry about: one for Terms, and
 * another for Types.  These are called as the DSOM is walked.
 *
 * Though recursion is used here to define the walk, it is not advised to use recursion between
 * these event handlers.  Instead, consider a stack-like structure, as the DFDL Schema structure
 * as well as the recursive method call structure can be represented by trees.
 */
abstract class AbstractDSOMWalker {

  /**
   * Method to be called on the beginning of the traversal.  It is recommended to add some
   * sort of wrapper element to a stack if you're doing a typical stack-based traversal.
   *
   * @param root the root element of the DFDL Schema
   */
  protected def onWalkBegin(root: RootView): Unit

  /**
   * Method to be called when the traversal concludes.  It is recommended to put any post-processing
   * and anything to tidy up the stack or the result here.
   *
   * @param root the root element of the DFDL Schema
   */
  protected def onWalkEnd(root: RootView): Unit

  /**
   * Method to be called whenever any element that is a Term is encountered.
   * This applies to Sequence, Choice, GroupRef, ElementBase, etc.
   *
   * It is highly recommended that, when implementing this method, you pattern match
   * some of these different sub-types at some point to handle each accordingly.
   *
   * @param termElement the term element
   */
  def onTermBegin(termElement: TermView): Unit

  /**
   * Method to be called when a Term element has finished processing
   *
   * @param termElement the term element
   */
  def onTermEnd(termElement: TermView): Unit

  /**
   * Method to be called when either a Simple or Complex Type element has been encountered.
   * This is just the element for <simpleType> or <complexType>, so no implementation is necessary if you
   * do not care about these wrapper portion; its children will be walked automatically.
   *
   * @param typeElement either a Simple or Complex type
   */
  def onTypeBegin(typeElement: TypeView): Unit

  /**
   * Method to be called when either a Simple or Complex Type element has been finished
   * processing.  See onTypeBegin method description
   *
   * @param typeElement either a Simple or Complex type
   */
  def onTypeEnd(typeElement: TypeView): Unit

  /**
   * Starts a DSOM walk from the Root object.  By default, the walk is completed recursively in the helper
   * method below, but a custom walk can be defined by overriding
   * this method instead and calling local event handlers.
   * @param schemaSetRoot The root element of the DFDL Schema.  This will be the starting point of the traversal
   */
  def walkFromRoot(schemaSetRoot: RootView): Unit = {
    onWalkBegin(schemaSetRoot)
    walkerHelper(schemaSetRoot)
    onWalkEnd(schemaSetRoot)
  }

  /**
   * Local helper method used to recursively walk the DSOM.
   * If a non-recursive walk or a more specific recursive walk is desired, one
   * can override this method, or simply not use it and instead override walkFromRoot
   * and have that overridden method reference custom helper methods.
   * @param termView the current Term element to be pattern matched
   */
  protected def walkerHelper(termView: TermView): Unit = {
    onTermBegin(termView)
    termView match {
      case element: ElementBaseView =>
        if (element.isComplexType) {
          onTypeBegin(element.complexType)
          walkerHelper(element.complexType.group)
          onTypeEnd(element.complexType)
        } else {
          onTypeBegin(element.simpleType)
          onTypeEnd(element.simpleType)
        }
      case groupRef: GroupRefView =>
        if (!groupRef.isHidden) {
          groupRef.groupMembers.foreach(walkerHelper)
        }
      case modelGroup: ModelGroupView =>
        modelGroup.groupMembers.foreach(walkerHelper)
    }
    onTermEnd(termView)
  }

}
