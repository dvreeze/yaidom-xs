package eu.cdevreeze.yaidom
package xs

import scala.collection.immutable

/**
 * "Model group component" applying to an attribute declaration. It is similar to a particle in role, but for an attribute declaration.
 * It specifies whether the contained attribute declaration requires or merely allows its attribute, and whether it has a default or fixed value.
 *
 * As a "model group component", it contributes to the portion of a complex type definition that controls the content of an element.
 */
final class XSAttributeUse(
  override val nameOption: Option[String],
  override val targetNamespaceOption: Option[String],
  val isRequired: Boolean,
  val attributeDeclaration: XSAttributeDeclaration,
  val constraintInfo: XSAttributeUse.ConstraintInfo,
  val annotations: immutable.IndexedSeq[XSAnnotation]) extends XSObject {

  require(nameOption ne null)
  require(targetNamespaceOption ne null)
  require(attributeDeclaration ne null)
  require(constraintInfo ne null)
  require(annotations ne null)
}

object XSAttributeUse {

  final class ConstraintInfo(
    val constraintType: ConstraintType,
    val constraintValueOption: Option[XSValue]) extends Immutable {

    require(constraintType ne null)
    require(constraintValueOption ne null)
  }
}
