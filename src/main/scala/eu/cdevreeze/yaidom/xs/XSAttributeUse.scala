package eu.cdevreeze.yaidom
package xs

import scala.collection.immutable

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
