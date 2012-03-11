package eu.cdevreeze.yaidom
package xs

import scala.collection.immutable

final class XSAttributeDeclaration(
  override val nameOption: Option[String],
  override val targetNamespaceOption: Option[String],
  val typeDefinition: XSSimpleTypeDefinition,
  val scope: DeclarationScope,
  val constraintInfo: XSAttributeDeclaration.ConstraintInfo,
  val annotations: immutable.IndexedSeq[XSAnnotation]) extends XSObject {

  require(nameOption ne null)
  require(targetNamespaceOption ne null)
  require(typeDefinition ne null)
  require(scope ne null)
  require(constraintInfo ne null)
  require(annotations ne null)

  def toInstanceExpandedNameOption: Option[ExpandedName] =
    nameOption map { name => targetNamespaceOption map { tns => tns.ns.ename(name) } getOrElse (name.ename) }
}

object XSAttributeDeclaration {

  final class ConstraintInfo(
    val constraintType: ConstraintType,
    val constraintValueOption: Option[XSValue]) extends Immutable {

    require(constraintType ne null)
    require(constraintValueOption ne null)
  }
}
