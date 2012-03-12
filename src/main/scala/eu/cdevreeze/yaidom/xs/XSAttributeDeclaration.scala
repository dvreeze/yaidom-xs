package eu.cdevreeze.yaidom
package xs

import scala.collection.immutable

/**
 * Attribute declaration. Its scope is either global, or it is scoped to a containing complex type definition (which is implicit here).
 */
final class XSAttributeDeclaration(
  val name: String,
  override val targetNamespaceOption: Option[String],
  val typeDefinition: XSSimpleTypeDefinition,
  val scope: DeclarationScope,
  val constraintInfo: XSAttributeDeclaration.ConstraintInfo,
  val annotations: immutable.IndexedSeq[XSAnnotation]) extends XSObject {

  require(name ne null)
  require(targetNamespaceOption ne null)
  require(typeDefinition ne null)
  require(scope ne null)
  require(constraintInfo ne null)
  require(annotations ne null)

  override def nameOption: Option[String] = Some(name)

  def toInstanceExpandedNameOption: Option[ExpandedName] =
    targetNamespaceOption map { tns => tns.ns.ename(name) } orElse (Some(name.ename))
}

object XSAttributeDeclaration {

  final class ConstraintInfo(
    val constraintType: ConstraintType,
    val constraintValueOption: Option[XSValue]) extends Immutable {

    require(constraintType ne null)
    require(constraintValueOption ne null)
  }
}
