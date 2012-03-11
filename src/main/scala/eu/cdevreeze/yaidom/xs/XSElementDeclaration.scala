package eu.cdevreeze.yaidom
package xs

import scala.collection.immutable

final class XSElementDeclaration(
  override val nameOption: Option[String],
  override val targetNamespaceOption: Option[String],
  val typeDefinition: XSTypeDefinition,
  val scope: DeclarationScope,
  val constraintInfo: XSElementDeclaration.ConstraintInfo,
  val isNillable: Boolean,
  // TODO Identity constraints
  val substitutionInfo: XSElementDeclaration.SubstitutionInfo,
  val isAbstract: Boolean,
  val annotations: immutable.IndexedSeq[XSAnnotation]) extends XSTerm {

  require(nameOption ne null)
  require(targetNamespaceOption ne null)
  require(typeDefinition ne null)
  require(scope ne null)
  require(constraintInfo ne null)
  require(substitutionInfo ne null)
  require(annotations ne null)

  def toInstanceExpandedNameOption: Option[ExpandedName] =
    nameOption map { name => targetNamespaceOption map { tns => tns.ns.ename(name) } getOrElse (name.ename) }
}

object XSElementDeclaration {

  final class ConstraintInfo(
    val constraintType: ConstraintType,
    val constraintValueOption: Option[XSValue]) extends Immutable {

    require(constraintType ne null)
    require(constraintValueOption ne null)
  }

  final class SubstitutionInfo(
    val substitutionGroupAffiliationOption: Option[XSElementDeclaration],
    val disallowedSubstitutions: Set[DerivationMethod],
    val substitutionGroupExclusions: Set[DerivationMethod]) extends Immutable {

    require(substitutionGroupAffiliationOption ne null)
    require(disallowedSubstitutions ne null)
    require(substitutionGroupExclusions ne null)
  }
}
