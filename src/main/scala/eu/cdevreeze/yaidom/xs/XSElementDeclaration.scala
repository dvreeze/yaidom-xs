package eu.cdevreeze.yaidom
package xs

import scala.collection.immutable

/**
 * Element declaration. Its scope is either global, or it is scoped to a containing complex type definition (which is implicit here).
 *
 * Element declarations are "primary" XML Schema components, along with attribute declarations and (simple and complex) type definitions.
 */
final class XSElementDeclaration(
  val name: String,
  override val targetNamespaceOption: Option[String],
  val typeDefinition: XSTypeDefinition,
  val scope: DeclarationScope,
  val constraintInfo: XSElementDeclaration.ConstraintInfo,
  val isNillable: Boolean,
  // TODO Identity constraints
  val substitutionInfo: XSElementDeclaration.SubstitutionInfo,
  val isAbstract: Boolean,
  val annotations: immutable.IndexedSeq[XSAnnotation]) extends XSTerm {

  require(name ne null)
  require(targetNamespaceOption ne null)
  require(typeDefinition ne null)
  require(scope ne null)
  require(constraintInfo ne null)
  require(substitutionInfo ne null)
  require(annotations ne null)

  override def nameOption: Option[String] = Some(name)

  def toInstanceExpandedNameOption: Option[ExpandedName] =
    targetNamespaceOption map { tns => tns.ns.ename(name) } orElse (Some(name.ename))
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
