package eu.cdevreeze.yaidom
package xs

import scala.collection.immutable

final class XSComplexTypeDefinition(
  override val nameOption: Option[String],
  override val targetNamespaceOption: Option[String],
  override val baseTypeOption: Option[XSTypeDefinition],
  override val finalFor: Set[DerivationMethod],
  val derivationMethodOption: Option[DerivationMethod],
  val isAbstract: Boolean,
  val attributeUses: immutable.IndexedSeq[XSAttributeUse],
  val attributeWildcardOption: Option[XSWildcard],
  val simpleTypeOption: Option[XSSimpleTypeDefinition],
  val particleOption: Option[XSParticle],
  val prohibitedSubstitutions: Set[DerivationMethod],
  val annotations: immutable.IndexedSeq[XSAnnotation]) extends XSTypeDefinition {

  require(nameOption ne null)
  require(targetNamespaceOption ne null)
  require(baseTypeOption ne null)
  require(finalFor ne null)
  require(derivationMethodOption ne null)
  require(attributeUses ne null)
  require(attributeWildcardOption ne null)
  require(simpleTypeOption ne null)
  require(particleOption ne null)
  require(prohibitedSubstitutions ne null)
  require(annotations ne null)
}
