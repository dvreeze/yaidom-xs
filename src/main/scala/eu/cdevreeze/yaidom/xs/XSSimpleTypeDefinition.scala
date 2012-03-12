package eu.cdevreeze.yaidom
package xs

import scala.collection.immutable
import Variety._

/**
 * Simple type definition. Simple types are types of attribute values and of text-only content of elements.
 */
final class XSSimpleTypeDefinition(
  override val nameOption: Option[String],
  override val targetNamespaceOption: Option[String],
  override val baseTypeOption: Option[XSTypeDefinition],
  override val finalFor: Set[DerivationMethod],
  val varietyOption: Option[Variety],
  val primitiveTypeOption: Option[XSSimpleTypeDefinition],
  val builtInKind: TypeCategory,
  val itemTypeOption: Option[XSSimpleTypeDefinition],
  val memberTypes: immutable.IndexedSeq[XSSimpleTypeDefinition],
  val definedFacets: Set[Facet],
  val fixedFacets: Set[Facet],
  val lexicalEnumeration: immutable.IndexedSeq[String],
  val lexicalPattern: immutable.IndexedSeq[String],
  val ordered: Ordered,
  val isFinite: Boolean,
  val isBounded: Boolean,
  val isNumeric: Boolean,
  val facets: immutable.IndexedSeq[Facet],
  val multiValueFacets: immutable.IndexedSeq[Facet],
  val annotations: immutable.IndexedSeq[XSAnnotation]) extends XSTypeDefinition {

  require(nameOption ne null)
  require(targetNamespaceOption ne null)
  require(baseTypeOption ne null)
  require(finalFor ne null)
  require(varietyOption ne null)
  require(primitiveTypeOption ne null)
  require(builtInKind ne null)
  require(itemTypeOption ne null)
  require(memberTypes ne null)
  require(definedFacets ne null)
  require(fixedFacets ne null)
  require(lexicalEnumeration ne null)
  require(lexicalPattern ne null)
  require(ordered ne null)
  require(facets ne null)
  require(multiValueFacets ne null)
  require(annotations ne null)

  require(itemTypeOption.isEmpty || varietyOption == Some(VarietyList))
  require(memberTypes.isEmpty || varietyOption == Some(VarietyUnion))
  require(primitiveTypeOption.isEmpty || varietyOption == Some(VarietyAtomic))
}
