package eu.cdevreeze.yaidom
package xs

import scala.collection.immutable

/**
 * A name for a set of attribute declarations, enabling re-use of the attribute declarations in several complex type definitions.
 */
final class XSAttributeGroupDefinition(
  val name: String,
  override val targetNamespaceOption: Option[String],
  val attributeUses: immutable.IndexedSeq[XSAttributeUse],
  val attributeWildcardOption: Option[XSWildcard],
  val annotationOption: Option[XSAnnotation]) extends XSObject {

  require(name ne null)
  require(targetNamespaceOption ne null)
  require(attributeUses ne null)
  require(attributeWildcardOption ne null)
  require(annotationOption ne null)

  override def nameOption: Option[String] = Some(name)
}
