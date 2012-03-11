package eu.cdevreeze.yaidom
package xs

import scala.collection.immutable

final class XSAttributeGroupDefinition(
  override val nameOption: Option[String],
  override val targetNamespaceOption: Option[String],
  val attributeUses: immutable.IndexedSeq[XSAttributeUse],
  val attributeWildcardOption: Option[XSWildcard],
  val annotationOption: Option[XSAnnotation]) extends XSObject {

  require(nameOption ne null)
  require(targetNamespaceOption ne null)
  require(attributeUses ne null)
  require(attributeWildcardOption ne null)
  require(annotationOption ne null)
}
