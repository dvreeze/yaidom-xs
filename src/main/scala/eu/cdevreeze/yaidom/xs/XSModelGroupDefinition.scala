package eu.cdevreeze.yaidom
package xs

import scala.collection.immutable

final class XSModelGroupDefinition(
  override val nameOption: Option[String],
  override val targetNamespaceOption: Option[String],
  val modelGroups: immutable.IndexedSeq[XSModelGroup],
  val annotations: immutable.IndexedSeq[XSAnnotation]) extends XSObject {

  require(nameOption ne null)
  require(targetNamespaceOption ne null)
  require(modelGroups ne null)
  require(annotations ne null)
}
