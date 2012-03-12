package eu.cdevreeze.yaidom
package xs

import scala.collection.immutable

/**
 * A name for a model group, enabling re-use of the model group in several complex type definitions.
 */
final class XSModelGroupDefinition(
  val name: String,
  override val targetNamespaceOption: Option[String],
  val modelGroups: immutable.IndexedSeq[XSModelGroup],
  val annotations: immutable.IndexedSeq[XSAnnotation]) extends XSObject {

  require(name ne null)
  require(targetNamespaceOption ne null)
  require(modelGroups ne null)
  require(annotations ne null)

  override def nameOption: Option[String] = Some(name)
}
