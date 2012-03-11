package eu.cdevreeze.yaidom
package xs

import scala.collection.immutable

final class XSModelGroup(
  override val nameOption: Option[String],
  override val targetNamespaceOption: Option[String],
  val compositor: Compositor,
  val particles: immutable.IndexedSeq[XSParticle],
  val annotations: immutable.IndexedSeq[XSAnnotation]) extends XSTerm {

  require(nameOption ne null)
  require(targetNamespaceOption ne null)
  require(compositor ne null)
  require(particles ne null)
  require(annotations ne null)
}
