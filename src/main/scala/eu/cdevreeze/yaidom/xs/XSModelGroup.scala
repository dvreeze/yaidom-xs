package eu.cdevreeze.yaidom
package xs

import scala.collection.immutable

/**
 * "Model group component" applying to a list of elements. Either a sequence, choice (disjunction) or conjunction of particles.
 *
 * As a "model group component", it contributes to the portion of a complex type definition that controls the content of an element.
 */
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
