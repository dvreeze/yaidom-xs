package eu.cdevreeze.yaidom
package xs

/**
 * Particle "model group component". So either an element declaration, model group or wildcard, along with occurrence constraints.
 *
 * As a "model group component", it contributes to the portion of a complex type definition that controls the content of an element.
 */
final class XSParticle(
  override val nameOption: Option[String],
  override val targetNamespaceOption: Option[String],
  val minOccursOption: Option[Int],
  val maxOccursOption: Option[Int],
  val maxOccursUnbounded: Boolean,
  val term: XSTerm) extends XSObject {

  require(nameOption ne null)
  require(targetNamespaceOption ne null)
  require(minOccursOption ne null)
  require(maxOccursOption ne null)
  require(term ne null)
}
