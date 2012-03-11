package eu.cdevreeze.yaidom
package xs

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
