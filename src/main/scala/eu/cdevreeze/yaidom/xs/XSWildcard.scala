package eu.cdevreeze.yaidom
package xs

import scala.collection.immutable

/**
 * Wildcard "model group component". A special kind of particle which matches elements and attributes on namespace only, independent
 * of their local names.
 *
 * As a "model group component", it contributes to the portion of a complex type definition that controls the content of an element.
 */
final class XSWildcard(
  override val nameOption: Option[String],
  override val targetNamespaceOption: Option[String],
  val constraintType: NSConstraintType,
  val nsConstraintList: immutable.IndexedSeq[String],
  val processContents: ProcessContents,
  val annotations: immutable.IndexedSeq[XSAnnotation]) extends XSTerm {

  require(nameOption ne null)
  require(targetNamespaceOption ne null)
  require(constraintType ne null)
  require(nsConstraintList ne null)
  require(processContents ne null)
  require(annotations ne null)
}
