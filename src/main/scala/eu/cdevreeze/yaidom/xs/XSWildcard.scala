package eu.cdevreeze.yaidom
package xs

import scala.collection.immutable

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
