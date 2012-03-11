package eu.cdevreeze.yaidom
package xs

import scala.collection.immutable

/** Schema, as container of declarations belonging to the same target namespace. Inspired by XSOM. */
final class XSSchema(
  val targetNamespaceOption: Option[String],
  val elementDeclarations: immutable.IndexedSeq[XSElementDeclaration],
  val attributeDeclarations: immutable.IndexedSeq[XSAttributeDeclaration],
  val attributeGroupDefinitions: immutable.IndexedSeq[XSAttributeGroupDefinition],
  val modelGroupDefinitions: immutable.IndexedSeq[XSModelGroupDefinition],
  val typeDefinitions: immutable.IndexedSeq[XSTypeDefinition],
  val annotations: immutable.IndexedSeq[XSAnnotation]) extends Immutable {

  require(targetNamespaceOption ne null)
  require(elementDeclarations ne null)
  require(attributeDeclarations ne null)
  require(attributeGroupDefinitions ne null)
  require(modelGroupDefinitions ne null)
  require(typeDefinitions ne null)
  require(annotations ne null)
}
