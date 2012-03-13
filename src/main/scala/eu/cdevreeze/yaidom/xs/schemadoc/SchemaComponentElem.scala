package eu.cdevreeze.yaidom
package xs
package schemadoc

import scala.collection.immutable

/**
 * A schema component element. It wraps an `Elem` in the corresponding schema document. It has no context, outside of
 * a `SchemaDocumentSet` (which needs 2 coordinates to find the element, namely a document and an `ElemPath` relative to the root element).
 */
sealed abstract class SchemaComponentElem(
  val qname: QName,
  val attributes: Map[QName, String],
  val scope: Scope) extends Immutable {

  require(qname ne null)
  require(attributes ne null)
  require(scope ne null)

  def wrappedElem: Elem

  protected[schemadoc] def wrappedElemWithoutChildren: Elem = Elem(qname, attributes, scope, immutable.IndexedSeq())
}

/** Trait mixed in by element declaration elements, model group elements and wildcard elements */
trait TermElem extends SchemaComponentElem

/** Schema document element */
final class SchemaDocumentElem(
  override val qname: QName,
  override val attributes: Map[QName, String],
  override val scope: Scope,
  val topLevelComponents: immutable.IndexedSeq[SchemaComponentElem]) extends SchemaComponentElem(qname, attributes, scope) {

  require(topLevelComponents ne null)
  require(wrappedElemWithoutChildren.resolvedName == NsXmlSchema.ename("schema"))

  require(topLevelComponents forall { comp =>
    comp.isInstanceOf[ElementDeclarationElem] ||
      comp.isInstanceOf[AttributeDeclarationElem] ||
      comp.isInstanceOf[TypeDefinitionElem] ||
      comp.isInstanceOf[ModelGroupDefinitionElem] ||
      comp.isInstanceOf[AttributeGroupDefinitionElem] ||
      comp.isInstanceOf[AnnotationElem]
  })

  def targetNamespace: String = wrappedElemWithoutChildren.attribute("targetNamespace".ename)

  override def wrappedElem: Elem = {
    val children = topLevelComponents map { comp => comp.wrappedElem }
    wrappedElemWithoutChildren.withChildren(children)
  }
}

/**
 * Element declaration element, which represents or refers to the primary schema component of an element declaration.
 * If wrapped by a particle element, both wrap the same yaidom `Elem`.
 */
final class ElementDeclarationElem(
  override val qname: QName,
  override val attributes: Map[QName, String],
  override val scope: Scope,
  val content: immutable.IndexedSeq[SchemaComponentElem]) extends SchemaComponentElem(qname, attributes, scope) with TermElem {

  require(content ne null)
  require(wrappedElemWithoutChildren.resolvedName == NsXmlSchema.ename("element"))
  require(wrappedElemWithoutChildren.attributeOption("name".ename).isDefined)

  require(content forall { comp =>
    comp.isInstanceOf[TypeDefinitionElem] ||
      comp.isInstanceOf[AnnotationElem]
    // TODO Or identity constraint element
  })

  def name: String = wrappedElemWithoutChildren.attribute("name".ename)

  override def wrappedElem: Elem = {
    val children = content map { comp => comp.wrappedElem }
    wrappedElemWithoutChildren.withChildren(children)
  }
}

/**
 * Attribute declaration element, which represents or refers to the primary schema component of an attribute declaration.
 * If wrapped by an attribute use element, both wrap the same yaidom `Elem`.
 */
final class AttributeDeclarationElem(
  override val qname: QName,
  override val attributes: Map[QName, String],
  override val scope: Scope,
  val content: immutable.IndexedSeq[SchemaComponentElem]) extends SchemaComponentElem(qname, attributes, scope) {

  require(content ne null)
  require(wrappedElemWithoutChildren.resolvedName == NsXmlSchema.ename("attribute"))
  require(wrappedElemWithoutChildren.attributeOption("name".ename).isDefined)

  def name: String = wrappedElemWithoutChildren.attribute("name".ename)

  override def wrappedElem: Elem = {
    val children = content map { comp => comp.wrappedElem }
    wrappedElemWithoutChildren.withChildren(children)
  }
}

/** Type definition element, which represents the primary schema component of a (complex or simple) type definition. */
abstract class TypeDefinitionElem(
  override val qname: QName,
  override val attributes: Map[QName, String],
  override val scope: Scope,
  val content: immutable.IndexedSeq[SchemaComponentElem]) extends SchemaComponentElem(qname, attributes, scope) {

  require(content ne null)
}

/** Complex type definition element */
final class ComplexTypeDefinitionElem(
  override val qname: QName,
  override val attributes: Map[QName, String],
  override val scope: Scope,
  override val content: immutable.IndexedSeq[SchemaComponentElem]) extends TypeDefinitionElem(qname, attributes, scope, content) {

  require(wrappedElemWithoutChildren.resolvedName == NsXmlSchema.ename("complexType"))

  override def wrappedElem: Elem = {
    val children = content map { comp => comp.wrappedElem }
    wrappedElemWithoutChildren.withChildren(children)
  }
}

/** Simple type definition element */
final class SimpleTypeDefinitionElem(
  override val qname: QName,
  override val attributes: Map[QName, String],
  override val scope: Scope,
  override val content: immutable.IndexedSeq[SchemaComponentElem]) extends TypeDefinitionElem(qname, attributes, scope, content) {

  require(wrappedElemWithoutChildren.resolvedName == NsXmlSchema.ename("simpleType"))

  override def wrappedElem: Elem = {
    val children = content map { comp => comp.wrappedElem }
    wrappedElemWithoutChildren.withChildren(children)
  }
}

/** Attribute group definition element, which represents the secondary schema component of an attribute group definition */
final class AttributeGroupDefinitionElem(
  override val qname: QName,
  override val attributes: Map[QName, String],
  override val scope: Scope,
  val content: immutable.IndexedSeq[SchemaComponentElem]) extends SchemaComponentElem(qname, attributes, scope) {

  require(content ne null)
  require(wrappedElemWithoutChildren.resolvedName == NsXmlSchema.ename("attributeGroup"))
  require(wrappedElemWithoutChildren.attributeOption("name".ename).isDefined)

  require(content forall { comp =>
    comp.isInstanceOf[AttributeDeclarationElem] ||
      comp.isInstanceOf[AttributeGroupDefinitionElem] ||
      comp.isInstanceOf[WildcardElem] ||
      comp.isInstanceOf[AnnotationElem]
  })

  def name: String = wrappedElemWithoutChildren.attribute("name".ename)

  override def wrappedElem: Elem = {
    val children = content map { comp => comp.wrappedElem }
    wrappedElemWithoutChildren.withChildren(children)
  }
}

/** Model group definition element, which represents the secondary schema component of a model group definition */
final class ModelGroupDefinitionElem(
  override val qname: QName,
  override val attributes: Map[QName, String],
  override val scope: Scope,
  val content: immutable.IndexedSeq[SchemaComponentElem]) extends SchemaComponentElem(qname, attributes, scope) {

  require(content ne null)
  require(wrappedElemWithoutChildren.resolvedName == NsXmlSchema.ename("group"))
  require(wrappedElemWithoutChildren.attributeOption("name".ename).isDefined)

  require(content forall { comp =>
    comp.isInstanceOf[ModelGroupElem] ||
      comp.isInstanceOf[AnnotationElem]
  })

  def name: String = wrappedElemWithoutChildren.attribute("name".ename)

  override def wrappedElem: Elem = {
    val children = content map { comp => comp.wrappedElem }
    wrappedElemWithoutChildren.withChildren(children)
  }
}

// TOOD The secondary schema component elements for identity constraint definitions (key, keyref and unique) and notation declaration elements (notation).

/**
 * Particle element, which represents the "helper" schema component of a particle.
 * It wraps the same yaidom `Elem` as the underlying term element.
 */
final class ParticleElem(
  val term: TermElem) extends SchemaComponentElem(
  term.wrappedElemWithoutChildren.qname,
  term.wrappedElemWithoutChildren.attributes,
  term.wrappedElemWithoutChildren.scope) {

  require(term ne null)

  override def wrappedElem: Elem = term.wrappedElem
}

/**
 * Attribute use element, which represents the "helper" schema component of an attribute use.
 * It wraps the same yaidom `Elem` as the underlying attribute declaration element.
 */
final class AttributeUseElem(
  val attributeDeclaration: AttributeDeclarationElem) extends SchemaComponentElem(
  attributeDeclaration.wrappedElemWithoutChildren.qname,
  attributeDeclaration.wrappedElemWithoutChildren.attributes,
  attributeDeclaration.wrappedElemWithoutChildren.scope) {

  require(attributeDeclaration ne null)

  def name: String = attributeDeclaration.name

  override def wrappedElem: Elem = attributeDeclaration.wrappedElem
}

/** Model group element, which represents the "helper" schema component of a model group */
final class ModelGroupElem(
  override val qname: QName,
  override val attributes: Map[QName, String],
  override val scope: Scope,
  val content: immutable.IndexedSeq[SchemaComponentElem]) extends SchemaComponentElem(qname, attributes, scope) with TermElem {

  require(content ne null)
  require(Set(NsXmlSchema.ename("all"), NsXmlSchema.ename("choice"), NsXmlSchema.ename("sequence")).contains(wrappedElemWithoutChildren.resolvedName))

  require(content forall { comp =>
    comp.isInstanceOf[ParticleElem] ||
      comp.isInstanceOf[AnnotationElem]
  })

  override def wrappedElem: Elem = {
    val children = content map { comp => comp.wrappedElem }
    wrappedElemWithoutChildren.withChildren(children)
  }
}

/** Wildcard element, which represents the "helper" schema component of a wildcard */
final class WildcardElem(
  override val qname: QName,
  override val attributes: Map[QName, String],
  override val scope: Scope,
  val content: immutable.IndexedSeq[SchemaComponentElem]) extends SchemaComponentElem(qname, attributes, scope) with TermElem {

  require(content ne null)
  require(Set(NsXmlSchema.ename("any"), NsXmlSchema.ename("anyAttribute")).contains(wrappedElemWithoutChildren.resolvedName))

  require(content forall { comp =>
    comp.isInstanceOf[AnnotationElem]
  })

  override def wrappedElem: Elem = {
    val children = content map { comp => comp.wrappedElem }
    wrappedElemWithoutChildren.withChildren(children)
  }
}

/** Annotation element, which represents the "helper" schema component of annotation */
final class AnnotationElem(
  override val qname: QName,
  override val attributes: Map[QName, String],
  override val scope: Scope,
  val annotationContent: Elem) extends SchemaComponentElem(qname, attributes, scope) {

  // TODO appinfo versus documentation

  require(annotationContent ne null)
  require(wrappedElemWithoutChildren.resolvedName == NsXmlSchema.ename("annotation"))

  override def wrappedElem: Elem = {
    wrappedElemWithoutChildren.withChildren(immutable.IndexedSeq(annotationContent))
  }
}
