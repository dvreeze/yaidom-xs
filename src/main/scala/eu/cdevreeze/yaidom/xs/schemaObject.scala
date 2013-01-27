/*
 * Copyright 2011 Chris de Vreeze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eu.cdevreeze.yaidom
package xs

import java.net.URI
import scala.collection.immutable
import eu.cdevreeze.yaidom._
import SchemaObject._

/**
 * Immutable XML Schema or a part thereof.
 *
 * Terminology is taken as much as possible from the XML Schema specification Part 1 (especially section 2.2).
 *
 * Still, the API has been inspired both by the Apache Common XML Schema API 2.0 and XSOM.
 * See http://ws.apache.org/commons/xmlschema20/xmlschema-core/apidocs/overview-summary.html and
 * http://xsom.java.net/nonav/javadoc/index.html, respectively.
 *
 * This represents only schema file content, without resolving imports and includes, and without
 * resolving types, substitution groups, etc. The latter requires an appropriate collection of schema documents
 * (closed under imports and includes).
 *
 * Note that `allChildElems`, being a val variable, is very fast, as it should be (see the `ElemLike` query API).
 *
 * TODO Use yaidom trait HasParent.
 *
 * @author Chris de Vreeze
 */
sealed abstract class SchemaObject private[xs] (
  val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends ElemLike[SchemaObject] with SchemaObject.HasParent[SchemaObject] with HasText with Immutable {

  require(wrappedElem ne null)
  require(allChildElems ne null)

  require(wrappedElem.allChildElems == allChildElems.map(_.wrappedElem), "Inconsistent SchemaObject")

  require(wrappedElem.rootElem.resolvedName == EName(ns, "schema"), "The root of the element tree must be a 'schema' element")
  require(
    (wrappedElem.resolvedName == EName(ns, "schema")) || (!wrappedElem.elemPath.isRoot),
    "This element must either be a 'schema' element, or not be the root of the element tree")

  /**
   * Expensive auxiliary constructor.
   */
  def this(wrappedElem: indexed.Elem) =
    this(wrappedElem, wrappedElem.allChildElems.map(e => SchemaObject(e)))

  final override def resolvedName: EName = wrappedElem.resolvedName

  final override def resolvedAttributes: immutable.IndexedSeq[(EName, String)] = wrappedElem.resolvedAttributes

  final override def equals(obj: Any): Boolean = obj match {
    case other: SchemaObject => (other.wrappedElem == this.wrappedElem)
    case _ => false
  }

  final override def hashCode: Int = wrappedElem.hashCode

  final override def text: String = wrappedElem.text

  final override def parentOption: Option[SchemaObject] =
    wrappedElem.parentOption map { e => SchemaObject(e) }

  final override def toString: String = wrappedElem.elem.toString
}

/**
 * XML Schema (from one document). That is, the "schema" XML element.
 *
 * This is what the XML Schema specification calls a schema document, or the document element thereof.
 * In the abstract schema model of the specification, a schema is represented by one or more of these "schema documents".
 */
final class Schema private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(wrappedElem, allChildElems) {
  SchemaObjects.checkSchemaElem(wrappedElem)

  /**
   * Expensive auxiliary constructor.
   */
  def this(wrappedElem: indexed.Elem) =
    this(wrappedElem, wrappedElem.allChildElems.map(e => SchemaObject(e)))

  final def targetNamespaceOption: Option[String] = wrappedElem \@ EName("targetNamespace")

  /**
   * Returns all element declarations, whether top-level or local.
   */
  final def elementDeclarations: immutable.IndexedSeq[ElementDeclaration] =
    this filterElemsOrSelf { e => e.resolvedName == EName(ns, "element") } collect { case e: ElementDeclaration => e }

  /**
   * Returns all top-level element declarations.
   */
  final def topLevelElementDeclarations: immutable.IndexedSeq[ElementDeclaration] =
    elementDeclarations filter { e => e.isTopLevel }

  /**
   * Returns all attribute declarations, whether top-level or local.
   */
  final def attributeDeclarations: immutable.IndexedSeq[AttributeDeclaration] =
    this filterElemsOrSelf { e => e.resolvedName == EName(ns, "attribute") } collect { case e: AttributeDeclaration => e }

  /**
   * Returns all top-level attribute declarations.
   */
  final def topLevelAttributeDeclarations: immutable.IndexedSeq[AttributeDeclaration] =
    attributeDeclarations filter { e => e.isTopLevel }
}

// Schema Components

/**
 * Schema component, as specified in the XML Schema specification, part 1, section 2.2.
 *
 * There is an important difference, however. For example, in the abstract schema model, a particle contains a term, such
 * as an element declaration. In the concrete XML representation of the schema, the particle and the term that it contains,
 * such as an element declaration, are part of the same XML element. To keep close to the XML representation of XML Schema,
 * a term like an element declaration IS a particle, so a particle does NOT HAVE a term. More generally, it is the schema
 * XML document that is leading, and not the abstract schema model that has no knowledge about its XML representation.
 */
abstract class SchemaComponent private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(wrappedElem, allChildElems)

/**
 * Particle, having a min and max occurs (possibly default).
 *
 * As documented for `SchemaComponent`, an element declaration (for example) is regarded a particle, because the element
 * declaration and the particle that it is a part of in the abstract schema model can not be separated in the XML representation.
 *
 * Moreover, top-level element declarations are not part of any particle in the abstract schema model. Hence, from a
 * "modelling perspective", regarding an element declaration (among other terms) to be a particle is not correct, yet from
 * an XML representation point of view it makes sense.
 */
abstract class Particle private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(wrappedElem, allChildElems) {
  SchemaObjects.checkParticleElem(wrappedElem)

  final def minOccurs: Int = minOccursAttrOption map (_.toInt) getOrElse 1

  final def maxOccurs: Int = {
    maxOccursAttrOption map { v =>
      if (v.toLowerCase(java.util.Locale.ENGLISH) == "unbounded") Particle.Unbounded else 1
    } getOrElse 1
  }

  final def minOccursAttrOption: Option[String] = SchemaObjects.minOccursAttrOption(wrappedElem)

  final def maxOccursAttrOption: Option[String] = SchemaObjects.maxOccursAttrOption(wrappedElem)
}

/**
 * Element declaration. That is, the "element" XML element.
 */
final class ElementDeclaration private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends Particle(wrappedElem, allChildElems) {
  SchemaObjects.checkElementDeclarationElem(wrappedElem)

  /**
   * Expensive auxiliary constructor.
   */
  def this(wrappedElem: indexed.Elem) =
    this(wrappedElem, wrappedElem.allChildElems.map(e => SchemaObject(e)))

  /**
   * Returns true if and only if the element declaration has the schema element as its parent.
   */
  final def isTopLevel: Boolean = wrappedElem.elemPath.entries.size == 1

  /**
   * Returns true if and only if the element declaration is a reference to another (global) element declaration.
   * Top level element declarations are never references.
   */
  final def isReference: Boolean = refOption.isDefined

  /**
   * Returns true if and only if the element declaration is abstract.
   * Only top level element declarations can be references.
   */
  final def isAbstract: Boolean = abstractOption == Some(true)

  /**
   * Returns the `EName` by combining the (root) target namespace and the value of the "name" attribute,
   * if any, wrapped in an Option.
   */
  final def enameOption: Option[EName] = SchemaObjects.enameOption(wrappedElem)

  /**
   * Returns the value of the 'id' attribute, if any, wrapped in an Option.
   */
  final def idOption: Option[String] = SchemaObjects.idOption(wrappedElem)

  /**
   * Returns the value of the 'type' attribute as expanded name, if any, wrapped in an Option.
   */
  final def typeAttributeOption: Option[EName] = SchemaObjects.typeAttributeOption(wrappedElem)

  /**
   * Returns the value of the 'substitutionGroup' attribute as expanded name, if any, wrapped in an Option.
   */
  final def substitutionGroupOption: Option[EName] = SchemaObjects.substitutionGroupOption(wrappedElem)

  /**
   * Returns the value of the 'abstract' attribute, if any, wrapped in an Option.
   */
  final def abstractOption: Option[Boolean] = SchemaObjects.abstractOption(wrappedElem)

  /**
   * Returns the value of the 'nillable' attribute, if any, wrapped in an Option.
   */
  final def nillableOption: Option[Boolean] = SchemaObjects.nillableOption(wrappedElem)

  /**
   * Returns the value of the 'ref' attribute as expanded name, if any, wrapped in an Option.
   */
  final def refOption: Option[EName] = SchemaObjects.refOption(wrappedElem)
}

/**
 * Attribute use. The correspondence between attribute use and attribute declarations is analogous to the one between
 * particles and element declarations.
 */
abstract class AttributeUse private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(wrappedElem, allChildElems) {
  SchemaObjects.checkAttributeUseElem(wrappedElem)

  // TODO
}

/**
 * Attribute declaration. That is, the "attribute" XML element.
 */
final class AttributeDeclaration private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends AttributeUse(wrappedElem, allChildElems) {
  SchemaObjects.checkAttributeDeclarationElem(wrappedElem)

  /**
   * Expensive auxiliary constructor.
   */
  def this(wrappedElem: indexed.Elem) =
    this(wrappedElem, wrappedElem.allChildElems.map(e => SchemaObject(e)))

  /**
   * Returns true if and only if the attribute declaration has the schema element as its parent.
   */
  final def isTopLevel: Boolean = wrappedElem.elemPath.entries.size == 1

  /**
   * Returns true if and only if the attribute declaration is a reference to another (global) attribute declaration.
   * Top level attribute declarations are never references.
   */
  final def isReference: Boolean = refOption.isDefined

  /**
   * Returns the value of the 'id' attribute, if any, wrapped in an Option.
   */
  final def idOption: Option[String] = SchemaObjects.idOption(wrappedElem)

  /**
   * Returns the value of the 'type' attribute as expanded name, if any, wrapped in an Option.
   */
  final def typeAttributeOption: Option[EName] = SchemaObjects.typeAttributeOption(wrappedElem)

  /**
   * Returns the value of the 'ref' attribute as expanded name, if any, wrapped in an Option.
   */
  final def refOption: Option[EName] = SchemaObjects.refOption(wrappedElem)
}

/**
 * Schema type definition, which is either a simple type or a complex type.
 */
abstract class TypeDefinition private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(wrappedElem, allChildElems)

/**
 * Simple type definition. That is, the "simpleType" XML element.
 */
final class SimpleTypeDefinition private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends TypeDefinition(wrappedElem, allChildElems) {
  SchemaObjects.checkSimpleTypeDefinitionElem(wrappedElem)

  /**
   * Expensive auxiliary constructor.
   */
  def this(wrappedElem: indexed.Elem) =
    this(wrappedElem, wrappedElem.allChildElems.map(e => SchemaObject(e)))
}

/**
 * Complex type definition. That is, the "complexType" XML element.
 */
final class ComplexTypeDefinition private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends TypeDefinition(wrappedElem, allChildElems) {
  SchemaObjects.checkComplexTypeDefinitionElem(wrappedElem)

  /**
   * Expensive auxiliary constructor.
   */
  def this(wrappedElem: indexed.Elem) =
    this(wrappedElem, wrappedElem.allChildElems.map(e => SchemaObject(e)))
}

/**
 * Attribute group definition. That is, the "attributeGroup" XML element.
 */
final class AttributeGroupDefinition private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(wrappedElem, allChildElems) {
  SchemaObjects.checkAttributeGroupDefinitionElem(wrappedElem)

  /**
   * Expensive auxiliary constructor.
   */
  def this(wrappedElem: indexed.Elem) =
    this(wrappedElem, wrappedElem.allChildElems.map(e => SchemaObject(e)))
}

/**
 * Identity constraint definition. That is, the "key", "keyref" or "unique" XML element.
 */
final class IdentityConstraintDefinition private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(wrappedElem, allChildElems) {
  SchemaObjects.checkIdentityConstraintDefinitionElem(wrappedElem)

  /**
   * Expensive auxiliary constructor.
   */
  def this(wrappedElem: indexed.Elem) =
    this(wrappedElem, wrappedElem.allChildElems.map(e => SchemaObject(e)))
}

/**
 * Model group definition. That is, the "group" XML element.
 */
final class ModelGroupDefinition private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends Particle(wrappedElem, allChildElems) {
  SchemaObjects.checkModelGroupDefinitionElem(wrappedElem)

  /**
   * Expensive auxiliary constructor.
   */
  def this(wrappedElem: indexed.Elem) =
    this(wrappedElem, wrappedElem.allChildElems.map(e => SchemaObject(e)))
}

/**
 * Notation declaration. That is, the "notation" XML element.
 */
final class NotationDeclaration private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(wrappedElem, allChildElems) {
  SchemaObjects.checkNotationDeclarationElem(wrappedElem)

  /**
   * Expensive auxiliary constructor.
   */
  def this(wrappedElem: indexed.Elem) =
    this(wrappedElem, wrappedElem.allChildElems.map(e => SchemaObject(e)))
}

/**
 * Model group. That is, the "all", "sequence" or "choice" XML element.
 */
final class ModelGroup private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends Particle(wrappedElem, allChildElems) {
  SchemaObjects.checkModelGroupElem(wrappedElem)

  /**
   * Expensive auxiliary constructor.
   */
  def this(wrappedElem: indexed.Elem) =
    this(wrappedElem, wrappedElem.allChildElems.map(e => SchemaObject(e)))
}

/**
 * Wildcard. That is, the "any" or "anyAttribute" XML element.
 */
final class Wildcard private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends Particle(wrappedElem, allChildElems) {
  SchemaObjects.checkWildcardElem(wrappedElem)

  /**
   * Expensive auxiliary constructor.
   */
  def this(wrappedElem: indexed.Elem) =
    this(wrappedElem, wrappedElem.allChildElems.map(e => SchemaObject(e)))
}

/**
 * Annotation schema component.
 */
final class Annotation private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(wrappedElem, allChildElems) {
  SchemaObjects.checkAnnotationElem(wrappedElem)

  /**
   * Expensive auxiliary constructor.
   */
  def this(wrappedElem: indexed.Elem) =
    this(wrappedElem, wrappedElem.allChildElems.map(e => SchemaObject(e)))
}

object SchemaComponent {

  def apply(elem: indexed.Elem): SchemaComponent = {
    wrapOption(elem).getOrElse(sys.error("%s is not a schema component".format(elem.resolvedName)))
  }

  def wrapOption(elem: indexed.Elem): Option[SchemaComponent] = elem match {
    case e if e.resolvedName == EName(ns, "element") => Some(new ElementDeclaration(e))
    case e if e.resolvedName == EName(ns, "attribute") => Some(new AttributeDeclaration(e))
    case e if e.resolvedName == EName(ns, "simpleType") => Some(new SimpleTypeDefinition(e))
    case e if e.resolvedName == EName(ns, "complexType") => Some(new ComplexTypeDefinition(e))
    case e if e.resolvedName == EName(ns, "attributeGroup") => Some(new AttributeGroupDefinition(e))
    case e if e.resolvedName == EName(ns, "key") => Some(new IdentityConstraintDefinition(e))
    case e if e.resolvedName == EName(ns, "keyref") => Some(new IdentityConstraintDefinition(e))
    case e if e.resolvedName == EName(ns, "unique") => Some(new IdentityConstraintDefinition(e))
    case e if e.resolvedName == EName(ns, "group") => Some(new ModelGroupDefinition(e))
    case e if e.resolvedName == EName(ns, "notation") => Some(new NotationDeclaration(e))
    case e if e.resolvedName == EName(ns, "all") => Some(new ModelGroup(e))
    case e if e.resolvedName == EName(ns, "sequence") => Some(new ModelGroup(e))
    case e if e.resolvedName == EName(ns, "choice") => Some(new ModelGroup(e))
    case e if e.resolvedName == EName(ns, "any") => Some(new Wildcard(e))
    case e if e.resolvedName == EName(ns, "anyAttribute") => Some(new Wildcard(e))
    case e if e.resolvedName == EName(ns, "annotation") => Some(new Annotation(e))
    case e => None
  }
}

// TODO Other schema parts, that are not Schema Components themselves

object SchemaObject {

  trait HasParent[E <: HasParent[E]] { self: E =>

    /**
     * Returns the parent element, if any, wrapped in an Option
     */
    def parentOption: Option[E]

    /**
     * Returns the equivalent `parentOption.get`, throwing an exception if this is the root element
     */
    final def parent: E = parentOption.getOrElse(sys.error("There is no parent element"))

    /**
     * Returns all ancestor elements or self
     */
    final def ancestorsOrSelf: immutable.IndexedSeq[E] =
      self +: (parentOption.toIndexedSeq flatMap ((e: E) => e.ancestorsOrSelf))

    /**
     * Returns `ancestorsOrSelf.drop(1)`
     */
    final def ancestors: immutable.IndexedSeq[E] = ancestorsOrSelf.drop(1)

    /**
     * Returns the first found ancestor-or-self element obeying the given predicate, if any, wrapped in an Option
     */
    final def findAncestorOrSelf(p: E => Boolean): Option[E] = ancestorsOrSelf find p

    /**
     * Returns the first found ancestor element obeying the given predicate, if any, wrapped in an Option
     */
    final def findAncestor(p: E => Boolean): Option[E] = ancestors find p
  }

  def apply(wrappedElem: indexed.Elem): SchemaObject = wrappedElem match {
    // TODO
    case e if e.resolvedName == EName(ns, "schema") => new Schema(wrappedElem)
    case e if Set(
      EName(ns, "element"),
      EName(ns, "attribute"),
      EName(ns, "simpleType"),
      EName(ns, "complexType"),
      EName(ns, "attributeGroup"),
      EName(ns, "key"),
      EName(ns, "keyref"),
      EName(ns, "unique"),
      EName(ns, "group"),
      EName(ns, "notation"),
      EName(ns, "all"),
      EName(ns, "sequence"),
      EName(ns, "choice"),
      EName(ns, "any"),
      EName(ns, "anyAttribute"),
      EName(ns, "annotation")).contains(e.resolvedName) => SchemaComponent(wrappedElem)
    case _ => new SchemaObject(wrappedElem) {}
  }
}

object Particle {

  val Unbounded = -1
}
