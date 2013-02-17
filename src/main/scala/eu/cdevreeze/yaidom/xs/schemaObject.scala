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
 * A well-constructed `SchemaObject` obeys a few (unsurprising) properties, such as:
 * {{{
 * wrappedElem.rootElem.resolvedName == EName("http://www.w3.org/2001/XMLSchema", "schema")
 * }}}
 * and
 * {{{
 * wrappedElem.allChildElems == allChildElems.map(_.wrappedElem)
 * }}}
 * The package-private primary constructor indeed enforces these properties.
 *
 * @author Chris de Vreeze
 */
sealed abstract class SchemaObject private[xs] (
  val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends ElemLike[SchemaObject] with HasParent[SchemaObject] with HasText with Immutable {

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

  /**
   * Returns true if this SchemaObject has the same `wrappedElem` as the passed object, if it is also a SchemaObject.
   */
  final override def equals(obj: Any): Boolean = obj match {
    case other: SchemaObject => (other.wrappedElem == this.wrappedElem)
    case _ => false
  }

  /**
   * Returns `wrappedElem.hashCode`
   */
  final override def hashCode: Int = wrappedElem.hashCode

  final override def text: String = wrappedElem.text

  /**
   * Returns the parent SchemaObject, if any, wrapped in an Option.
   *
   * The wrapped SchemaObject, if any, `equals` any parent SchemaObject found by querying for it from any ancestor.
   */
  final override def parentOption: Option[SchemaObject] =
    wrappedElem.parentOption map { e => SchemaObject(e) }

  final override def toString: String = wrappedElem.elem.toString

  /**
   * Returns `wrappedElem.rootElem`
   */
  final def rootElem: Elem = wrappedElem.rootElem

  /**
   * Returns `wrappedElem.elemPath`
   */
  final def elemPath: ElemPath = wrappedElem.elemPath

  /**
   * Returns the root element as Schema object.
   */
  final def schema: Schema = {
    val resultOption = this.ancestors collectFirst { case e: Schema => e }
    assert(resultOption.isDefined)
    assert(resultOption.get.elemPath.isRoot)
    resultOption.get
  }

  /**
   * Returns all element declarations inside this SchemaObject (excluding self).
   */
  final def elementDeclarations: immutable.IndexedSeq[ElementDeclaration] =
    this collectFromElems { case e: ElementDeclaration => e }

  /**
   * Returns all topmost element declarations inside this SchemaObject (excluding self).
   * Note that "topmost" is not the same as "top-level" (which only makes sense for the Schema object).
   */
  final def topmostElementDeclarations: immutable.IndexedSeq[ElementDeclaration] =
    this findTopmostElems { e => e.resolvedName == EName(ns, "element") } collect { case e: ElementDeclaration => e }

  /**
   * Returns all attribute declarations inside this SchemaObject (excluding self).
   */
  final def attributeDeclarations: immutable.IndexedSeq[AttributeDeclaration] =
    this collectFromElems { case e: AttributeDeclaration => e }

  /**
   * Returns all topmost attribute declarations inside this SchemaObject (excluding self).
   * Note that "topmost" is not the same as "top-level" (which only makes sense for the Schema object).
   */
  final def topmostAttributeDeclarations: immutable.IndexedSeq[AttributeDeclaration] =
    this findTopmostElems { e => e.resolvedName == EName(ns, "attribute") } collect { case e: AttributeDeclaration => e }

  /**
   * Returns all type definitions inside this SchemaObject (excluding self).
   */
  final def typeDefinitions: immutable.IndexedSeq[TypeDefinition] =
    this collectFromElems { case e: TypeDefinition => e }

  /**
   * Returns all topmost type definitions inside this SchemaObject (excluding self).
   * Note that "topmost" is not the same as "top-level" (which only makes sense for the Schema object).
   */
  final def topmostTypeDefinitions: immutable.IndexedSeq[TypeDefinition] =
    this findTopmostElems { e => Set(EName(ns, "complexType"), EName(ns, "simpleType")).contains(e.resolvedName) } collect
      { case e: TypeDefinition => e }
}

/**
 * XML Schema (from one document). That is, the "xs:schema" XML element.
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
   * Returns all top-level element declarations.
   */
  final def topLevelElementDeclarations: immutable.IndexedSeq[ElementDeclaration] =
    topmostElementDeclarations filter { e => e.isTopLevel }

  /**
   * Returns all top-level attribute declarations.
   */
  final def topLevelAttributeDeclarations: immutable.IndexedSeq[AttributeDeclaration] =
    topmostAttributeDeclarations filter { e => e.isTopLevel }

  /**
   * Returns all imports.
   */
  final def imports: immutable.IndexedSeq[Import] =
    this collectFromElems { case e: Import => e }

  /**
   * Returns all includes.
   */
  final def includes: immutable.IndexedSeq[Include] =
    this collectFromElems { case e: Include => e }

  /**
   * Returns all redefines.
   */
  final def redefines: immutable.IndexedSeq[Redefine] =
    this collectFromElems { case e: Redefine => e }
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
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(wrappedElem, allChildElems) {

  /**
   * Returns the target namespace of the schema component, if any, wrapped in an Option.
   * Some types of schema component do not have the notion of a target namespace (in which case None is returned), but most do
   * (whether it is defined or not).
   */
  def targetNamespaceOption: Option[String]
}

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
 * Element declaration. That is, the "xs:element" XML element.
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
  final def isTopLevel: Boolean = elemPath.entries.size == 1

  /**
   * Returns true if and only if the element declaration is a reference to another (global) element declaration.
   * Top level element declarations are never references.
   */
  final def isReference: Boolean = refOption.isDefined

  /**
   * Returns true if and only if the element declaration is abstract.
   * Only top level element declarations can be abstract.
   */
  final def isAbstract: Boolean = abstractOption == Some(true)

  /**
   * Returns the target namespace, if any, wrapped in an Option. The target namespace depends on the target namespace
   * of the schema root element, if any, and on the form and (schema root element) elementFormDefault attributes, if any.
   */
  final override def targetNamespaceOption: Option[String] = {
    val tnsOption = this.rootElem.attributeOption(EName("targetNamespace"))

    if (isTopLevel) tnsOption
    else if (isReference) None
    else {
      val formOption = this.wrappedElem.attributeOption(EName("form"))
      val elementFormDefaultOption = this.rootElem.attributeOption(EName("elementFormDefault"))

      if (formOption == Some("qualified")) tnsOption
      else if (formOption.isEmpty && (elementFormDefaultOption == Some("qualified"))) tnsOption
      else None
    }
  }

  /**
   * Returns the `EName` by combining the target namespace and the value of the "name" attribute,
   * if any, wrapped in an Option.
   */
  final def enameOption: Option[EName] = {
    val tnsOption = targetNamespaceOption
    val localNameOption = nameAttributeOption
    localNameOption map { nm => EName(tnsOption, nm) }
  }

  /**
   * Returns the value of the "name" attribute, if any, wrapped in an Option.
   */
  final def nameAttributeOption: Option[String] = this.wrappedElem \@ EName("name")

  /**
   * Returns the "scope", if any, wrapped in an Option.
   *
   * That is, if this element declaration is not a reference, and has a complex type definition as ancestor, that complex
   * type definition is returned, wrapped in an Option. In all other cases, None is returned.
   */
  final def scopeOption: Option[ComplexTypeDefinition] = {
    if (isTopLevel) None
    else if (isReference) None
    else {
      val complexTypeOption = this findAncestor {
        case complexTypeDef: ComplexTypeDefinition => true
        case _ => false
      } collect {
        case complexTypeDef: ComplexTypeDefinition => complexTypeDef
      }

      complexTypeOption
    }
  }

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
 * Attribute declaration. That is, the "xs:attribute" XML element.
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
  final def isTopLevel: Boolean = elemPath.entries.size == 1

  /**
   * Returns true if and only if the attribute declaration is a reference to another (global) attribute declaration.
   * Top level attribute declarations are never references.
   */
  final def isReference: Boolean = refOption.isDefined

  /**
   * Returns the target namespace, if any, wrapped in an Option. The target namespace depends on the target namespace
   * of the schema root element, if any, and on the form and (schema root element) attributeFormDefault attributes, if any.
   */
  final override def targetNamespaceOption: Option[String] = {
    val tnsOption = this.rootElem.attributeOption(EName("targetNamespace"))

    if (isTopLevel) tnsOption
    else if (isReference) None
    else {
      val formOption = this.wrappedElem.attributeOption(EName("form"))
      val attributeFormDefaultOption = this.rootElem.attributeOption(EName("attributeFormDefault"))

      if (formOption == Some("qualified")) tnsOption
      else if (formOption.isEmpty && (attributeFormDefaultOption == Some("qualified"))) tnsOption
      else None
    }
  }

  /**
   * Returns the `EName` by combining the target namespace and the value of the "name" attribute,
   * if any, wrapped in an Option.
   */
  final def enameOption: Option[EName] = {
    val tnsOption = targetNamespaceOption
    val localNameOption = nameAttributeOption
    localNameOption map { nm => EName(tnsOption, nm) }
  }

  /**
   * Returns the value of the "name" attribute, if any, wrapped in an Option.
   */
  final def nameAttributeOption: Option[String] = this.wrappedElem \@ EName("name")

  /**
   * Returns the "scope", if any, wrapped in an Option.
   *
   * That is, if this attribute declaration is not a reference, and has a complex type definition as ancestor, that complex
   * type definition is returned, wrapped in an Option. In all other cases, None is returned.
   */
  final def scopeOption: Option[ComplexTypeDefinition] = {
    if (isTopLevel) None
    else if (isReference) None
    else {
      val complexTypeOption = this findAncestor {
        case complexTypeDef: ComplexTypeDefinition => true
        case _ => false
      } collect {
        case complexTypeDef: ComplexTypeDefinition => complexTypeDef
      }

      complexTypeOption
    }
  }

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
 * Simple type definition. That is, the "xs:simpleType" XML element.
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

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(EName("targetNamespace"))
  }
}

/**
 * Complex type definition. That is, the "xs:complexType" XML element.
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

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(EName("targetNamespace"))
  }
}

/**
 * Attribute group definition. That is, the "xs:attributeGroup" XML element.
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

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(EName("targetNamespace"))
  }
}

/**
 * Identity constraint definition. That is, the "xs:key", "xs:keyref" or "xs:unique" XML element.
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

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(EName("targetNamespace"))
  }
}

/**
 * Model group definition. That is, the "xs:group" XML element.
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

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(EName("targetNamespace"))
  }
}

/**
 * Notation declaration. That is, the "xs:notation" XML element.
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

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(EName("targetNamespace"))
  }
}

/**
 * Model group. That is, the "xs:all", "xs:sequence" or "xs:choice" XML element.
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

  final override def targetNamespaceOption: Option[String] = None
}

/**
 * Wildcard. That is, the "xs:any" or "xs:anyAttribute" XML element.
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

  final override def targetNamespaceOption: Option[String] = None
}

/**
 * Annotation schema component. That is, the "xs:annotation" XML element.
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

  final override def targetNamespaceOption: Option[String] = None
}

// Import, include, redefine

/**
 * The "xs:import" XML element.
 */
final class Import private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(wrappedElem, allChildElems) {

  SchemaObjects.checkImportElem(wrappedElem)

  /**
   * Expensive auxiliary constructor.
   */
  def this(wrappedElem: indexed.Elem) =
    this(wrappedElem, wrappedElem.allChildElems.map(e => SchemaObject(e)))
}

/**
 * The "xs:include" XML element.
 */
final class Include private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(wrappedElem, allChildElems) {

  SchemaObjects.checkIncludeElem(wrappedElem)

  /**
   * Expensive auxiliary constructor.
   */
  def this(wrappedElem: indexed.Elem) =
    this(wrappedElem, wrappedElem.allChildElems.map(e => SchemaObject(e)))
}

/**
 * The "xs:redefine" XML element.
 */
final class Redefine private[xs] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(wrappedElem, allChildElems) {

  SchemaObjects.checkRedefineElem(wrappedElem)

  /**
   * Expensive auxiliary constructor.
   */
  def this(wrappedElem: indexed.Elem) =
    this(wrappedElem, wrappedElem.allChildElems.map(e => SchemaObject(e)))
}

// TODO Other schema parts, that are not Schema Components themselves

// Companion objects

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

object SchemaObject {

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
    case e if e.resolvedName == EName(ns, "import") => new Import(wrappedElem)
    case e if e.resolvedName == EName(ns, "include") => new Include(wrappedElem)
    case e if e.resolvedName == EName(ns, "redefine") => new Redefine(wrappedElem)
    case _ => new SchemaObject(wrappedElem) {}
  }
}

object Particle {

  val Unbounded = -1
}
