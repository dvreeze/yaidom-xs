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
package schema

import java.net.URI
import scala.collection.immutable
import eu.cdevreeze.yaidom._
import SchemaObjects._
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
sealed abstract class SchemaObject private[schema] (
  val wrappedElem: indexed.Elem,
  val allChildElems: immutable.IndexedSeq[SchemaObject]) extends ElemLike[SchemaObject] with HasText with Immutable {

  require(wrappedElem ne null)
  require(allChildElems ne null)

  require(wrappedElem.findAllChildElems == allChildElems.map(_.wrappedElem), "Inconsistent SchemaObject")

  require(wrappedElem.rootElem.resolvedName == enameSchema, "The root of the element tree must be a 'schema' element")
  require(
    (wrappedElem.resolvedName == enameSchema) || (!wrappedElem.elemPath.isRoot),
    "This element must either be a 'schema' element, or not be the root of the element tree")

  final override def findAllChildElems: immutable.IndexedSeq[SchemaObject] = allChildElems

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
   * Returns all element declarations inside this SchemaObject (excluding self).
   */
  final def findAllElementDeclarations: immutable.IndexedSeq[ElementDeclaration] =
    this.findAllElems collect { case e: ElementDeclaration => e }

  /**
   * Returns all element declarations obeying the given predicate (excluding self).
   */
  final def filterElementDeclarations(p: ElementDeclaration => Boolean): immutable.IndexedSeq[ElementDeclaration] =
    this.findAllElems collect { case e: ElementDeclaration if p(e) => e }

  /**
   * Returns all topmost element declarations inside this SchemaObject (excluding self) obeying the given predicate.
   * Note that "topmost" is not the same as "top-level" (which only makes sense for the Schema object).
   */
  final def findTopmostElementDeclarations(p: ElementDeclaration => Boolean): immutable.IndexedSeq[ElementDeclaration] =
    this findTopmostElems { e =>
      e.resolvedName == enameElement && p(e.asInstanceOf[ElementDeclaration])
    } collect { case e: ElementDeclaration => e }

  /**
   * Returns all attribute declarations inside this SchemaObject (excluding self).
   */
  final def findAllAttributeDeclarations: immutable.IndexedSeq[AttributeDeclaration] =
    this.findAllElems collect { case e: AttributeDeclaration => e }

  /**
   * Returns all attribute declarations obeying the given predicate (excluding self).
   */
  final def filterAttributeDeclarations(p: AttributeDeclaration => Boolean): immutable.IndexedSeq[AttributeDeclaration] =
    this.findAllElems collect { case e: AttributeDeclaration if p(e) => e }

  /**
   * Returns all topmost attribute declarations inside this SchemaObject (excluding self) obeying the given predicate.
   * Note that "topmost" is not the same as "top-level" (which only makes sense for the Schema object).
   */
  final def findTopmostAttributeDeclarations(p: AttributeDeclaration => Boolean): immutable.IndexedSeq[AttributeDeclaration] =
    this findTopmostElems { e =>
      e.resolvedName == enameAttribute && p(e.asInstanceOf[AttributeDeclaration])
    } collect { case e: AttributeDeclaration => e }

  /**
   * Returns all type definitions inside this SchemaObject (excluding self).
   */
  final def findAllTypeDefinitions: immutable.IndexedSeq[TypeDefinition] =
    this.findAllElems collect { case e: TypeDefinition => e }

  /**
   * Returns all type declarations obeying the given predicate (excluding self).
   */
  final def filterTypeDefinitions(p: TypeDefinition => Boolean): immutable.IndexedSeq[TypeDefinition] =
    this.findAllElems collect { case e: TypeDefinition if p(e) => e }

  /**
   * Returns all topmost type declarations inside this SchemaObject (excluding self) obeying the given predicate.
   * Note that "topmost" is not the same as "top-level" (which only makes sense for the Schema object).
   */
  final def findTopmostTypeDefinitions(p: TypeDefinition => Boolean): immutable.IndexedSeq[TypeDefinition] =
    this findTopmostElems { e =>
      Set(enameComplexType, enameSimpleType).contains(e.resolvedName) && p(e.asInstanceOf[TypeDefinition])
    } collect { case e: TypeDefinition => e }
}

/**
 * XML Schema (from one document). That is, the "xs:schema" XML element.
 *
 * This is what the XML Schema specification calls a schema document, or the document element thereof.
 * In the abstract schema model of the specification, a schema is represented by one or more of these "schema documents".
 */
final class Schema private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(wrappedElem, allChildElems) {

  SchemaObjects.checkSchemaElem(wrappedElem)

  final def targetNamespaceOption: Option[String] = wrappedElem \@ enameTargetNamespace

  /**
   * Returns all top-level element declarations.
   */
  final def findAllTopLevelElementDeclarations: immutable.IndexedSeq[ElementDeclaration] =
    findTopmostElementDeclarations { e => e.isTopLevel }

  /**
   * Returns all top-level element declarations obeying the given predicate.
   */
  final def filterTopLevelElementDeclarations(p: ElementDeclaration => Boolean): immutable.IndexedSeq[ElementDeclaration] =
    this.findAllElems collect { case e: ElementDeclaration if e.isTopLevel && p(e) => e }

  /**
   * Finds the top-level element declaration with the given EName, if any, wrapped in an Option.
   */
  final def findTopLevelElementDeclarationByEName(ename: EName): Option[ElementDeclaration] =
    filterTopLevelElementDeclarations(_.enameOption == Some(ename)).headOption

  /**
   * Returns all top-level attribute declarations.
   */
  final def findAllTopLevelAttributeDeclarations: immutable.IndexedSeq[AttributeDeclaration] =
    findTopmostAttributeDeclarations { e => e.isTopLevel }

  /**
   * Returns all top-level attribute declarations obeying the given predicate.
   */
  final def filterTopLevelAttributeDeclarations(p: AttributeDeclaration => Boolean): immutable.IndexedSeq[AttributeDeclaration] =
    this.findAllElems collect { case e: AttributeDeclaration if e.isTopLevel && p(e) => e }

  /**
   * Returns all top-level element declarations that have precisely the given substitution group.
   */
  final def findAllDirectSubstitutables(substGroup: EName): immutable.IndexedSeq[ElementDeclaration] = {
    val substGroupOption = Some(substGroup)
    filterTopLevelElementDeclarations { e => e.substitutionGroupOption == substGroupOption }
  }

  /**
   * Returns all top-level element declarations that have one of the given substitution groups.
   */
  final def findAllDirectSubstitutables(substGroups: Set[EName]): immutable.IndexedSeq[ElementDeclaration] = {
    val substGroupOptions = substGroups map { sg => Option(sg) }
    filterTopLevelElementDeclarations { e => substGroupOptions.contains(e.substitutionGroupOption) }
  }

  /**
   * Returns all imports.
   */
  final def findAllImports: immutable.IndexedSeq[Import] =
    this.findAllElems collect { case e: Import => e }

  /**
   * Returns all includes.
   */
  final def findAllIncludes: immutable.IndexedSeq[Include] =
    this.findAllElems collect { case e: Include => e }

  /**
   * Returns all redefines.
   */
  final def findAllRedefines: immutable.IndexedSeq[Redefine] =
    this.findAllElems collect { case e: Redefine => e }
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
abstract class SchemaComponent private[schema] (
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
 */
trait Particle extends SchemaComponent {

  def wrappedElem: indexed.Elem

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
sealed class ElementDeclaration private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(wrappedElem, allChildElems) {

  SchemaObjects.checkElementDeclarationElem(wrappedElem)

  if (isTopLevel) {
    SchemaObjects.checkNotAParticleElem(wrappedElem)
  } else {
    SchemaObjects.checkParticleElem(wrappedElem)
  }

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
    val tnsOption = this.rootElem.attributeOption(enameTargetNamespace)

    if (isTopLevel) tnsOption
    else if (isReference) None
    else {
      val formOption = this.wrappedElem.attributeOption(enameForm)
      val elementFormDefaultOption = this.rootElem.attributeOption(enameElementFormDefault)

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
  final def nameAttributeOption: Option[String] = this.wrappedElem \@ enameName

  /**
   * Returns the "scope", if any, wrapped in an Option.
   *
   * That is, if this element declaration is not a reference, and has a complex type definition as ancestor, that complex
   * type definition is returned as indexed.Elem, wrapped in an Option. In all other cases, None is returned.
   */
  final def scopeOption: Option[indexed.Elem] = {
    if (isTopLevel) None
    else if (isReference) None
    else {
      val complexTypeOption = this.wrappedElem findAncestor { e => e.resolvedName == enameComplexType }
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
 * particles and (for example) element declarations.
 */
trait AttributeUse extends SchemaComponent {

  def wrappedElem: indexed.Elem

  // TODO
}

/**
 * Attribute declaration. That is, the "xs:attribute" XML element.
 */
sealed class AttributeDeclaration private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(wrappedElem, allChildElems) {

  SchemaObjects.checkAttributeDeclarationElem(wrappedElem)

  if (isTopLevel) {
    SchemaObjects.checkNotAnAttributeUseElem(wrappedElem)
  } else {
    SchemaObjects.checkAttributeUseElem(wrappedElem)
  }

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
    val tnsOption = this.rootElem.attributeOption(enameTargetNamespace)

    if (isTopLevel) tnsOption
    else if (isReference) None
    else {
      val formOption = this.wrappedElem.attributeOption(enameForm)
      val attributeFormDefaultOption = this.rootElem.attributeOption(enameAttributeFormDefault)

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
  final def nameAttributeOption: Option[String] = this.wrappedElem \@ enameName

  /**
   * Returns the "scope", if any, wrapped in an Option.
   *
   * That is, if this attribute declaration is not a reference, and has a complex type definition as ancestor, that complex
   * type definition is returned as indexed.Elem, wrapped in an Option. In all other cases, None is returned.
   */
  final def scopeOption: Option[indexed.Elem] = {
    if (isTopLevel) None
    else if (isReference) None
    else {
      val complexTypeOption = this.wrappedElem findAncestor { e => e.resolvedName == enameComplexType }
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
abstract class TypeDefinition private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(wrappedElem, allChildElems)

/**
 * Simple type definition. That is, the "xs:simpleType" XML element.
 */
final class SimpleTypeDefinition private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends TypeDefinition(wrappedElem, allChildElems) {

  SchemaObjects.checkSimpleTypeDefinitionElem(wrappedElem)

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(enameTargetNamespace)
  }
}

/**
 * Complex type definition. That is, the "xs:complexType" XML element.
 */
final class ComplexTypeDefinition private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends TypeDefinition(wrappedElem, allChildElems) {

  SchemaObjects.checkComplexTypeDefinitionElem(wrappedElem)

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(enameTargetNamespace)
  }
}

/**
 * Attribute group definition. That is, the "xs:attributeGroup" XML element.
 */
final class AttributeGroupDefinition private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(wrappedElem, allChildElems) {

  SchemaObjects.checkAttributeGroupDefinitionElem(wrappedElem)

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(enameTargetNamespace)
  }
}

/**
 * Identity constraint definition. That is, the "xs:key", "xs:keyref" or "xs:unique" XML element.
 */
final class IdentityConstraintDefinition private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(wrappedElem, allChildElems) {

  SchemaObjects.checkIdentityConstraintDefinitionElem(wrappedElem)

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(enameTargetNamespace)
  }
}

/**
 * Model group definition. That is, the "xs:group" XML element.
 */
sealed class ModelGroupDefinition private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(wrappedElem, allChildElems) {

  SchemaObjects.checkModelGroupDefinitionElem(wrappedElem)

  if (refOption.isEmpty) {
    SchemaObjects.checkNotAParticleElem(wrappedElem)
  } else {
    SchemaObjects.checkParticleElem(wrappedElem)
  }

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(enameTargetNamespace)
  }

  /**
   * Returns the value of the 'ref' attribute as expanded name, if any, wrapped in an Option.
   */
  final def refOption: Option[EName] = SchemaObjects.refOption(wrappedElem)
}

/**
 * Notation declaration. That is, the "xs:notation" XML element.
 */
final class NotationDeclaration private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(wrappedElem, allChildElems) {

  SchemaObjects.checkNotationDeclarationElem(wrappedElem)

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(enameTargetNamespace)
  }
}

/**
 * Model group. That is, the "xs:all", "xs:sequence" or "xs:choice" XML element.
 */
sealed class ModelGroup private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(wrappedElem, allChildElems) {

  SchemaObjects.checkModelGroupElem(wrappedElem)

  if (inNamedGroup) {
    SchemaObjects.checkNotAParticleElem(wrappedElem)
  } else {
    SchemaObjects.checkParticleElem(wrappedElem)
  }

  final override def targetNamespaceOption: Option[String] = None

  final def inNamedGroup: Boolean = {
    assert(wrappedElem.parentOption.isDefined)
    val parent = wrappedElem.parent

    (parent.resolvedName == enameGroup) && ((parent \@ enameName).isDefined)
  }
}

/**
 * Wildcard. That is, the "xs:any" or "xs:anyAttribute" XML element.
 */
sealed class Wildcard private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(wrappedElem, allChildElems) {

  SchemaObjects.checkWildcardElem(wrappedElem)

  if (wrappedElem.resolvedName == enameAnyAttribute) {
    SchemaObjects.checkNotAParticleElem(wrappedElem)
  } else {
    assert(wrappedElem.resolvedName == enameAny)
    SchemaObjects.checkParticleElem(wrappedElem)
  }

  final override def targetNamespaceOption: Option[String] = None
}

/**
 * Annotation schema component. That is, the "xs:annotation" XML element.
 */
final class Annotation private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(wrappedElem, allChildElems) {

  SchemaObjects.checkAnnotationElem(wrappedElem)

  final override def targetNamespaceOption: Option[String] = None
}

// Import, include, redefine

/**
 * The "xs:import" XML element.
 */
final class Import private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(wrappedElem, allChildElems) {

  SchemaObjects.checkImportElem(wrappedElem)
}

/**
 * The "xs:include" XML element.
 */
final class Include private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(wrappedElem, allChildElems) {

  SchemaObjects.checkIncludeElem(wrappedElem)
}

/**
 * The "xs:redefine" XML element.
 */
final class Redefine private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(wrappedElem, allChildElems) {

  SchemaObjects.checkRedefineElem(wrappedElem)
}

// Other schema parts, that are not Schema Components themselves, such as extension, restriction, etc.

/**
 * The "xs:complexContent" XML element.
 */
final class ComplexContent private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(wrappedElem, allChildElems) {

  SchemaObjects.checkComplexContentElem(wrappedElem)
}

/**
 * The "xs:simpleContent" XML element.
 */
final class SimpleContent private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(wrappedElem, allChildElems) {

  SchemaObjects.checkSimpleContentElem(wrappedElem)
}

/**
 * The "xs:extension" XML element.
 */
final class Extension private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(wrappedElem, allChildElems) {

  SchemaObjects.checkExtensionElem(wrappedElem)
}

/**
 * The "xs:restriction" XML element.
 */
final class Restriction private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(wrappedElem, allChildElems) {

  SchemaObjects.checkRestrictionElem(wrappedElem)
}

/**
 * The "xs:appinfo" XML element.
 */
final class Appinfo private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(wrappedElem, allChildElems) {

  require(wrappedElem.resolvedName == enameAppinfo, "The element must be an 'appinfo' element")
}

/**
 * The "xs:documentation" XML element.
 */
final class Documentation private[schema] (
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(wrappedElem, allChildElems) {

  require(wrappedElem.resolvedName == enameDocumentation, "The element must be a 'documentation' element")
}

// Companion objects

object Schema {

  def apply(elem: indexed.Elem): Schema =
    new Schema(elem, SchemaObject.childSchemaObjects(elem))
}

object ElementDeclaration {

  /**
   * Creates an `ElementDeclaration` from an `indexed.Elem`. If not top-level, the result is also a `Particle`.
   */
  def apply(elem: indexed.Elem): ElementDeclaration = {
    def isTopLevel: Boolean = elem.elemPath.entries.size == 1

    if (isTopLevel) new ElementDeclaration(elem, childSchemaObjects(elem))
    else new ElementDeclaration(elem, childSchemaObjects(elem)) with Particle
  }
}

object ModelGroupDefinition {

  /**
   * Creates a `ModelGroupDefinition` from an `indexed.Elem`. If it has a "ref" attribute, the result is also a `Particle`.
   */
  def apply(elem: indexed.Elem): ModelGroupDefinition = {
    if ((elem \@ enameRef).isEmpty) new ModelGroupDefinition(elem, childSchemaObjects(elem))
    else new ModelGroupDefinition(elem, childSchemaObjects(elem)) with Particle
  }
}

object ModelGroup {

  /**
   * Creates a `ModelGroup` from an `indexed.Elem`. If not inside a named group (xs:group with "name" attribute),
   * the result is also a `Particle`.
   */
  def apply(elem: indexed.Elem): ModelGroup = {
    if (inNamedGroup(elem)) new ModelGroup(elem, childSchemaObjects(elem))
    else new ModelGroup(elem, childSchemaObjects(elem)) with Particle
  }

  private def inNamedGroup(elem: indexed.Elem): Boolean = {
    assert(elem.parentOption.isDefined)
    val parent = elem.parent

    (parent.resolvedName == enameGroup) && ((parent \@ enameName).isDefined)
  }
}

object Wildcard {

  /**
   * Creates a `Wildcard` from an `indexed.Elem`. If it is an xs:any, the result is also a `Particle`.
   */
  def apply(elem: indexed.Elem): Wildcard = {
    if (elem.resolvedName == enameAnyAttribute) new Wildcard(elem, childSchemaObjects(elem))
    else new Wildcard(elem, childSchemaObjects(elem)) with Particle
  }
}

object AttributeDeclaration {

  /**
   * Creates an `AttributeDeclaration` from an `indexed.Elem`. If not top-level, the result is also an `AttributeUse`.
   */
  def apply(elem: indexed.Elem): AttributeDeclaration = {
    def isTopLevel: Boolean = elem.elemPath.entries.size == 1

    if (isTopLevel) new AttributeDeclaration(elem, childSchemaObjects(elem))
    else new AttributeDeclaration(elem, childSchemaObjects(elem)) with AttributeUse
  }
}

object SchemaComponent {

  def apply(elem: indexed.Elem): SchemaComponent = {
    wrapOption(elem).getOrElse(sys.error("%s is not a schema component".format(elem.resolvedName)))
  }

  def wrapOption(elem: indexed.Elem): Option[SchemaComponent] = {
    import SchemaObject._

    elem match {
      case e if e.resolvedName == enameElement => Some(ElementDeclaration(e))
      case e if e.resolvedName == enameAttribute => Some(AttributeDeclaration(e))
      case e if e.resolvedName == enameSimpleType => Some(new SimpleTypeDefinition(e, childSchemaObjects(e)))
      case e if e.resolvedName == enameComplexType => Some(new ComplexTypeDefinition(e, childSchemaObjects(e)))
      case e if e.resolvedName == enameAttributeGroup => Some(new AttributeGroupDefinition(e, childSchemaObjects(e)))
      case e if e.resolvedName == enameKey => Some(new IdentityConstraintDefinition(e, childSchemaObjects(e)))
      case e if e.resolvedName == enameKeyref => Some(new IdentityConstraintDefinition(e, childSchemaObjects(e)))
      case e if e.resolvedName == enameUnique => Some(new IdentityConstraintDefinition(e, childSchemaObjects(e)))
      case e if e.resolvedName == enameGroup => Some(ModelGroupDefinition(e))
      case e if e.resolvedName == enameNotation => Some(new NotationDeclaration(e, childSchemaObjects(e)))
      case e if e.resolvedName == enameAll => Some(ModelGroup(e))
      case e if e.resolvedName == enameSequence => Some(ModelGroup(e))
      case e if e.resolvedName == enameChoice => Some(ModelGroup(e))
      case e if e.resolvedName == enameAny => Some(Wildcard(e))
      case e if e.resolvedName == enameAnyAttribute => Some(Wildcard(e))
      case e if e.resolvedName == enameAnnotation => Some(new Annotation(e, childSchemaObjects(e)))
      case e => None
    }
  }
}

object SchemaObject {

  def apply(wrappedElem: indexed.Elem): SchemaObject = wrappedElem match {
    // TODO
    case e if e.resolvedName == enameSchema => new Schema(wrappedElem, childSchemaObjects(wrappedElem))
    case e if Set(
      enameElement,
      enameAttribute,
      enameSimpleType,
      enameComplexType,
      enameAttributeGroup,
      enameKey,
      enameKeyref,
      enameUnique,
      enameGroup,
      enameNotation,
      enameAll,
      enameSequence,
      enameChoice,
      enameAny,
      enameAnyAttribute,
      enameAnnotation).contains(e.resolvedName) => SchemaComponent(wrappedElem)
    case e if e.resolvedName == enameImport => new Import(wrappedElem, childSchemaObjects(wrappedElem))
    case e if e.resolvedName == enameInclude => new Include(wrappedElem, childSchemaObjects(wrappedElem))
    case e if e.resolvedName == enameRedefine => new Redefine(wrappedElem, childSchemaObjects(wrappedElem))
    case e if e.resolvedName == enameComplexContent => new ComplexContent(wrappedElem, childSchemaObjects(wrappedElem))
    case e if e.resolvedName == enameSimpleContent => new SimpleContent(wrappedElem, childSchemaObjects(wrappedElem))
    case e if e.resolvedName == enameAppinfo => new Appinfo(wrappedElem, childSchemaObjects(wrappedElem))
    case e if e.resolvedName == enameDocumentation => new Documentation(wrappedElem, childSchemaObjects(wrappedElem))
    case e if e.resolvedName == enameExtension => new Extension(wrappedElem, childSchemaObjects(wrappedElem))
    case e if e.resolvedName == enameRestriction => new Restriction(wrappedElem, childSchemaObjects(wrappedElem))
    case _ => new SchemaObject(wrappedElem, childSchemaObjects(wrappedElem)) {}
  }

  private[schema] def childSchemaObjects(e: indexed.Elem): immutable.IndexedSeq[SchemaObject] =
    e.findAllChildElems.map(e => SchemaObject(e))
}

object Particle {

  val Unbounded = -1
}
