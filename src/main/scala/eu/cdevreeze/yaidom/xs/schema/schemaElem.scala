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
import SchemaElem._

/**
 * Immutable XML Schema or a part thereof. These elements offer the `ElemLike` API.
 *
 * Terminology is taken as much as possible from the book Definitive XML Schema, 2nd Edition (by Priscilla Walmsley).
 *
 * This class and its subclasses are meant for processing XML schema documents as XML schema data themselves.
 * For example, in order to retrieve data from a large set of XBRL taxonomy schemas, these classes should prove
 * very useful.
 *
 * In XML schema content the ancestry is typically important. For example, a global element declaration is very different
 * from a local element declaration. By keeping the ancestry in queried elements, many of these differences can be modeled
 * correctly.
 *
 * The ancestry of an element is kept by keeping an `indexed.Elem` as state. Redundantly, the `childElems` are also kept as
 * state, to make `ElemLike` API querying very fast, with little object creation overhead. The `SchemaElem` implementation
 * makes sure that the following indeed holds:
 * {{{
 * childElems.map(_.indexedElem) == indexedElem.findAllChildElems
 * }}}
 *
 * @author Chris de Vreeze
 */
sealed class SchemaElem private[schema] (
  val indexedElem: indexed.Elem,
  val childElems: immutable.IndexedSeq[SchemaElem],
  val docUri: URI) extends ElemLike[SchemaElem] with HasText with Immutable {

  /**
   * The yaidom Elem itself, stored as a val
   */
  final val elem: Elem = indexedElem.elem

  require(
    childElems.map(_.indexedElem) == indexedElem.findAllChildElems,
    "Corrupt element!")

  require(indexedElem.rootElem.resolvedName == XsSchemaEName, "The root of the element tree must be a 'schema' element")
  require(
    (elem.resolvedName == XsSchemaEName) || (!indexedElem.elemPath.isRoot),
    "This element must either be a 'schema' element, or not be the root of the element tree")

  /**
   * Returns all child elements, in the correct order. That is, returns `childElems`.
   *
   * These child elements share the same rootElem (and doc URI) with this element, but differ in the element paths, which have
   * one more "path entry".
   *
   * The implementation is extremely fast, which is needed for fast querying through SchemaApi query methods.
   */
  final override def findAllChildElems: immutable.IndexedSeq[SchemaElem] = childElems

  final override def resolvedName: EName = elem.resolvedName

  final override def resolvedAttributes: immutable.IndexedSeq[(EName, String)] = elem.resolvedAttributes

  final override def equals(obj: Any): Boolean = obj match {
    case other: SchemaElem =>
      (other.indexedElem == this.indexedElem) && (other.docUri == this.docUri)
    case _ => false
  }

  final override def hashCode: Int = (indexedElem, docUri).hashCode

  final override def text: String = elem.text

  final override def toString: String = elem.toString

  /**
   * Returns `elem.scope`
   */
  final def scope: Scope = elem.scope

  /**
   * Returns the namespaces declared in this element.
   *
   * If the original parsed XML document contained duplicate namespace declarations (i.e. namespace declarations that are the same
   * as some namespace declarations in their context), these duplicate namespace declarations were lost during parsing of the
   * XML into an `Elem` tree. They therefore do not occur in the namespace declarations returned by this method.
   */
  final def namespaces: Declarations = indexedElem.namespaces

  final def idOption: Option[String] = elem \@ IdEName

  /**
   * Returns the optional URI of this element, containing the id attribute value as URI fragment, if any.
   * If the id attribute is absent, None is returned.
   */
  final def uriOption: Option[URI] = idOption.map(id => new URI(docUri.getScheme, docUri.getSchemeSpecificPart, id))
}

/**
 * XML Schema (from one document). That is, the "xs:schema" XML element.
 *
 * This is what the XML Schema specification calls a schema document, or the document element thereof.
 * In the abstract schema model of the specification, a schema is represented by one or more of these "schema documents".
 */
final class SchemaRootElem private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaElem(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsSchemaEName, "The element must be a 'schema' element")
  require(indexedElem.elemPath.isRoot, "The element must be the root of the element tree")

  final def targetNamespaceOption: Option[String] = elem \@ TargetNamespaceEName

  /**
   * Returns all global element declarations.
   */
  final def findAllGlobalElementDeclarations: immutable.IndexedSeq[GlobalElementDeclaration] =
    filterChildElems { e => e.resolvedName == XsElementEName } collect { case e: GlobalElementDeclaration => e }

  /**
   * Returns all global element declarations obeying the given predicate.
   */
  final def filterGlobalElementDeclarations(p: GlobalElementDeclaration => Boolean): immutable.IndexedSeq[GlobalElementDeclaration] =
    filterChildElems { e => e.resolvedName == XsElementEName } collect { case e: GlobalElementDeclaration if p(e) => e }

  /**
   * Finds the global element declaration with the given EName, if any, wrapped in an Option.
   */
  final def findGlobalElementDeclarationByEName(ename: EName): Option[GlobalElementDeclaration] =
    filterGlobalElementDeclarations(_.enameOption == Some(ename)).headOption

  /**
   * Returns all global attribute declarations.
   */
  final def findAllGlobalAttributeDeclarations: immutable.IndexedSeq[GlobalAttributeDeclaration] =
    filterChildElems { e => e.resolvedName == XsAttributeEName } collect { case e: GlobalAttributeDeclaration => e }

  /**
   * Returns all global attribute declarations obeying the given predicate.
   */
  final def filterGlobalAttributeDeclarations(p: GlobalAttributeDeclaration => Boolean): immutable.IndexedSeq[GlobalAttributeDeclaration] =
    filterChildElems { e => e.resolvedName == XsAttributeEName } collect { case e: GlobalAttributeDeclaration if p(e) => e }

  /**
   * Returns all global element declarations that have a substitution group matching the given predicate on the
   * substitution group.
   */
  final def findAllDirectSubstitutables(p: EName => Boolean): immutable.IndexedSeq[GlobalElementDeclaration] = {
    filterGlobalElementDeclarations { e => e.substitutionGroupOption.isDefined && p(e.substitutionGroupOption.get) }
  }

  /**
   * Returns all imports.
   */
  final def findAllImports: immutable.IndexedSeq[Import] =
    filterElems { e => e.resolvedName == XsImportEName } collect { case e: Import => e }

  /**
   * Returns all includes.
   */
  final def findAllIncludes: immutable.IndexedSeq[Include] =
    filterElems { e => e.resolvedName == XsIncludeEName } collect { case e: Include => e }

  /**
   * Returns all redefines.
   */
  final def findAllRedefines: immutable.IndexedSeq[Redefine] =
    filterElems { e => e.resolvedName == XsRedefineEName } collect { case e: Redefine => e }
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
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaElem(indexedElem, childElems, docUri) {

  /**
   * Returns the target namespace of the schema component, if any, wrapped in an Option.
   *
   * Some types of schema component do not have the notion of a target namespace (in which case None is returned), but most do
   * (whether it is defined or not).
   */
  def targetNamespaceOption: Option[String]
}

/**
 * Particle, having a min and max occurs (possibly default).
 */
trait Particle extends SchemaComponent {

  final def minOccurs: Int = minOccursAttrOption map (_.toInt) getOrElse 1

  final def maxOccurs: Int = {
    maxOccursAttrOption map { v =>
      if (v.toLowerCase(java.util.Locale.ENGLISH) == "unbounded") Particle.Unbounded else 1
    } getOrElse 1
  }

  final def minOccursAttrOption: Option[String] = elem \@ MinOccursEName

  final def maxOccursAttrOption: Option[String] = elem \@ MaxOccursEName
}

/**
 * Element declaration or element reference. That is, the "xs:element" XML element.
 */
abstract class ElementDeclarationOrReference private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaComponent(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsElementEName, "The element must be an 'element' element")

  /**
   * Returns true if and only if the element declaration has the schema element as its parent.
   *
   * Element references are not considered global, because their parent is not the schema element, but they do always
   * refer to global element declarations.
   */
  final def isGlobal: Boolean = indexedElem.elemPath.entries.size == 1

  /**
   * Returns true if and only if the element declaration is a reference to another (global) element declaration.
   * Top level element declarations are never references.
   */
  final def isReference: Boolean = refOption.isDefined

  /**
   * Returns true if and only if the element declaration is abstract.
   * Only global element declarations can be abstract.
   */
  final def isAbstract: Boolean = abstractOption == Some(true)

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
  final def nameAttributeOption: Option[String] = elem \@ NameEName

  /**
   * Returns the "scope", if any, wrapped in an Option.
   *
   * That is, if this element declaration is not a reference, and has a complex type definition as ancestor, that complex
   * type definition is returned as indexed.Elem, wrapped in an Option. In all other cases, None is returned.
   */
  def scopeOption: Option[indexed.Elem]

  /**
   * Returns the value of the 'type' attribute as expanded name, if any, wrapped in an Option.
   */
  final def typeAttributeOption: Option[EName] = {
    val typeAttrAttrOption = elem \@ TypeEName
    typeAttrAttrOption map { tpe =>
      elem.scope.resolveQNameOption(QName(tpe)).getOrElse(
        sys.error("Could not resolve type '%s' as expanded name".format(tpe)))
    }
  }

  /**
   * Returns the value of the 'substitutionGroup' attribute as expanded name, if any, wrapped in an Option.
   */
  final def substitutionGroupOption: Option[EName] = {
    val substGroupAttrOption = elem \@ SubstitutionGroupEName
    substGroupAttrOption map { substGroup =>
      elem.scope.resolveQNameOption(QName(substGroup)).getOrElse(
        sys.error("Could not resolve substitution group '%s' as expanded name".format(substGroup)))
    }
  }

  /**
   * Returns the value of the 'abstract' attribute, if any, wrapped in an Option.
   */
  final def abstractOption: Option[Boolean] = {
    try {
      (elem \@ AbstractEName) map (_.toBoolean)
    } catch {
      case e: Exception => None
    }
  }

  /**
   * Returns the value of the 'nillable' attribute, if any, wrapped in an Option.
   */
  final def nillableOption: Option[Boolean] = {
    try {
      (elem \@ NillableEName) map (_.toBoolean)
    } catch {
      case e: Exception => None
    }
  }

  /**
   * Returns the value of the 'ref' attribute as expanded name, if any, wrapped in an Option.
   */
  final def refOption: Option[EName] = {
    val refOption = elem \@ RefEName
    refOption map { ref =>
      elem.scope.resolveQNameOption(QName(ref)).getOrElse(
        sys.error("Could not resolve ref '%s' as expanded name".format(ref)))
    }
  }
}

/**
 * Element declaration. An element declaration is either a global or local element declaration.
 */
abstract class ElementDeclaration private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends ElementDeclarationOrReference(indexedElem, childElems, docUri) {

  require(!isReference, "Must not be a reference")
}

/**
 * Global element declaration.
 */
final class GlobalElementDeclaration private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends ElementDeclaration(indexedElem, childElems, docUri) {

  require(isGlobal, "Must be global")

  /**
   * Returns the target namespace, if any, wrapped in an Option. The target namespace of a global component is the target namespace
   * of the schema root element, if any.
   */
  final override def targetNamespaceOption: Option[String] = indexedElem.rootElem.attributeOption(TargetNamespaceEName)

  /**
   * Returns None as the non-existent scope, wrapped in an Option.
   */
  final override def scopeOption: Option[indexed.Elem] = None

  /**
   * Returns `enameOption.get`.
   */
  final def ename: EName = enameOption.getOrElse(sys.error("Global element declarations must have a name"))

  final def typeOption: Option[EName] = elem.attributeAsResolvedQNameOption(TypeEName)
}

/**
 * Local element declaration.
 */
final class LocalElementDeclaration private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends ElementDeclaration(indexedElem, childElems, docUri) with Particle {

  require(!isGlobal, "Must be local")

  /**
   * Returns the target namespace, if any, wrapped in an Option. The target namespace depends on the target namespace
   * of the schema root element, if any, and on the form and (schema root element) elementFormDefault attributes, if any.
   */
  final override def targetNamespaceOption: Option[String] = {
    val tnsOption = indexedElem.rootElem.attributeOption(TargetNamespaceEName)

    val formOption = elem.attributeOption(FormEName)
    val elementFormDefaultOption = indexedElem.rootElem.attributeOption(ElementFormDefaultEName)

    if (formOption == Some("qualified")) tnsOption
    else if (formOption.isEmpty && (elementFormDefaultOption == Some("qualified"))) tnsOption
    else None
  }

  /**
   * Returns the "scope", as a complex type definition, wrapped in an Option.
   */
  final override def scopeOption: Option[indexed.Elem] = {
    val complexTypeOption = indexedElem.elemPath findAncestorPath { p =>
      p.elementNameOption.getOrElse(XsSchemaEName) == XsComplexTypeEName
    } map { p => indexed.Elem(indexedElem.rootElem).findElem(e => e.elemPath == p).get }
    complexTypeOption
  }

  /**
   * Returns `enameOption.get`.
   */
  final def ename: EName = enameOption.getOrElse(sys.error("Local element declarations must have a name"))
}

/**
 * Element reference. Strictly it is not an element declaration, but it can be considered an element declaration in that
 * it is represented by the same xs:element XML element.
 */
final class ElementReference private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends ElementDeclarationOrReference(indexedElem, childElems, docUri) with Particle {

  require(isReference, "Must be a reference")
  require(!isGlobal, "Must not be global")

  /**
   * Returns None as the optional target namespace of the (unresolved) element reference.
   */
  final override def targetNamespaceOption: Option[String] = None

  /**
   * Returns None as the optional scope of the (unresolved) element reference.
   */
  final override def scopeOption: Option[indexed.Elem] = None

  /**
   * Returns the value of the 'ref' attribute as expanded name.
   */
  final def ref: EName = refOption.getOrElse(sys.error("Element references must have a ref attribute"))
}

/**
 * Attribute use. The correspondence between attribute use and attribute declarations is analogous to the one between
 * particles and (for example) element declarations.
 */
trait AttributeUse extends SchemaComponent {

  // TODO
}

/**
 * Attribute declaration or attribute reference. That is, the "xs:attribute" XML element.
 */
abstract class AttributeDeclarationOrReference private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaComponent(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsAttributeEName, "The element must be an 'attribute' element")

  /**
   * Returns true if and only if the attribute declaration has the schema element as its parent.
   *
   * Attribute references are not considered global, because their parent is not the schema element, but they do always
   * refer to global attribute declarations.
   */
  final def isGlobal: Boolean = indexedElem.elemPath.entries.size == 1

  /**
   * Returns true if and only if the attribute declaration is a reference to another (global) attribute declaration.
   * Top level attribute declarations are never references.
   */
  final def isReference: Boolean = refOption.isDefined

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
  final def nameAttributeOption: Option[String] = elem \@ NameEName

  /**
   * Returns the "scope", if any, wrapped in an Option.
   *
   * That is, if this attribute declaration is not a reference, and has a complex type definition as ancestor, that complex
   * type definition is returned as indexed.Elem, wrapped in an Option. In all other cases, None is returned.
   */
  def scopeOption: Option[indexed.Elem]

  /**
   * Returns the value of the 'type' attribute as expanded name, if any, wrapped in an Option.
   */
  final def typeAttributeOption: Option[EName] = {
    val typeAttrAttrOption = elem \@ TypeEName
    typeAttrAttrOption map { tpe =>
      elem.scope.resolveQNameOption(QName(tpe)).getOrElse(
        sys.error("Could not resolve type '%s' as expanded name".format(tpe)))
    }
  }

  /**
   * Returns the value of the 'ref' attribute as expanded name, if any, wrapped in an Option.
   */
  final def refOption: Option[EName] = {
    val refOption = elem \@ RefEName
    refOption map { ref =>
      elem.scope.resolveQNameOption(QName(ref)).getOrElse(
        sys.error("Could not resolve ref '%s' as expanded name".format(ref)))
    }
  }
}

/**
 * Attribute declaration. An attribute declaration is either a global or local attribute declaration.
 */
abstract class AttributeDeclaration private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends AttributeDeclarationOrReference(indexedElem, childElems, docUri) {

  require(!isReference, "Must not be a reference")
}

/**
 * Global attribute declaration.
 */
final class GlobalAttributeDeclaration private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends AttributeDeclaration(indexedElem, childElems, docUri) {

  require(isGlobal, "Must be global")

  /**
   * Returns the target namespace, if any, wrapped in an Option. The target namespace of a global component is the target namespace
   * of the schema root element, if any.
   */
  final override def targetNamespaceOption: Option[String] = indexedElem.rootElem.attributeOption(TargetNamespaceEName)

  /**
   * Returns None as the non-existent scope, wrapped in an Option.
   */
  final override def scopeOption: Option[indexed.Elem] = None

  /**
   * Returns `enameOption.get`.
   */
  final def ename: EName = enameOption.getOrElse(sys.error("Global attribute declarations must have a name"))
}

/**
 * Local attribute declaration.
 */
final class LocalAttributeDeclaration private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends AttributeDeclaration(indexedElem, childElems, docUri) with AttributeUse {

  require(!isGlobal, "Must be local")

  /**
   * Returns the target namespace, if any, wrapped in an Option. The target namespace depends on the target namespace
   * of the schema root element, if any, and on the form and (schema root element) attributeFormDefault attributes, if any.
   */
  final override def targetNamespaceOption: Option[String] = {
    val tnsOption = indexedElem.rootElem.attributeOption(TargetNamespaceEName)

    val formOption = elem.attributeOption(FormEName)
    val attributeFormDefaultOption = indexedElem.rootElem.attributeOption(AttributeFormDefaultEName)

    if (formOption == Some("qualified")) tnsOption
    else if (formOption.isEmpty && (attributeFormDefaultOption == Some("qualified"))) tnsOption
    else None
  }

  /**
   * Returns the "scope", as a complex type definition, wrapped in an Option.
   */
  final override def scopeOption: Option[indexed.Elem] = {
    val complexTypeOption = indexedElem.elemPath findAncestorPath { p =>
      p.elementNameOption.getOrElse(XsSchemaEName) == XsComplexTypeEName
    } map { p => indexed.Elem(indexedElem.rootElem).findElem(e => e.elemPath == p).get }
    complexTypeOption
  }

  /**
   * Returns `enameOption.get`.
   */
  final def ename: EName = enameOption.getOrElse(sys.error("Local attribute declarations must have a name"))
}

/**
 * Attribute reference. Strictly it is not an attribute declaration, but it can be considered an attribute declaration in that
 * it is represented by the same xs:attribute XML element.
 */
final class AttributeReference private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends AttributeDeclarationOrReference(indexedElem, childElems, docUri) with AttributeUse {

  require(isReference, "Must be a reference")
  require(!isGlobal, "Must not be global")

  /**
   * Returns None as the optional target namespace of the (unresolved) attribute reference.
   */
  final override def targetNamespaceOption: Option[String] = None

  /**
   * Returns None as the optional scope of the (unresolved) attribute reference.
   */
  final override def scopeOption: Option[indexed.Elem] = None

  /**
   * Returns the value of the 'ref' attribute as expanded name.
   */
  final def ref: EName = refOption.getOrElse(sys.error("Attribute references must have a ref attribute"))
}

/**
 * Schema type definition, which is either a simple type or a complex type.
 */
abstract class TypeDefinition private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaComponent(indexedElem, childElems, docUri) {

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
  final def nameAttributeOption: Option[String] = elem \@ NameEName
}

/**
 * Simple type definition. That is, the "xs:simpleType" XML element.
 */
final class SimpleTypeDefinition private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends TypeDefinition(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsSimpleTypeEName, "The element must be an 'simpleType' element")

  final override def targetNamespaceOption: Option[String] = {
    indexedElem.rootElem.attributeOption(TargetNamespaceEName)
  }
}

/**
 * Complex type definition. That is, the "xs:complexType" XML element.
 */
final class ComplexTypeDefinition private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends TypeDefinition(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsComplexTypeEName, "The element must be an 'complexType' element")

  final override def targetNamespaceOption: Option[String] = {
    indexedElem.rootElem.attributeOption(TargetNamespaceEName)
  }
}

/**
 * Attribute group definition. That is, the "xs:attributeGroup" XML element.
 */
final class AttributeGroupDefinition private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaComponent(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsAttributeGroupEName, "The element must be an 'attributeGroup' element")

  final override def targetNamespaceOption: Option[String] = {
    indexedElem.rootElem.attributeOption(TargetNamespaceEName)
  }
}

/**
 * Identity constraint definition. That is, the "xs:key", "xs:keyref" or "xs:unique" XML element.
 */
final class IdentityConstraintDefinition private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaComponent(indexedElem, childElems, docUri) {

  require(
    Set(XsKeyEName, XsKeyrefEName, XsUniqueEName).contains(elem.resolvedName),
    "The element must be an 'key', 'keyref' or 'unique' element")

  final override def targetNamespaceOption: Option[String] = {
    indexedElem.rootElem.attributeOption(TargetNamespaceEName)
  }
}

/**
 * Model group definition. That is, the "xs:group" XML element.
 */
sealed class ModelGroupDefinition private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaComponent(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsGroupEName, "The element must be a 'group' element")

  final override def targetNamespaceOption: Option[String] = {
    indexedElem.rootElem.attributeOption(TargetNamespaceEName)
  }

  /**
   * Returns the value of the 'ref' attribute as expanded name, if any, wrapped in an Option.
   */
  final def refOption: Option[EName] = {
    val refOption = elem \@ RefEName
    refOption map { ref =>
      elem.scope.resolveQNameOption(QName(ref)).getOrElse(
        sys.error("Could not resolve ref '%s' as expanded name".format(ref)))
    }
  }
}

/**
 * Notation declaration. That is, the "xs:notation" XML element.
 */
final class NotationDeclaration private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaComponent(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsNotationEName, "The element must be a 'notation' element")

  final override def targetNamespaceOption: Option[String] = {
    indexedElem.rootElem.attributeOption(TargetNamespaceEName)
  }
}

/**
 * Model group. That is, the "xs:all", "xs:sequence" or "xs:choice" XML element.
 */
sealed class ModelGroup private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaComponent(indexedElem, childElems, docUri) {

  require(
    Set(XsAllEName, XsSequenceEName, XsChoiceEName).contains(elem.resolvedName),
    "The element must be an 'all', 'sequence' or 'choice' element")

  final override def targetNamespaceOption: Option[String] = None

  final def inNamedGroup: Boolean = {
    assert(indexedElem.elemPath.parentPathOption.isDefined)
    val parentPath = indexedElem.elemPath.parentPath

    val parent = indexedElem.rootElem.getWithElemPath(parentPath)
    (parent.resolvedName == XsGroupEName) && ((parent \@ NameEName).isDefined)
  }
}

/**
 * Wildcard. That is, the "xs:any" or "xs:anyAttribute" XML element.
 */
sealed class Wildcard private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaComponent(indexedElem, childElems, docUri) {

  require(
    Set(XsAnyEName, XsAnyAttributeEName).contains(elem.resolvedName),
    "The element must be an 'any', 'anyAttribute' element")

  final override def targetNamespaceOption: Option[String] = None
}

/**
 * Annotation schema component. That is, the "xs:annotation" XML element.
 */
final class Annotation private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaComponent(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsAnnotationEName, "The element must be an 'annotation' element")

  final override def targetNamespaceOption: Option[String] = None
}

// Import, include, redefine

/**
 * The "xs:import" XML element.
 */
final class Import private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaElem(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsImportEName, "The element must be an 'import' element")
}

/**
 * The "xs:include" XML element.
 */
final class Include private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaElem(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsIncludeEName, "The element must be an 'include' element")
}

/**
 * The "xs:redefine" XML element.
 */
final class Redefine private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaElem(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsRedefineEName, "The element must be a 'redefine' element")
}

// Other schema parts, that are not Schema Components themselves, such as extension, restriction, etc.

/**
 * The "xs:complexContent" XML element.
 */
final class ComplexContent private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaElem(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsComplexContentEName, "The element must be a 'complexContent' element")
}

/**
 * The "xs:simpleContent" XML element.
 */
final class SimpleContent private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaElem(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsSimpleContentEName, "The element must be a 'simpleContent' element")
}

/**
 * The "xs:extension" XML element.
 */
final class Extension private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaElem(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsExtensionEName, "The element must be an 'extension' element")
}

/**
 * The "xs:restriction" XML element.
 */
final class Restriction private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaElem(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsRestrictionEname, "The element must be a 'restriction' element")
}

/**
 * The "xs:appinfo" XML element.
 */
final class Appinfo private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaElem(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsAppinfoEName, "The element must be an 'appinfo' element")
}

/**
 * The "xs:documentation" XML element.
 */
final class Documentation private[schema] (
  indexedElem: indexed.Elem,
  childElems: immutable.IndexedSeq[SchemaElem],
  docUri: URI) extends SchemaElem(indexedElem, childElems, docUri) {

  require(elem.resolvedName == XsDocumentationEName, "The element must be a 'documentation' element")
}

// Companion objects

object SchemaRootElem {

  /**
   * Public factory method for SchemaRootElem instances. It returns `SchemaElem(indexedElem, docUri)` as a `SchemaRootElem`.
   *
   * This is an expensive method, but once a `SchemaRootElem` has been created, querying through the `ElemLike` API is very fast.
   */
  def apply(indexedElem: indexed.Elem, docUri: URI): SchemaRootElem = {
    require(indexedElem.elemPath.isRoot)

    val childElems = indexedElem.findAllChildElems.map(e => SchemaElem.apply(e, docUri))
    new SchemaRootElem(indexedElem, childElems, docUri)
  }
}

object SchemaElem {

  /**
   * Recursive public factory method for SchemaElem instances.
   */
  def apply(indexedElem: indexed.Elem, docUri: URI): SchemaElem = {
    // Recursive calls
    val childElems = indexedElem.findAllChildElems.map(e => SchemaElem.apply(e, docUri))

    indexedElem.elemPath.elementNameOption.getOrElse(XsSchemaEName) match {
      case XsSchemaEName => new SchemaRootElem(indexedElem, childElems, docUri)
      case XsElementEName if indexedElem.attributeOption(RefEName).isDefined =>
        new ElementReference(indexedElem, childElems, docUri)
      case XsElementEName if indexedElem.elemPath.entries.size == 1 =>
        new GlobalElementDeclaration(indexedElem, childElems, docUri)
      case XsElementEName => new LocalElementDeclaration(indexedElem, childElems, docUri)
      case XsAttributeEName if indexedElem.attributeOption(RefEName).isDefined =>
        new AttributeReference(indexedElem, childElems, docUri)
      case XsAttributeEName if indexedElem.elemPath.entries.size == 1 =>
        new GlobalAttributeDeclaration(indexedElem, childElems, docUri)
      case XsAttributeEName => new LocalAttributeDeclaration(indexedElem, childElems, docUri)
      case XsSimpleTypeEName => new SimpleTypeDefinition(indexedElem, childElems, docUri)
      case XsComplexTypeEName => new ComplexTypeDefinition(indexedElem, childElems, docUri)
      case XsAttributeGroupEName => new AttributeGroupDefinition(indexedElem, childElems, docUri)
      case XsKeyEName => new IdentityConstraintDefinition(indexedElem, childElems, docUri)
      case XsKeyrefEName => new IdentityConstraintDefinition(indexedElem, childElems, docUri)
      case XsUniqueEName => new IdentityConstraintDefinition(indexedElem, childElems, docUri)
      case XsGroupEName if (indexedElem \@ RefEName).isDefined =>
        new ModelGroupDefinition(indexedElem, childElems, docUri) with Particle
      case XsGroupEName => new ModelGroupDefinition(indexedElem, childElems, docUri)
      case XsAllEName if inNamedGroup(indexedElem) =>
        new ModelGroup(indexedElem, childElems, docUri)
      case XsSequenceEName if inNamedGroup(indexedElem) =>
        new ModelGroup(indexedElem, childElems, docUri)
      case XsChoiceEName if inNamedGroup(indexedElem) =>
        new ModelGroup(indexedElem, childElems, docUri)
      case XsAllEName => new ModelGroup(indexedElem, childElems, docUri) with Particle
      case XsSequenceEName => new ModelGroup(indexedElem, childElems, docUri) with Particle
      case XsChoiceEName => new ModelGroup(indexedElem, childElems, docUri) with Particle
      case XsNotationEName => new NotationDeclaration(indexedElem, childElems, docUri)
      case XsAnnotationEName => new Annotation(indexedElem, childElems, docUri)
      case XsAnyEName => new Wildcard(indexedElem, childElems, docUri) with Particle
      case XsAnyAttributeEName => new Wildcard(indexedElem, childElems, docUri)
      case XsImportEName => new Import(indexedElem, childElems, docUri)
      case XsIncludeEName => new Include(indexedElem, childElems, docUri)
      case XsRedefineEName => new Redefine(indexedElem, childElems, docUri)
      case XsComplexContentEName => new ComplexContent(indexedElem, childElems, docUri)
      case XsSimpleContentEName => new SimpleContent(indexedElem, childElems, docUri)
      case XsAppinfoEName => new Appinfo(indexedElem, childElems, docUri)
      case XsDocumentationEName => new Documentation(indexedElem, childElems, docUri)
      case XsExtensionEName => new Extension(indexedElem, childElems, docUri)
      case XsRestrictionEname => new Restriction(indexedElem, childElems, docUri)
      case _ => new SchemaElem(indexedElem, childElems, docUri)
    }
  }

  private def inNamedGroup(indexedElem: indexed.Elem): Boolean = {
    assert(indexedElem.elemPath.parentPathOption.isDefined)
    val parent = indexedElem.rootElem.getWithElemPath(indexedElem.elemPath.parentPath)

    (parent.resolvedName == XsGroupEName) && ((parent \@ NameEName).isDefined)
  }
}

object Particle {

  val Unbounded = -1
}
