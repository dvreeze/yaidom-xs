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
import scala.reflect.classTag
import eu.cdevreeze.yaidom._
import eu.cdevreeze.yaidom.subtypeaware.SubtypeAwareParentElemLike
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
 * The ancestry of an element is kept by keeping an `docawware.Elem` as state. Redundantly, the `childElems` are also kept as
 * state, to make `ElemLike` API querying very fast, with little object creation overhead. The `SchemaElem` implementation
 * makes sure that the following indeed holds:
 * {{{
 * childElems.map(_.indexedElem) == indexedElem.findAllChildElems
 * }}}
 *
 * @author Chris de Vreeze
 */
sealed class SchemaElem private[schema] (
  val indexedElem: docaware.Elem,
  val childElems: immutable.IndexedSeq[SchemaElem]) extends ElemLike[SchemaElem] with SubtypeAwareParentElemLike[SchemaElem] with HasText with Immutable {

  /**
   * The yaidom Elem itself, stored as a val
   */
  final val elem: Elem = indexedElem.elem

  require(
    childElems.map(_.indexedElem) == indexedElem.findAllChildElems,
    "Corrupt element!")

  require(indexedElem.rootElem.resolvedName == XsSchemaEName, "The root of the element tree must be a 'schema' element")
  require(
    (elem.resolvedName == XsSchemaEName) || (!indexedElem.path.isRoot),
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

  final def docUri: URI = indexedElem.docUri

  final override def equals(obj: Any): Boolean = obj match {
    case other: SchemaElem =>
      (other.indexedElem == this.indexedElem) && (other.docUri == this.docUri)
    case _ => false
  }

  final override def hashCode: Int = (indexedElem, docUri).hashCode

  final override def text: String = elem.text

  final override def toString: String = elem.toString

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
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(elem.resolvedName == XsSchemaEName, "The element must be a 'schema' element")
  require(indexedElem.path.isRoot, "The element must be the root of the element tree")

  final def targetNamespaceOption: Option[String] = elem \@ TargetNamespaceEName

  /**
   * Returns all global element declarations.
   */
  final def findAllGlobalElementDeclarations: immutable.IndexedSeq[GlobalElementDeclaration] =
    findAllChildElemsOfType(classTag[GlobalElementDeclaration])

  /**
   * Returns all global element declarations obeying the given predicate.
   */
  final def filterGlobalElementDeclarations(p: GlobalElementDeclaration => Boolean): immutable.IndexedSeq[GlobalElementDeclaration] =
    filterChildElemsOfType(classTag[GlobalElementDeclaration])(p)

  /**
   * Returns all global attribute declarations.
   */
  final def findAllGlobalAttributeDeclarations: immutable.IndexedSeq[GlobalAttributeDeclaration] =
    findAllChildElemsOfType(classTag[GlobalAttributeDeclaration])

  /**
   * Returns all global attribute declarations obeying the given predicate.
   */
  final def filterGlobalAttributeDeclarations(p: GlobalAttributeDeclaration => Boolean): immutable.IndexedSeq[GlobalAttributeDeclaration] =
    filterChildElemsOfType(classTag[GlobalAttributeDeclaration])(p)

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
    findAllChildElemsOfType(classTag[Import])

  /**
   * Returns all includes.
   */
  final def findAllIncludes: immutable.IndexedSeq[Include] =
    findAllChildElemsOfType(classTag[Include])

  /**
   * Returns all redefines.
   */
  final def findAllRedefines: immutable.IndexedSeq[Redefine] =
    findAllChildElemsOfType(classTag[Redefine])
}

// Schema Components

/**
 * Particle, having a min and max occurs (possibly default).
 */
trait Particle extends SchemaElem {

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
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(elem.resolvedName == XsElementEName, "The element must be an 'element' element")
}

/**
 * Element declaration. An element declaration is either a global or local element declaration.
 */
abstract class ElementDeclaration private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends ElementDeclarationOrReference(indexedElem, childElems) with HasName {

  require((elem \@ RefEName).isEmpty, "Must not be a reference")

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
   * Returns the value of the 'nillable' attribute, if any, wrapped in an Option.
   */
  final def nillableOption: Option[Boolean] = {
    try {
      (elem \@ NillableEName) map (_.toBoolean)
    } catch {
      case e: Exception => None
    }
  }
}

/**
 * Global element declaration.
 */
final class GlobalElementDeclaration private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends ElementDeclaration(indexedElem, childElems) with CanBeAbstract {

  require(indexedElem.path.entries.size == 1, "Must be global")

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
}

/**
 * Local element declaration.
 */
final class LocalElementDeclaration private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends ElementDeclaration(indexedElem, childElems) with Particle {

  require(indexedElem.path.entries.size >= 2, "Must be local")
}

/**
 * Element reference. Strictly it is not an element declaration, but it can be considered an element declaration in that
 * it is represented by the same xs:element XML element.
 */
final class ElementReference private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends ElementDeclarationOrReference(indexedElem, childElems) with Particle {

  require((elem \@ RefEName).isDefined, "Must be a reference")
  require(indexedElem.path.entries.size >= 2, "Must not be global")

  /**
   * Returns the value of the 'ref' attribute as expanded name.
   */
  final def ref: EName =
    elem.attributeAsResolvedQNameOption(RefEName).getOrElse(sys.error("Element references must have a ref attribute"))
}

/**
 * Attribute declaration or attribute reference. That is, the "xs:attribute" XML element.
 */
abstract class AttributeDeclarationOrReference private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(elem.resolvedName == XsAttributeEName, "The element must be an 'attribute' element")
}

/**
 * Attribute declaration. An attribute declaration is either a global or local attribute declaration.
 */
abstract class AttributeDeclaration private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends AttributeDeclarationOrReference(indexedElem, childElems) with HasName {

  require((elem \@ RefEName).isEmpty, "Must not be a reference")

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
}

/**
 * Global attribute declaration.
 */
final class GlobalAttributeDeclaration private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends AttributeDeclaration(indexedElem, childElems) {

  require(indexedElem.path.entries.size == 1, "Must be global")
}

/**
 * Local attribute declaration.
 */
final class LocalAttributeDeclaration private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends AttributeDeclaration(indexedElem, childElems) {

  require(indexedElem.path.entries.size >= 2, "Must be local")
}

/**
 * Attribute reference. Strictly it is not an attribute declaration, but it can be considered an attribute declaration in that
 * it is represented by the same xs:attribute XML element.
 */
final class AttributeReference private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends AttributeDeclarationOrReference(indexedElem, childElems) {

  require((elem \@ RefEName).isDefined, "Must be a reference")
  require(indexedElem.path.entries.size >= 2, "Must not be global")

  /**
   * Returns the value of the 'ref' attribute as expanded name.
   */
  final def ref: EName =
    elem.attributeAsResolvedQNameOption(RefEName).getOrElse(sys.error("Attribute references must have a ref attribute"))
}

/**
 * Schema type definition, which is either a simple type or a complex type.
 */
abstract class TypeDefinition private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  /**
   * Returns the `EName` by combining the target namespace (of the schema root element) and the value of the "name" attribute,
   * if any, wrapped in an Option.
   */
  final def enameOption: Option[EName] = {
    val tnsOption = indexedElem.rootElem \@ TargetNamespaceEName
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
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends TypeDefinition(indexedElem, childElems) {

  require(elem.resolvedName == XsSimpleTypeEName, "The element must be an 'simpleType' element")
}

/**
 * Complex type definition. That is, the "xs:complexType" XML element.
 */
final class ComplexTypeDefinition private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends TypeDefinition(indexedElem, childElems) {

  require(elem.resolvedName == XsComplexTypeEName, "The element must be an 'complexType' element")
}

/**
 * Attribute group definition. That is, the "xs:attributeGroup" XML element.
 */
final class AttributeGroupDefinition private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(elem.resolvedName == XsAttributeGroupEName, "The element must be an 'attributeGroup' element")
}

/**
 * Identity constraint definition. That is, the "xs:key", "xs:keyref" or "xs:unique" XML element.
 */
abstract class IdentityConstraintDefinition private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(
    Set(XsKeyEName, XsKeyrefEName, XsUniqueEName).contains(elem.resolvedName),
    "The element must be an 'key', 'keyref' or 'unique' element")
}

/**
 * Identity constraint definition "xs:key".
 */
final class KeyConstraint private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends IdentityConstraintDefinition(indexedElem, childElems) {

  require(elem.resolvedName == XsKeyEName, "The element must be a 'key' element")
}

/**
 * Identity constraint definition "xs:keyref".
 */
final class KeyrefConstraint private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends IdentityConstraintDefinition(indexedElem, childElems) {

  require(elem.resolvedName == XsKeyrefEName, "The element must be a 'keyref' element")
}

/**
 * Identity constraint definition "xs:unique".
 */
final class UniqueConstraint private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends IdentityConstraintDefinition(indexedElem, childElems) {

  require(elem.resolvedName == XsUniqueEName, "The element must be a 'unique' element")
}

/**
 * Model group definition. That is, the "xs:group" XML element introducing a named model group.
 */
final class ModelGroupDefinition private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(elem.resolvedName == XsGroupEName, "The element must be a 'group' element")
  require((elem \@ RefEName).isEmpty, "The element must have no 'ref' attribute")
}

/**
 * Model group reference. That is, the "xs:group" XML element referring to a named model group.
 */
final class ModelGroupReference private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) with Particle {

  require(elem.resolvedName == XsGroupEName, "The element must be a 'group' element")
  require((elem \@ RefEName).isDefined, "The element must have a 'ref' attribute")

  /**
   * Returns the value of the 'ref' attribute as expanded name.
   */
  final def ref: EName = {
    elem.attributeAsResolvedQName(RefEName)
  }
}

/**
 * Notation declaration. That is, the "xs:notation" XML element.
 */
final class NotationDeclaration private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(elem.resolvedName == XsNotationEName, "The element must be a 'notation' element")
}

/**
 * Model group. That is, the "xs:all", "xs:sequence" or "xs:choice" XML element.
 */
abstract class ModelGroup private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) with Particle {

  require(
    Set(XsAllEName, XsSequenceEName, XsChoiceEName).contains(elem.resolvedName),
    "The element must be an 'all', 'sequence' or 'choice' element")
  require(
    !inNamedGroup || (minOccursAttrOption.isEmpty && maxOccursAttrOption.isEmpty),
    "If in a named group, there must be no @minOccurs and @maxOccurs")

  final def inNamedGroup: Boolean = {
    assert(indexedElem.path.parentPathOption.isDefined)
    val parentPath = indexedElem.path.parentPath

    val parent = indexedElem.rootElem.getElemOrSelfByPath(parentPath)
    (parent.resolvedName == XsGroupEName) && ((parent \@ NameEName).isDefined)
  }
}

/**
 * Model group "all".
 */
final class AllGroup private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends ModelGroup(indexedElem, childElems) {

  require(elem.resolvedName == XsAllEName, "The element must be an 'all' element")
}

/**
 * Model group "choice".
 */
final class ChoiceGroup private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends ModelGroup(indexedElem, childElems) {

  require(elem.resolvedName == XsChoiceEName, "The element must be a 'choice' element")
}

/**
 * Model group "sequence".
 */
final class SequenceGroup private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends ModelGroup(indexedElem, childElems) {

  require(elem.resolvedName == XsSequenceEName, "The element must be a 'sequence' element")
}

/**
 * Wildcard "xs:any".
 */
final class AnyWildcard private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) with Particle {

  require(elem.resolvedName == XsAnyEName, "The element must be an 'any' element")
}

/**
 * Wildcard "xs:anyAttribute".
 */
final class AnyAttributeWildcard private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(elem.resolvedName == XsAnyAttributeEName, "The element must be an 'anyAttribute' element")
}

/**
 * Annotation schema component. That is, the "xs:annotation" XML element.
 */
final class Annotation private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(elem.resolvedName == XsAnnotationEName, "The element must be an 'annotation' element")
}

// Import, include, redefine

/**
 * The "xs:import" XML element.
 */
final class Import private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(elem.resolvedName == XsImportEName, "The element must be an 'import' element")
}

/**
 * The "xs:include" XML element.
 */
final class Include private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(elem.resolvedName == XsIncludeEName, "The element must be an 'include' element")
}

/**
 * The "xs:redefine" XML element.
 */
final class Redefine private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(elem.resolvedName == XsRedefineEName, "The element must be a 'redefine' element")
}

// Other schema parts, that are not Schema Components themselves, such as extension, restriction, etc.

/**
 * The "xs:complexContent" XML element.
 */
final class ComplexContent private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(elem.resolvedName == XsComplexContentEName, "The element must be a 'complexContent' element")
}

/**
 * The "xs:simpleContent" XML element.
 */
final class SimpleContent private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(elem.resolvedName == XsSimpleContentEName, "The element must be a 'simpleContent' element")
}

/**
 * The "xs:extension" XML element.
 */
final class Extension private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(elem.resolvedName == XsExtensionEName, "The element must be an 'extension' element")
}

/**
 * The "xs:restriction" XML element.
 */
final class Restriction private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(elem.resolvedName == XsRestrictionEname, "The element must be a 'restriction' element")
}

/**
 * The "xs:appinfo" XML element.
 */
final class Appinfo private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(elem.resolvedName == XsAppinfoEName, "The element must be an 'appinfo' element")
}

/**
 * The "xs:documentation" XML element.
 */
final class Documentation private[schema] (
  indexedElem: docaware.Elem,
  childElems: immutable.IndexedSeq[SchemaElem]) extends SchemaElem(indexedElem, childElems) {

  require(elem.resolvedName == XsDocumentationEName, "The element must be a 'documentation' element")
}

// Capabilities

trait CanBeAbstract { self: SchemaElem =>

  /**
   * Returns true if and only if the element declaration is abstract.
   * Only global element declarations can be abstract.
   */
  final def isAbstract: Boolean = abstractOption(elem) == Some(true)

  final def abstractOption(elem: Elem): Option[Boolean] = {
    try {
      (elem \@ AbstractEName) map (_.toBoolean)
    } catch {
      case e: Exception => None
    }
  }
}

trait HasName { self: SchemaElem =>

  /**
   * Returns the `EName` by combining the target namespace (of the schema root element) and the value of the "name" attribute
   */
  final def ename: EName = {
    val tnsOption = indexedElem.rootElem \@ TargetNamespaceEName
    EName(tnsOption, nameAttribute)
  }

  /**
   * Returns the value of the "name" attribute
   */
  final def nameAttribute: String = (elem \@ NameEName).getOrElse(sys.error("Expected @name"))
}

// Companion objects

object SchemaRootElem {

  /**
   * Public factory method for SchemaRootElem instances. It returns `SchemaElem(indexedElem, docUri)` as a `SchemaRootElem`.
   *
   * This is an expensive method, but once a `SchemaRootElem` has been created, querying through the `ElemLike` API is very fast.
   */
  def apply(indexedElem: docaware.Elem): SchemaRootElem = {
    require(indexedElem.path.isRoot)

    val childElems = indexedElem.findAllChildElems.map(e => SchemaElem.apply(e))
    new SchemaRootElem(indexedElem, childElems)
  }
}

object SchemaElem {

  /**
   * Recursive public factory method for SchemaElem instances.
   */
  def apply(indexedElem: docaware.Elem): SchemaElem = {
    // Recursive calls
    val childElems = indexedElem.findAllChildElems.map(e => SchemaElem.apply(e))

    indexedElem.path.elementNameOption.getOrElse(XsSchemaEName) match {
      case XsSchemaEName => new SchemaRootElem(indexedElem, childElems)
      case XsElementEName if indexedElem.attributeOption(RefEName).isDefined =>
        new ElementReference(indexedElem, childElems)
      case XsElementEName if indexedElem.path.entries.size == 1 =>
        new GlobalElementDeclaration(indexedElem, childElems)
      case XsElementEName => new LocalElementDeclaration(indexedElem, childElems)
      case XsAttributeEName if indexedElem.attributeOption(RefEName).isDefined =>
        new AttributeReference(indexedElem, childElems)
      case XsAttributeEName if indexedElem.path.entries.size == 1 =>
        new GlobalAttributeDeclaration(indexedElem, childElems)
      case XsAttributeEName => new LocalAttributeDeclaration(indexedElem, childElems)
      case XsSimpleTypeEName => new SimpleTypeDefinition(indexedElem, childElems)
      case XsComplexTypeEName => new ComplexTypeDefinition(indexedElem, childElems)
      case XsAttributeGroupEName => new AttributeGroupDefinition(indexedElem, childElems)
      case XsKeyEName => new KeyConstraint(indexedElem, childElems)
      case XsKeyrefEName => new KeyrefConstraint(indexedElem, childElems)
      case XsUniqueEName => new UniqueConstraint(indexedElem, childElems)
      case XsGroupEName if (indexedElem \@ RefEName).isDefined =>
        new ModelGroupReference(indexedElem, childElems)
      case XsGroupEName => new ModelGroupDefinition(indexedElem, childElems)
      case XsAllEName => new AllGroup(indexedElem, childElems)
      case XsSequenceEName => new SequenceGroup(indexedElem, childElems)
      case XsChoiceEName => new ChoiceGroup(indexedElem, childElems)
      case XsNotationEName => new NotationDeclaration(indexedElem, childElems)
      case XsAnnotationEName => new Annotation(indexedElem, childElems)
      case XsAnyEName => new AnyWildcard(indexedElem, childElems)
      case XsAnyAttributeEName => new AnyAttributeWildcard(indexedElem, childElems)
      case XsImportEName => new Import(indexedElem, childElems)
      case XsIncludeEName => new Include(indexedElem, childElems)
      case XsRedefineEName => new Redefine(indexedElem, childElems)
      case XsComplexContentEName => new ComplexContent(indexedElem, childElems)
      case XsSimpleContentEName => new SimpleContent(indexedElem, childElems)
      case XsAppinfoEName => new Appinfo(indexedElem, childElems)
      case XsDocumentationEName => new Documentation(indexedElem, childElems)
      case XsExtensionEName => new Extension(indexedElem, childElems)
      case XsRestrictionEname => new Restriction(indexedElem, childElems)
      case _ => new SchemaElem(indexedElem, childElems)
    }
  }
}

object Particle {

  val Unbounded = -1
}
