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

package eu.cdevreeze.yaidom.xs

import java.net.URI

import scala.collection.immutable
import scala.reflect.classTag

import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.core.Path
import eu.cdevreeze.yaidom.core.QName
import eu.cdevreeze.yaidom.core.Scope
import eu.cdevreeze.yaidom.queryapi.IsNavigable
import eu.cdevreeze.yaidom.queryapi.ScopedElemLike
import eu.cdevreeze.yaidom.queryapi.SubtypeAwareElemLike
import eu.cdevreeze.yaidom.bridge.IndexedBridgeElem
import XsdElem.CanBeAbstract
import XsdElem.HasName
import XsdElem.IsReference

/**
 * Immutable XML Schema or a part thereof. These elements offer the `ScopedElemLike` API, among other APIs.
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
 * The ancestry of an element is kept by keeping an `IndexedBridgeElem` as state. Redundantly, the `childElems` are also kept as
 * state, to make `ElemLike` API querying very fast, with little object creation overhead.
 *
 * TODO Mind xsi:nil.
 *
 * @author Chris de Vreeze
 */
sealed class XsdElem private[xs] (
  val bridgeElem: IndexedBridgeElem,
  val childElems: immutable.IndexedSeq[XsdElem]) extends ScopedElemLike[XsdElem] with SubtypeAwareElemLike[XsdElem] with Immutable {

  require(childElems.map(_.bridgeElem.backingElem) == bridgeElem.findAllChildElems.map(_.backingElem))

  // assert(bridgeElem.rootElem.resolvedName == XsSchemaEName, "The root of the element tree must be a 'schema' element")
  // assert(
  //   (resolvedName == XsSchemaEName) || (!bridgeElem.path.isRoot),
  //   "This element must either be a 'schema' element, or not be the root of the element tree")

  /**
   * Returns all child elements, in the correct order. That is, returns `childElems`.
   *
   * These child elements share the same rootElem (and doc URI) with this element, but differ in the element paths, which have
   * one more "path entry".
   *
   * The implementation is extremely fast, which is needed for fast querying through SchemaApi query methods.
   */
  final override def findAllChildElems: immutable.IndexedSeq[XsdElem] = childElems

  final override def resolvedName: EName = bridgeElem.resolvedName

  final override def resolvedAttributes: immutable.IndexedSeq[(EName, String)] =
    bridgeElem.resolvedAttributes.toVector

  final def qname: QName = bridgeElem.qname

  final def attributes: immutable.Iterable[(QName, String)] = bridgeElem.attributes

  final def scope: Scope = bridgeElem.scope

  final def text: String = bridgeElem.text

  final def baseUri: URI = bridgeElem.baseUri

  final override def equals(other: Any): Boolean = other match {
    case e: XsdElem => bridgeElem.backingElem == e.bridgeElem.backingElem
    case _ => false
  }

  final override def hashCode: Int = bridgeElem.backingElem.hashCode

  final override def toString: String = bridgeElem.toString

  final def idOption: Option[String] = attributeOption(IdEName)

  /**
   * Returns the optional URI of this element, containing the id attribute value as URI fragment, if any.
   * If the id attribute is absent, None is returned.
   */
  final def uriOption: Option[URI] =
    idOption.map(id => new URI(baseUri.getScheme, baseUri.getSchemeSpecificPart, id))
}

/**
 * XML Schema (from one document). That is, the "xs:schema" XML element.
 *
 * This is what the XML Schema specification calls a schema document, or the document element thereof.
 * In the abstract schema model of the specification, a schema is represented by one or more of these "schema documents".
 */
final class SchemaRootElem private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsSchemaEName, "The element must be a 'schema' element")
  assert(bridgeElem.path.isRoot, "The element must be the root of the element tree")

  final def targetNamespaceOption: Option[String] = attributeOption(TargetNamespaceEName)

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
trait Particle extends XsdElem {

  final def minOccurs: Int = minOccursAttrOption map (_.toInt) getOrElse 1

  final def maxOccurs: Int = {
    maxOccursAttrOption map { v =>
      if (v.toLowerCase(java.util.Locale.ENGLISH) == "unbounded") Particle.Unbounded else 1
    } getOrElse 1
  }

  final def minOccursAttrOption: Option[String] = attributeOption(MinOccursEName)

  final def maxOccursAttrOption: Option[String] = attributeOption(MaxOccursEName)
}

/**
 * Element declaration or element reference. That is, the "xs:element" XML element.
 */
abstract class ElementDeclarationOrReference private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsElementEName, "The element must be an 'element' element")
}

/**
 * Element declaration. An element declaration is either a global or local element declaration.
 */
abstract class ElementDeclaration private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ElementDeclarationOrReference(bridgeElem, childElems) with HasName {

  assert(attributeOption(RefEName).isEmpty, "Must not be a reference")
  assert(attributeOption(NameEName).isDefined, "Must have a name")

  /**
   * Returns the value of the 'type' attribute as expanded name, if any, wrapped in an Option.
   */
  final def typeAttributeOption: Option[EName] = {
    val typeAttrAttrOption = attributeOption(TypeEName)
    typeAttrAttrOption map { tpe =>
      scope.resolveQNameOption(QName(tpe)).getOrElse(
        sys.error("Could not resolve type '%s' as expanded name".format(tpe)))
    }
  }

  /**
   * Returns the value of the 'nillable' attribute, if any, wrapped in an Option.
   */
  final def nillableOption: Option[Boolean] = {
    try {
      attributeOption(NillableEName) map (_.toBoolean)
    } catch {
      case e: Exception => None
    }
  }
}

/**
 * Global element declaration.
 */
final class GlobalElementDeclaration private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ElementDeclaration(bridgeElem, childElems) with CanBeAbstract {

  assert(bridgeElem.path.entries.size == 1, "Must be global")

  def targetNamespaceOption: Option[String] = bridgeElem.rootElem.attributeOption(TargetNamespaceEName)

  /**
   * Returns the value of the 'substitutionGroup' attribute as expanded name, if any, wrapped in an Option.
   */
  final def substitutionGroupOption: Option[EName] = {
    val substGroupAttrOption = attributeOption(SubstitutionGroupEName)
    substGroupAttrOption map { substGroup =>
      scope.resolveQNameOption(QName(substGroup)).getOrElse(
        sys.error("Could not resolve substitution group '%s' as expanded name".format(substGroup)))
    }
  }
}

/**
 * Local element declaration.
 */
final class LocalElementDeclaration private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ElementDeclaration(bridgeElem, childElems) with Particle {

  assert(bridgeElem.path.entries.size >= 2, "Must be local")

  final def targetNamespaceOption: Option[String] = {
    val tnsOption = bridgeElem.rootElem \@ TargetNamespaceEName
    if (isQualified) tnsOption else None
  }

  private def isQualified: Boolean = isQualified(bridgeElem.path)

  private def isQualified(path: Path): Boolean = {
    if (path.isRoot) {
      bridgeElem.rootElem.attributeOption(ElementFormDefaultEName) map {
        case "qualified" => true
        case "unqualified" => false
      } getOrElse false
    } else {
      attributeOption(FormEName) map {
        case "qualified" => true
        case "unqualified" => false
      } getOrElse {
        // Recursive call
        isQualified(Path.Root)
      }
    }
  }
}

/**
 * Element reference. Strictly it is not an element declaration, but it can be considered an element declaration in that
 * it is represented by the same xs:element XML element.
 */
final class ElementReference private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ElementDeclarationOrReference(bridgeElem, childElems) with Particle with IsReference {

  assert(attributeOption(RefEName).isDefined, "Must be a reference")
  assert(bridgeElem.path.entries.size >= 2, "Must not be global")
}

/**
 * Attribute declaration or attribute reference. That is, the "xs:attribute" XML element.
 */
abstract class AttributeDeclarationOrReference private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsAttributeEName, "The element must be an 'attribute' element")
}

/**
 * Attribute declaration. An attribute declaration is either a global or local attribute declaration.
 */
abstract class AttributeDeclaration private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends AttributeDeclarationOrReference(bridgeElem, childElems) with HasName {

  assert(attributeOption(RefEName).isEmpty, "Must not be a reference")
  assert(attributeOption(NameEName).isDefined, "Must have a name")

  /**
   * Returns the value of the 'type' attribute as expanded name, if any, wrapped in an Option.
   */
  final def typeAttributeOption: Option[EName] = {
    val typeAttrAttrOption = attributeOption(TypeEName)
    typeAttrAttrOption map { tpe =>
      scope.resolveQNameOption(QName(tpe)).getOrElse(
        sys.error("Could not resolve type '%s' as expanded name".format(tpe)))
    }
  }
}

/**
 * Global attribute declaration.
 */
final class GlobalAttributeDeclaration private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends AttributeDeclaration(bridgeElem, childElems) {

  assert(bridgeElem.path.entries.size == 1, "Must be global")

  def targetNamespaceOption: Option[String] = bridgeElem.rootElem.attributeOption(TargetNamespaceEName)
}

/**
 * Local attribute declaration.
 */
final class LocalAttributeDeclaration private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends AttributeDeclaration(bridgeElem, childElems) {

  assert(bridgeElem.path.entries.size >= 2, "Must be local")

  final def targetNamespaceOption: Option[String] = {
    val tnsOption = bridgeElem.rootElem \@ TargetNamespaceEName
    if (isQualified) tnsOption else None
  }

  private def isQualified: Boolean = isQualified(bridgeElem.path)

  private def isQualified(path: Path): Boolean = {
    if (path.isRoot) {
      bridgeElem.rootElem.attributeOption(AttributeFormDefaultEName) map {
        case "qualified" => true
        case "unqualified" => false
      } getOrElse false
    } else {
      attributeOption(FormEName) map {
        case "qualified" => true
        case "unqualified" => false
      } getOrElse {
        // Recursive call
        isQualified(Path.Root)
      }
    }
  }
}

/**
 * Attribute reference. Strictly it is not an attribute declaration, but it can be considered an attribute declaration in that
 * it is represented by the same xs:attribute XML element.
 */
final class AttributeReference private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends AttributeDeclarationOrReference(bridgeElem, childElems) with IsReference {

  assert(attributeOption(RefEName).isDefined, "Must be a reference")
  assert(bridgeElem.path.entries.size >= 2, "Must not be global")
}

/**
 * Schema type definition, which is either a simple type or a complex type.
 */
abstract class TypeDefinition private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {
}

trait NamedTypeDefinition extends TypeDefinition with HasName

trait AnonymousTypeDefinition extends TypeDefinition

/**
 * Simple type definition. That is, the "xs:simpleType" XML element.
 */
abstract class SimpleTypeDefinition private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends TypeDefinition(bridgeElem, childElems) {

  assert(resolvedName == XsSimpleTypeEName, "The element must be an 'simpleType' element")
}

/**
 * Named simple type definition. That is, the "xs:simpleType" XML element.
 */
final class NamedSimpleTypeDefinition private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends SimpleTypeDefinition(bridgeElem, childElems) with NamedTypeDefinition {

  assert(bridgeElem.path.entries.size == 1, "Must be global")

  def targetNamespaceOption: Option[String] = bridgeElem.rootElem.attributeOption(TargetNamespaceEName)
}

/**
 * Anonymous simple type definition. That is, the "xs:simpleType" XML element.
 */
final class AnonymousSimpleTypeDefinition private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends SimpleTypeDefinition(bridgeElem, childElems) with AnonymousTypeDefinition {

  assert(bridgeElem.path.entries.size >= 2, "Must not be global")
}

/**
 * Complex type definition. That is, the "xs:complexType" XML element.
 */
abstract class ComplexTypeDefinition private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends TypeDefinition(bridgeElem, childElems) {

  assert(resolvedName == XsComplexTypeEName, "The element must be an 'complexType' element")
}

/**
 * Named complex type definition. That is, the "xs:complexType" XML element.
 */
final class NamedComplexTypeDefinition private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ComplexTypeDefinition(bridgeElem, childElems) with NamedTypeDefinition {

  assert(bridgeElem.path.entries.size == 1, "Must be global")

  def targetNamespaceOption: Option[String] = bridgeElem.rootElem.attributeOption(TargetNamespaceEName)
}

/**
 * Anonymous complex type definition. That is, the "xs:complexType" XML element.
 */
final class AnonymousComplexTypeDefinition private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ComplexTypeDefinition(bridgeElem, childElems) with AnonymousTypeDefinition {

  assert(bridgeElem.path.entries.size >= 2, "Must not be global")
}

/**
 * Attribute group definition or reference. That is, the "xs:attributeGroup" XML element.
 */
abstract class AttributeGroupDefinitionOrReference private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsAttributeGroupEName, "The element must be an 'attributeGroup' element")
}

/**
 * Attribute group definition. That is, the "xs:attributeGroup" XML element.
 */
final class AttributeGroupDefinition private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends AttributeGroupDefinitionOrReference(bridgeElem, childElems) with HasName {

  assert(attributeOption(RefEName).isEmpty, "Must not be a reference")
  assert(attributeOption(NameEName).isDefined, "Must have a name")

  def targetNamespaceOption: Option[String] = bridgeElem.rootElem.attributeOption(TargetNamespaceEName)
}

/**
 * Attribute group reference. That is, the "xs:attributeGroup" XML element.
 */
final class AttributeGroupReference private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends AttributeGroupDefinitionOrReference(bridgeElem, childElems) with IsReference {

  assert(attributeOption(RefEName).isDefined, "Must be a reference")
  assert(attributeOption(NameEName).isEmpty, "Must not have a name")
}

/**
 * Identity constraint definition. That is, the "xs:key", "xs:keyref" or "xs:unique" XML element.
 */
abstract class IdentityConstraintDefinition private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(
    Set(XsKeyEName, XsKeyrefEName, XsUniqueEName).contains(resolvedName),
    "The element must be an 'key', 'keyref' or 'unique' element")
}

/**
 * Identity constraint definition "xs:key".
 */
final class KeyConstraint private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends IdentityConstraintDefinition(bridgeElem, childElems) {

  assert(resolvedName == XsKeyEName, "The element must be a 'key' element")
}

/**
 * Identity constraint definition "xs:keyref".
 */
final class KeyrefConstraint private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends IdentityConstraintDefinition(bridgeElem, childElems) {

  assert(resolvedName == XsKeyrefEName, "The element must be a 'keyref' element")
}

/**
 * Identity constraint definition "xs:unique".
 */
final class UniqueConstraint private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends IdentityConstraintDefinition(bridgeElem, childElems) {

  assert(resolvedName == XsUniqueEName, "The element must be a 'unique' element")
}

/**
 * Model group definition. That is, the "xs:group" XML element introducing a named model group.
 */
abstract class ModelGroupDefinitionOrReference private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsGroupEName, "The element must be a 'group' element")
}

/**
 * Model group definition. That is, the "xs:group" XML element introducing a named model group.
 */
final class ModelGroupDefinition private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ModelGroupDefinitionOrReference(bridgeElem, childElems) {

  assert(attributeOption(RefEName).isEmpty, "The element must have no 'ref' attribute")
}

/**
 * Model group reference. That is, the "xs:group" XML element referring to a named model group.
 */
final class ModelGroupReference private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ModelGroupDefinitionOrReference(bridgeElem, childElems) with Particle with IsReference {

  assert(attributeOption(RefEName).isDefined, "The element must have a 'ref' attribute")
}

/**
 * Notation declaration. That is, the "xs:notation" XML element.
 */
final class NotationDeclaration private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsNotationEName, "The element must be a 'notation' element")
}

/**
 * Model group. That is, the "xs:all", "xs:sequence" or "xs:choice" XML element.
 */
abstract class ModelGroup private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) with Particle {

  assert(
    Set(XsAllEName, XsSequenceEName, XsChoiceEName).contains(bridgeElem.resolvedName),
    "The element must be an 'all', 'sequence' or 'choice' element")
  // TODO Check in apply method, and replace require by assert here
  require(
    !inNamedGroup || (minOccursAttrOption.isEmpty && maxOccursAttrOption.isEmpty),
    "If in a named group, there must be no @minOccurs and @maxOccurs")

  final def inNamedGroup: Boolean = {
    assert(bridgeElem.path.parentPathOption.isDefined)
    val parentPath = bridgeElem.path.parentPath

    val parent = bridgeElem.rootElem.getElemOrSelfByPath(parentPath)
    (parent.resolvedName == XsGroupEName) && ((parent \@ NameEName).isDefined)
  }
}

/**
 * Model group "all".
 */
final class AllGroup private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ModelGroup(bridgeElem, childElems) {

  assert(resolvedName == XsAllEName, "The element must be an 'all' element")
}

/**
 * Model group "choice".
 */
final class ChoiceGroup private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ModelGroup(bridgeElem, childElems) {

  assert(resolvedName == XsChoiceEName, "The element must be a 'choice' element")
}

/**
 * Model group "sequence".
 */
final class SequenceGroup private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ModelGroup(bridgeElem, childElems) {

  assert(resolvedName == XsSequenceEName, "The element must be a 'sequence' element")
}

/**
 * Wildcard "xs:any".
 */
final class AnyWildcard private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) with Particle {

  assert(resolvedName == XsAnyEName, "The element must be an 'any' element")
}

/**
 * Wildcard "xs:anyAttribute".
 */
final class AnyAttributeWildcard private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsAnyAttributeEName, "The element must be an 'anyAttribute' element")
}

/**
 * Annotation schema component. That is, the "xs:annotation" XML element.
 */
final class Annotation private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsAnnotationEName, "The element must be an 'annotation' element")
}

// Import, include, redefine

/**
 * The "xs:import" XML element.
 */
final class Import private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsImportEName, "The element must be an 'import' element")
}

/**
 * The "xs:include" XML element.
 */
final class Include private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsIncludeEName, "The element must be an 'include' element")
}

/**
 * The "xs:redefine" XML element.
 */
final class Redefine private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsRedefineEName, "The element must be a 'redefine' element")
}

// Other schema parts, that are not Schema Components themselves, such as extension, restriction, etc.

/**
 * The "xs:complexContent" XML element.
 */
final class ComplexContent private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsComplexContentEName, "The element must be a 'complexContent' element")
}

/**
 * The "xs:simpleContent" XML element.
 */
final class SimpleContent private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsSimpleContentEName, "The element must be a 'simpleContent' element")
}

/**
 * The "xs:extension" XML element.
 */
final class Extension private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsExtensionEName, "The element must be an 'extension' element")
}

/**
 * The "xs:restriction" XML element.
 */
final class Restriction private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsRestrictionEName, "The element must be a 'restriction' element")
}

/**
 * The "xs:field" XML element.
 */
final class Field private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsFieldEName, "The element must be a 'field' element")
}

/**
 * The "xs:selector" XML element.
 */
final class Selector private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsSelectorEName, "The element must be a 'selector' element")
}

/**
 * The "xs:appinfo" XML element.
 */
final class Appinfo private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsAppinfoEName, "The element must be an 'appinfo' element")
}

/**
 * The "xs:documentation" XML element.
 */
final class Documentation private[xs] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(bridgeElem, childElems) {

  assert(resolvedName == XsDocumentationEName, "The element must be a 'documentation' element")
}

// Companion objects

object SchemaRootElem {

  /**
   * Public factory method for SchemaRootElem instances. It returns `XsdElem(bridgeElem)` as a `SchemaRootElem`.
   *
   * This is an expensive method, but once a `SchemaRootElem` has been created, querying through the `ElemLike` API is very fast.
   */
  def apply(elem: IndexedBridgeElem): SchemaRootElem = {
    require(elem.path.isRoot)

    val childElems = elem.findAllChildElems.map(e => XsdElem.apply(e))
    new SchemaRootElem(elem, childElems)
  }
}

object XsdElem {

  // Capabilities

  trait CanBeAbstract { self: XsdElem =>

    /**
     * Returns true if and only if the element declaration is abstract.
     * Only global element declarations can be abstract.
     */
    final def isAbstract: Boolean = abstractOption(bridgeElem) == Some(true)

    final def abstractOption(elem: IndexedBridgeElem): Option[Boolean] = {
      try {
        self.attributeOption(AbstractEName) map (_.toBoolean)
      } catch {
        case e: Exception => None
      }
    }
  }

  trait HasName { self: XsdElem =>

    def targetNamespaceOption: Option[String]

    /**
     * Returns the `EName` by combining the target namespace and the value of the "name" attribute.
     */
    final def targetEName: EName = {
      val tnsOption = targetNamespaceOption
      EName(tnsOption, nameAttribute)
    }

    /**
     * Returns the value of the "name" attribute
     */
    final def nameAttribute: String = (self \@ NameEName).getOrElse(sys.error("Expected @name"))
  }

  trait IsReference { self: XsdElem =>

    /**
     * Returns the value of the 'ref' attribute as expanded name.
     */
    final def ref: EName =
      self.attributeAsResolvedQNameOption(RefEName).getOrElse(sys.error("Attribute references must have a ref attribute"))
  }

  /**
   * Recursive public factory method for XsdElem instances. Indeed, construction of an XsdElem is expensive,
   * but after construction querying is very fast, due to the stored child XsdElems.
   */
  def apply(elem: IndexedBridgeElem): XsdElem = {
    // TODO Better error messages, and more checks, so that constructors only need assertions and no require statements
    // TODO Turn this into a validating factory method that accumulates validation errors

    // require(
    //   elem.rootElem.resolvedName == XsSchemaEName,
    //   "The root of the element tree must be a 'schema' element")
    // require(
    //   (elem.resolvedName == XsSchemaEName) || (!elem.path.isRoot),
    //   "This element must either be a 'schema' element, or not be the root of the element tree")

    // Recursive calls
    val childElems = elem.findAllChildElems.map(e => XsdElem.apply(e))

    assert(
      childElems.map(_.bridgeElem) == elem.findAllChildElems,
      "Corrupt element!")

    def attributeOption(e: IndexedBridgeElem, ename: EName): Option[String] =
      e.resolvedAttributes.toMap.filterKeys(Set(ename)).map(_._2).headOption

    elem.resolvedName match {
      case XsSchemaEName =>
        new SchemaRootElem(elem, childElems)
      case XsElementEName if elem.path.entries.size == 1 =>
        require(attributeOption(elem, NameEName).isDefined)
        new GlobalElementDeclaration(elem, childElems)
      case XsElementEName if attributeOption(elem, RefEName).isDefined =>
        require(attributeOption(elem, NameEName).isEmpty)
        new ElementReference(elem, childElems)
      case XsElementEName if attributeOption(elem, NameEName).isDefined =>
        require(attributeOption(elem, RefEName).isEmpty)
        new LocalElementDeclaration(elem, childElems)
      case XsElementEName =>
        sys.error(s"Not an element declaration or reference")
      case XsAttributeEName if elem.path.entries.size == 1 =>
        require(attributeOption(elem, NameEName).isDefined)
        new GlobalAttributeDeclaration(elem, childElems)
      case XsAttributeEName if attributeOption(elem, RefEName).isDefined =>
        require(attributeOption(elem, NameEName).isEmpty)
        new AttributeReference(elem, childElems)
      case XsAttributeEName if attributeOption(elem, NameEName).isDefined =>
        require(attributeOption(elem, RefEName).isEmpty)
        new LocalAttributeDeclaration(elem, childElems)
      case XsAttributeEName =>
        sys.error(s"Not an attribute declaration or reference")
      case XsSimpleTypeEName if elem.path.entries.size == 1 =>
        require(attributeOption(elem, NameEName).isDefined)
        new NamedSimpleTypeDefinition(elem, childElems)
      case XsSimpleTypeEName =>
        require(attributeOption(elem, NameEName).isEmpty)
        new AnonymousSimpleTypeDefinition(elem, childElems)
      case XsComplexTypeEName if elem.path.entries.size == 1 =>
        require(attributeOption(elem, NameEName).isDefined)
        new NamedComplexTypeDefinition(elem, childElems)
      case XsComplexTypeEName =>
        require(attributeOption(elem, NameEName).isEmpty)
        new AnonymousComplexTypeDefinition(elem, childElems)
      case XsAttributeGroupEName if attributeOption(elem, RefEName).isDefined =>
        require(attributeOption(elem, NameEName).isEmpty)
        new AttributeGroupReference(elem, childElems)
      case XsAttributeGroupEName if attributeOption(elem, NameEName).isDefined =>
        require(attributeOption(elem, RefEName).isEmpty)
        new AttributeGroupDefinition(elem, childElems)
      case XsAttributeGroupEName =>
        sys.error(s"Not an attribute group definition or reference")
      case XsKeyEName =>
        new KeyConstraint(elem, childElems)
      case XsKeyrefEName =>
        new KeyrefConstraint(elem, childElems)
      case XsUniqueEName =>
        new UniqueConstraint(elem, childElems)
      case XsGroupEName if attributeOption(elem, RefEName).isDefined =>
        new ModelGroupReference(elem, childElems)
      case XsGroupEName =>
        new ModelGroupDefinition(elem, childElems)
      case XsAllEName =>
        new AllGroup(elem, childElems)
      case XsSequenceEName =>
        new SequenceGroup(elem, childElems)
      case XsChoiceEName =>
        new ChoiceGroup(elem, childElems)
      case XsNotationEName =>
        new NotationDeclaration(elem, childElems)
      case XsAnnotationEName =>
        new Annotation(elem, childElems)
      case XsAnyEName =>
        new AnyWildcard(elem, childElems)
      case XsAnyAttributeEName =>
        new AnyAttributeWildcard(elem, childElems)
      case XsImportEName =>
        new Import(elem, childElems)
      case XsIncludeEName =>
        new Include(elem, childElems)
      case XsRedefineEName =>
        new Redefine(elem, childElems)
      case XsComplexContentEName =>
        new ComplexContent(elem, childElems)
      case XsSimpleContentEName =>
        new SimpleContent(elem, childElems)
      case XsAppinfoEName =>
        new Appinfo(elem, childElems)
      case XsDocumentationEName =>
        new Documentation(elem, childElems)
      case XsExtensionEName =>
        new Extension(elem, childElems)
      case XsRestrictionEName =>
        new Restriction(elem, childElems)
      case XsFieldEName =>
        new Field(elem, childElems)
      case XsSelectorEName =>
        new Selector(elem, childElems)
      case _ =>
        new XsdElem(elem, childElems)
    }
  }
}

object Particle {

  val Unbounded = -1
}
