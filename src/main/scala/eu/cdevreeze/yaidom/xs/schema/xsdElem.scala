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
import eu.cdevreeze.yaidom.xs.schemaapi
import XsdElem._

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
 * The ancestry of an element is kept by keeping an `docaware.Elem` as state. Redundantly, the `childElems` are also kept as
 * state, to make `ElemLike` API querying very fast, with little object creation overhead. The `XsdElem` implementation
 * makes sure that the following indeed holds:
 * {{{
 * childElems.map(_.docawareElem) == docawareElem.findAllChildElems
 * }}}
 *
 * @author Chris de Vreeze
 */
class XsdElem private[schema] (
  val docawareElem: docaware.Elem,
  val childElems: immutable.IndexedSeq[XsdElem]) extends ElemLike[XsdElem] with SubtypeAwareParentElemLike[XsdElem] with HasText with schemaapi.XsdElem[XsdElem] with Immutable {

  type GlobalElementDeclarationType = GlobalElementDeclaration

  type GlobalAttributeDeclarationType = GlobalAttributeDeclaration

  /**
   * The yaidom Elem itself, stored as a val
   */
  final val elem: Elem = docawareElem.elem

  assert(
    childElems.map(_.docawareElem) == docawareElem.findAllChildElems,
    "Corrupt element!")

  assert(docawareElem.rootElem.resolvedName == XsSchemaEName, "The root of the element tree must be a 'schema' element")
  assert(
    (elem.resolvedName == XsSchemaEName) || (!docawareElem.path.isRoot),
    "This element must either be a 'schema' element, or not be the root of the element tree")

  /**
   * Returns all child elements, in the correct order. That is, returns `childElems`.
   *
   * These child elements share the same rootElem (and doc URI) with this element, but differ in the element paths, which have
   * one more "path entry".
   *
   * The implementation is extremely fast, which is needed for fast querying through SchemaApi query methods.
   */
  final override def findAllChildElems: immutable.IndexedSeq[XsdElem] = childElems

  final override def resolvedName: EName = elem.resolvedName

  final override def resolvedAttributes: immutable.IndexedSeq[(EName, String)] = elem.resolvedAttributes

  final def docUri: URI = docawareElem.docUri

  final override def equals(obj: Any): Boolean = obj match {
    case other: XsdElem =>
      (other.docawareElem == this.docawareElem) && (other.docUri == this.docUri)
    case _ => false
  }

  final override def hashCode: Int = (docawareElem, docUri).hashCode

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
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.SchemaRootElem[XsdElem] {

  assert(elem.resolvedName == XsSchemaEName, "The element must be a 'schema' element")
  assert(docawareElem.path.isRoot, "The element must be the root of the element tree")

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
trait Particle extends XsdElem with schemaapi.Particle[XsdElem] {

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
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.ElementDeclarationOrReference[XsdElem] {

  assert(elem.resolvedName == XsElementEName, "The element must be an 'element' element")
}

/**
 * Element declaration. An element declaration is either a global or local element declaration.
 */
abstract class ElementDeclaration private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ElementDeclarationOrReference(docawareElem, childElems) with HasName with schemaapi.ElementDeclaration[XsdElem] {

  assert((elem \@ RefEName).isEmpty, "Must not be a reference")
  assert((elem \@ NameEName).isDefined, "Must have a name")

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
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ElementDeclaration(docawareElem, childElems) with CanBeAbstract with schemaapi.GlobalElementDeclaration[XsdElem] {

  assert(docawareElem.path.entries.size == 1, "Must be global")

  def targetNamespaceOption: Option[String] = docawareElem.rootElem.attributeOption(TargetNamespaceEName)

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
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ElementDeclaration(docawareElem, childElems) with Particle with schemaapi.LocalElementDeclaration[XsdElem] {

  assert(docawareElem.path.entries.size >= 2, "Must be local")

  final def targetNamespaceOption: Option[String] = {
    val tnsOption = docawareElem.rootElem \@ TargetNamespaceEName
    if (isQualified) tnsOption else None
  }

  private def isQualified: Boolean = isQualified(docawareElem.path)

  private def isQualified(path: Path): Boolean = {
    if (path.isRoot) {
      docawareElem.rootElem.attributeOption(ElementFormDefaultEName) map {
        case "qualified" => true
        case "unqualified" => false
      } getOrElse false
    } else {
      docawareElem.elem.attributeOption(FormEName) map {
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
final class ElementReference private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ElementDeclarationOrReference(docawareElem, childElems) with Particle with IsReference with schemaapi.ElementReference[XsdElem] {

  assert((elem \@ RefEName).isDefined, "Must be a reference")
  assert(docawareElem.path.entries.size >= 2, "Must not be global")
}

/**
 * Attribute declaration or attribute reference. That is, the "xs:attribute" XML element.
 */
abstract class AttributeDeclarationOrReference private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.AttributeDeclarationOrReference[XsdElem] {

  assert(elem.resolvedName == XsAttributeEName, "The element must be an 'attribute' element")
}

/**
 * Attribute declaration. An attribute declaration is either a global or local attribute declaration.
 */
abstract class AttributeDeclaration private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends AttributeDeclarationOrReference(docawareElem, childElems) with HasName with schemaapi.AttributeDeclaration[XsdElem] {

  assert((elem \@ RefEName).isEmpty, "Must not be a reference")
  assert((elem \@ NameEName).isDefined, "Must have a name")

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
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends AttributeDeclaration(docawareElem, childElems) with schemaapi.GlobalAttributeDeclaration[XsdElem] {

  assert(docawareElem.path.entries.size == 1, "Must be global")

  def targetNamespaceOption: Option[String] = docawareElem.rootElem.attributeOption(TargetNamespaceEName)
}

/**
 * Local attribute declaration.
 */
final class LocalAttributeDeclaration private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends AttributeDeclaration(docawareElem, childElems) with schemaapi.LocalAttributeDeclaration[XsdElem] {

  assert(docawareElem.path.entries.size >= 2, "Must be local")

  final def targetNamespaceOption: Option[String] = {
    val tnsOption = docawareElem.rootElem \@ TargetNamespaceEName
    if (isQualified) tnsOption else None
  }

  private def isQualified: Boolean = isQualified(docawareElem.path)

  private def isQualified(path: Path): Boolean = {
    if (path.isRoot) {
      docawareElem.rootElem.attributeOption(AttributeFormDefaultEName) map {
        case "qualified" => true
        case "unqualified" => false
      } getOrElse false
    } else {
      docawareElem.elem.attributeOption(FormEName) map {
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
final class AttributeReference private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends AttributeDeclarationOrReference(docawareElem, childElems) with IsReference with schemaapi.AttributeReference[XsdElem] {

  assert((elem \@ RefEName).isDefined, "Must be a reference")
  assert(docawareElem.path.entries.size >= 2, "Must not be global")
}

/**
 * Schema type definition, which is either a simple type or a complex type.
 */
abstract class TypeDefinition private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.TypeDefinition[XsdElem] {
}

trait NamedTypeDefinition extends TypeDefinition with HasName

trait AnonymousTypeDefinition extends TypeDefinition

/**
 * Simple type definition. That is, the "xs:simpleType" XML element.
 */
abstract class SimpleTypeDefinition private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends TypeDefinition(docawareElem, childElems) with schemaapi.SimpleTypeDefinition[XsdElem] {

  assert(elem.resolvedName == XsSimpleTypeEName, "The element must be an 'simpleType' element")
}

/**
 * Named simple type definition. That is, the "xs:simpleType" XML element.
 */
final class NamedSimpleTypeDefinition private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends SimpleTypeDefinition(docawareElem, childElems) with NamedTypeDefinition with schemaapi.NamedSimpleTypeDefinition[XsdElem] {

  assert(docawareElem.path.entries.size == 1, "Must be global")

  def targetNamespaceOption: Option[String] = docawareElem.rootElem.attributeOption(TargetNamespaceEName)
}

/**
 * Anonymous simple type definition. That is, the "xs:simpleType" XML element.
 */
final class AnonymousSimpleTypeDefinition private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends SimpleTypeDefinition(docawareElem, childElems) with AnonymousTypeDefinition with schemaapi.AnonymousSimpleTypeDefinition[XsdElem] {

  assert(docawareElem.path.entries.size >= 2, "Must not be global")
}

/**
 * Complex type definition. That is, the "xs:complexType" XML element.
 */
abstract class ComplexTypeDefinition private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends TypeDefinition(docawareElem, childElems) with schemaapi.ComplexTypeDefinition[XsdElem] {

  assert(elem.resolvedName == XsComplexTypeEName, "The element must be an 'complexType' element")
}

/**
 * Named complex type definition. That is, the "xs:complexType" XML element.
 */
final class NamedComplexTypeDefinition private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ComplexTypeDefinition(docawareElem, childElems) with NamedTypeDefinition with schemaapi.NamedComplexTypeDefinition[XsdElem] {

  assert(docawareElem.path.entries.size == 1, "Must be global")

  def targetNamespaceOption: Option[String] = docawareElem.rootElem.attributeOption(TargetNamespaceEName)
}

/**
 * Anonymous complex type definition. That is, the "xs:complexType" XML element.
 */
final class AnonymousComplexTypeDefinition private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ComplexTypeDefinition(docawareElem, childElems) with AnonymousTypeDefinition with schemaapi.AnonymousComplexTypeDefinition[XsdElem] {

  assert(docawareElem.path.entries.size >= 2, "Must not be global")
}

/**
 * Attribute group definition or reference. That is, the "xs:attributeGroup" XML element.
 */
abstract class AttributeGroupDefinitionOrReference private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.AttributeGroupDefinitionOrReference[XsdElem] {

  assert(elem.resolvedName == XsAttributeGroupEName, "The element must be an 'attributeGroup' element")
}

/**
 * Attribute group definition. That is, the "xs:attributeGroup" XML element.
 */
final class AttributeGroupDefinition private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends AttributeGroupDefinitionOrReference(docawareElem, childElems) with HasName with schemaapi.AttributeGroupDefinition[XsdElem] {

  assert((elem \@ RefEName).isEmpty, "Must not be a reference")
  assert((elem \@ NameEName).isDefined, "Must have a name")

  def targetNamespaceOption: Option[String] = docawareElem.rootElem.attributeOption(TargetNamespaceEName)
}

/**
 * Attribute group reference. That is, the "xs:attributeGroup" XML element.
 */
final class AttributeGroupReference private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends AttributeGroupDefinitionOrReference(docawareElem, childElems) with IsReference with schemaapi.AttributeGroupReference[XsdElem] {

  assert((elem \@ RefEName).isDefined, "Must be a reference")
  assert((elem \@ NameEName).isEmpty, "Must not have a name")
}

/**
 * Identity constraint definition. That is, the "xs:key", "xs:keyref" or "xs:unique" XML element.
 */
abstract class IdentityConstraintDefinition private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.IdentityConstraintDefinition[XsdElem] {

  assert(
    Set(XsKeyEName, XsKeyrefEName, XsUniqueEName).contains(elem.resolvedName),
    "The element must be an 'key', 'keyref' or 'unique' element")
}

/**
 * Identity constraint definition "xs:key".
 */
final class KeyConstraint private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends IdentityConstraintDefinition(docawareElem, childElems) with schemaapi.KeyConstraint[XsdElem] {

  assert(elem.resolvedName == XsKeyEName, "The element must be a 'key' element")
}

/**
 * Identity constraint definition "xs:keyref".
 */
final class KeyrefConstraint private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends IdentityConstraintDefinition(docawareElem, childElems) with schemaapi.KeyrefConstraint[XsdElem] {

  assert(elem.resolvedName == XsKeyrefEName, "The element must be a 'keyref' element")
}

/**
 * Identity constraint definition "xs:unique".
 */
final class UniqueConstraint private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends IdentityConstraintDefinition(docawareElem, childElems) with schemaapi.UniqueConstraint[XsdElem] {

  assert(elem.resolvedName == XsUniqueEName, "The element must be a 'unique' element")
}

/**
 * Model group definition. That is, the "xs:group" XML element introducing a named model group.
 */
abstract class ModelGroupDefinitionOrReference private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.ModelGroupDefinitionOrReference[XsdElem] {

  assert(elem.resolvedName == XsGroupEName, "The element must be a 'group' element")
}

/**
 * Model group definition. That is, the "xs:group" XML element introducing a named model group.
 */
final class ModelGroupDefinition private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ModelGroupDefinitionOrReference(docawareElem, childElems) with schemaapi.ModelGroupDefinition[XsdElem] {

  assert((elem \@ RefEName).isEmpty, "The element must have no 'ref' attribute")
}

/**
 * Model group reference. That is, the "xs:group" XML element referring to a named model group.
 */
final class ModelGroupReference private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ModelGroupDefinitionOrReference(docawareElem, childElems) with Particle with IsReference with schemaapi.ModelGroupReference[XsdElem] {

  assert((elem \@ RefEName).isDefined, "The element must have a 'ref' attribute")
}

/**
 * Notation declaration. That is, the "xs:notation" XML element.
 */
final class NotationDeclaration private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.NotationDeclaration[XsdElem] {

  assert(elem.resolvedName == XsNotationEName, "The element must be a 'notation' element")
}

/**
 * Model group. That is, the "xs:all", "xs:sequence" or "xs:choice" XML element.
 */
abstract class ModelGroup private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with Particle with schemaapi.ModelGroup[XsdElem] {

  assert(
    Set(XsAllEName, XsSequenceEName, XsChoiceEName).contains(elem.resolvedName),
    "The element must be an 'all', 'sequence' or 'choice' element")
  // TODO Check in apply method, and replace require by assert here
  require(
    !inNamedGroup || (minOccursAttrOption.isEmpty && maxOccursAttrOption.isEmpty),
    "If in a named group, there must be no @minOccurs and @maxOccurs")

  final def inNamedGroup: Boolean = {
    assert(docawareElem.path.parentPathOption.isDefined)
    val parentPath = docawareElem.path.parentPath

    val parent = docawareElem.rootElem.getElemOrSelfByPath(parentPath)
    (parent.resolvedName == XsGroupEName) && ((parent \@ NameEName).isDefined)
  }
}

/**
 * Model group "all".
 */
final class AllGroup private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ModelGroup(docawareElem, childElems) with schemaapi.AllGroup[XsdElem] {

  assert(elem.resolvedName == XsAllEName, "The element must be an 'all' element")
}

/**
 * Model group "choice".
 */
final class ChoiceGroup private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ModelGroup(docawareElem, childElems) with schemaapi.ChoiceGroup[XsdElem] {

  assert(elem.resolvedName == XsChoiceEName, "The element must be a 'choice' element")
}

/**
 * Model group "sequence".
 */
final class SequenceGroup private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends ModelGroup(docawareElem, childElems) with schemaapi.SequenceGroup[XsdElem] {

  assert(elem.resolvedName == XsSequenceEName, "The element must be a 'sequence' element")
}

/**
 * Wildcard "xs:any".
 */
final class AnyWildcard private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with Particle with schemaapi.AnyWildcard[XsdElem] {

  assert(elem.resolvedName == XsAnyEName, "The element must be an 'any' element")
}

/**
 * Wildcard "xs:anyAttribute".
 */
final class AnyAttributeWildcard private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.AnyAttributeWildcard[XsdElem] {

  assert(elem.resolvedName == XsAnyAttributeEName, "The element must be an 'anyAttribute' element")
}

/**
 * Annotation schema component. That is, the "xs:annotation" XML element.
 */
final class Annotation private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.Annotation[XsdElem] {

  assert(elem.resolvedName == XsAnnotationEName, "The element must be an 'annotation' element")
}

// Import, include, redefine

/**
 * The "xs:import" XML element.
 */
final class Import private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.Import[XsdElem] {

  assert(elem.resolvedName == XsImportEName, "The element must be an 'import' element")
}

/**
 * The "xs:include" XML element.
 */
final class Include private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.Include[XsdElem] {

  assert(elem.resolvedName == XsIncludeEName, "The element must be an 'include' element")
}

/**
 * The "xs:redefine" XML element.
 */
final class Redefine private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.Redefine[XsdElem] {

  assert(elem.resolvedName == XsRedefineEName, "The element must be a 'redefine' element")
}

// Other schema parts, that are not Schema Components themselves, such as extension, restriction, etc.

/**
 * The "xs:complexContent" XML element.
 */
final class ComplexContent private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.ComplexContent[XsdElem] {

  assert(elem.resolvedName == XsComplexContentEName, "The element must be a 'complexContent' element")
}

/**
 * The "xs:simpleContent" XML element.
 */
final class SimpleContent private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.SimpleContent[XsdElem] {

  assert(elem.resolvedName == XsSimpleContentEName, "The element must be a 'simpleContent' element")
}

/**
 * The "xs:extension" XML element.
 */
final class Extension private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.Extension[XsdElem] {

  assert(elem.resolvedName == XsExtensionEName, "The element must be an 'extension' element")
}

/**
 * The "xs:restriction" XML element.
 */
final class Restriction private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.Restriction[XsdElem] {

  assert(elem.resolvedName == XsRestrictionEName, "The element must be a 'restriction' element")
}

/**
 * The "xs:field" XML element.
 */
final class Field private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.Field[XsdElem] {

  assert(elem.resolvedName == XsFieldEName, "The element must be a 'field' element")
}

/**
 * The "xs:selector" XML element.
 */
final class Selector private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.Selector[XsdElem] {

  assert(elem.resolvedName == XsSelectorEName, "The element must be a 'selector' element")
}

/**
 * The "xs:appinfo" XML element.
 */
final class Appinfo private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.Appinfo[XsdElem] {

  assert(elem.resolvedName == XsAppinfoEName, "The element must be an 'appinfo' element")
}

/**
 * The "xs:documentation" XML element.
 */
final class Documentation private[schema] (
  docawareElem: docaware.Elem,
  childElems: immutable.IndexedSeq[XsdElem]) extends XsdElem(docawareElem, childElems) with schemaapi.Documentation[XsdElem] {

  assert(elem.resolvedName == XsDocumentationEName, "The element must be a 'documentation' element")
}

// Companion objects

object SchemaRootElem {

  /**
   * Public factory method for SchemaRootElem instances. It returns `XsdElem(docawareElem, docUri)` as a `SchemaRootElem`.
   *
   * This is an expensive method, but once a `SchemaRootElem` has been created, querying through the `ElemLike` API is very fast.
   */
  def apply(docawareElem: docaware.Elem): SchemaRootElem = {
    require(docawareElem.path.isRoot)

    val childElems = docawareElem.findAllChildElems.map(e => XsdElem.apply(e))
    new SchemaRootElem(docawareElem, childElems)
  }
}

object XsdElem {

  // Capabilities

  trait CanBeAbstract { self: XsdElem =>

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
    final def nameAttribute: String = (elem \@ NameEName).getOrElse(sys.error("Expected @name"))
  }

  trait IsReference { self: XsdElem =>

    /**
     * Returns the value of the 'ref' attribute as expanded name.
     */
    final def ref: EName =
      elem.attributeAsResolvedQNameOption(RefEName).getOrElse(sys.error("Attribute references must have a ref attribute"))
  }

  /**
   * Recursive public factory method for XsdElem instances. Indeed, construction of an XsdElem is expensive,
   * but after construction querying is very fast, due to the stored child XsdElems.
   */
  def apply(docawareElem: docaware.Elem): XsdElem = {
    // TODO Better error messages, and more checks, so that constructors only need assertions and no require statements
    // TODO Turn this into a validating factory method that accumulates validation errors

    require(
      docawareElem.rootElem.resolvedName == XsSchemaEName,
      "The root of the element tree must be a 'schema' element")
    require(
      (docawareElem.resolvedName == XsSchemaEName) || (!docawareElem.path.isRoot),
      "This element must either be a 'schema' element, or not be the root of the element tree")

    // Recursive calls
    val childElems = docawareElem.findAllChildElems.map(e => XsdElem.apply(e))

    assert(
      childElems.map(_.docawareElem) == docawareElem.findAllChildElems,
      "Corrupt element!")

    docawareElem.resolvedName match {
      case XsSchemaEName =>
        new SchemaRootElem(docawareElem, childElems)
      case XsElementEName if docawareElem.path.entries.size == 1 =>
        require(docawareElem.attributeOption(NameEName).isDefined)
        new GlobalElementDeclaration(docawareElem, childElems)
      case XsElementEName if docawareElem.attributeOption(RefEName).isDefined =>
        require(docawareElem.attributeOption(NameEName).isEmpty)
        new ElementReference(docawareElem, childElems)
      case XsElementEName if docawareElem.attributeOption(NameEName).isDefined =>
        require(docawareElem.attributeOption(RefEName).isEmpty)
        new LocalElementDeclaration(docawareElem, childElems)
      case XsElementEName =>
        sys.error(s"Not an element declaration or reference")
      case XsAttributeEName if docawareElem.path.entries.size == 1 =>
        require(docawareElem.attributeOption(NameEName).isDefined)
        new GlobalAttributeDeclaration(docawareElem, childElems)
      case XsAttributeEName if docawareElem.attributeOption(RefEName).isDefined =>
        require(docawareElem.attributeOption(NameEName).isEmpty)
        new AttributeReference(docawareElem, childElems)
      case XsAttributeEName if docawareElem.attributeOption(NameEName).isDefined =>
        require(docawareElem.attributeOption(RefEName).isEmpty)
        new LocalAttributeDeclaration(docawareElem, childElems)
      case XsAttributeEName =>
        sys.error(s"Not an attribute declaration or reference")
      case XsSimpleTypeEName if docawareElem.path.entries.size == 1 =>
        require(docawareElem.attributeOption(NameEName).isDefined)
        new NamedSimpleTypeDefinition(docawareElem, childElems)
      case XsSimpleTypeEName =>
        require(docawareElem.attributeOption(NameEName).isEmpty)
        new AnonymousSimpleTypeDefinition(docawareElem, childElems)
      case XsComplexTypeEName if docawareElem.path.entries.size == 1 =>
        require(docawareElem.attributeOption(NameEName).isDefined)
        new NamedComplexTypeDefinition(docawareElem, childElems)
      case XsComplexTypeEName =>
        require(docawareElem.attributeOption(NameEName).isEmpty)
        new AnonymousComplexTypeDefinition(docawareElem, childElems)
      case XsAttributeGroupEName if docawareElem.attributeOption(RefEName).isDefined =>
        require(docawareElem.attributeOption(NameEName).isEmpty)
        new AttributeGroupReference(docawareElem, childElems)
      case XsAttributeGroupEName if docawareElem.attributeOption(NameEName).isDefined =>
        require(docawareElem.attributeOption(RefEName).isEmpty)
        new AttributeGroupDefinition(docawareElem, childElems)
      case XsAttributeGroupEName =>
        sys.error(s"Not an attribute group definition or reference")
      case XsKeyEName =>
        new KeyConstraint(docawareElem, childElems)
      case XsKeyrefEName =>
        new KeyrefConstraint(docawareElem, childElems)
      case XsUniqueEName =>
        new UniqueConstraint(docawareElem, childElems)
      case XsGroupEName if (docawareElem \@ RefEName).isDefined =>
        new ModelGroupReference(docawareElem, childElems)
      case XsGroupEName =>
        new ModelGroupDefinition(docawareElem, childElems)
      case XsAllEName =>
        new AllGroup(docawareElem, childElems)
      case XsSequenceEName =>
        new SequenceGroup(docawareElem, childElems)
      case XsChoiceEName =>
        new ChoiceGroup(docawareElem, childElems)
      case XsNotationEName =>
        new NotationDeclaration(docawareElem, childElems)
      case XsAnnotationEName =>
        new Annotation(docawareElem, childElems)
      case XsAnyEName =>
        new AnyWildcard(docawareElem, childElems)
      case XsAnyAttributeEName =>
        new AnyAttributeWildcard(docawareElem, childElems)
      case XsImportEName =>
        new Import(docawareElem, childElems)
      case XsIncludeEName =>
        new Include(docawareElem, childElems)
      case XsRedefineEName =>
        new Redefine(docawareElem, childElems)
      case XsComplexContentEName =>
        new ComplexContent(docawareElem, childElems)
      case XsSimpleContentEName =>
        new SimpleContent(docawareElem, childElems)
      case XsAppinfoEName =>
        new Appinfo(docawareElem, childElems)
      case XsDocumentationEName =>
        new Documentation(docawareElem, childElems)
      case XsExtensionEName =>
        new Extension(docawareElem, childElems)
      case XsRestrictionEName =>
        new Restriction(docawareElem, childElems)
      case XsFieldEName =>
        new Field(docawareElem, childElems)
      case XsSelectorEName =>
        new Selector(docawareElem, childElems)
      case _ =>
        new XsdElem(docawareElem, childElems)
    }
  }
}

object Particle {

  val Unbounded = -1
}
