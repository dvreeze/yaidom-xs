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

package eu.cdevreeze.yaidomxs.model.bridged

import java.net.URI

import scala.collection.immutable
import scala.reflect.classTag

import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.core.Path
import eu.cdevreeze.yaidom.core.QName
import eu.cdevreeze.yaidom.core.Scope
import eu.cdevreeze.yaidom.queryapi.Nodes
import eu.cdevreeze.yaidom.queryapi.ScopedElemLike
import eu.cdevreeze.yaidom.queryapi.SubtypeAwareElemLike
import eu.cdevreeze.yaidom.bridge.IndexedBridgeElem
import eu.cdevreeze.yaidomxs.model
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
 * Note that this class hierarchy directly implements the purely abstract traits of the model. There is no partial
 * implementation, inherited by these concrete model classes. That would likely have been impractical for various reasons.
 * First of all, a partial implementation would probably need to use generics throughout the trait hierarchy, unless
 * we avoided the yaidom query API traits. Yet the SubtypeAwareElemApi trait would be very helpful in such a partial
 * implementation. Secondly, there are many ways to create the objects of an XML Schema model. Do we build the entire
 * model recursively up-front, for querying speed, but sacrificing some creation overhead? Thirdly, a partial implementation
 * would probably need to make some assumptions about the (type of the) backing element. Fortunately, this class
 * hierarchy uses rather generic bridge elements, despite the lack of type parameters.
 *
 * Chameleon schemas are supported by the ability to pass an external target namespace. Such a chameleon schema document
 * instance (with the target namespace of the includer) is identified by the combination of external target namespace
 * and document URI.
 *
 * TODO Mind xsi:nil.
 *
 * It is not assumed that xs:schema is at the root of the document, because a schema document can be embedded.
 *
 * @author Chris de Vreeze
 */
sealed class XsdElem private[bridged] (
  val bridgeElem: IndexedBridgeElem,
  val childElems: immutable.IndexedSeq[XsdElem],
  val externalTnsOption: Option[String]) extends Nodes.Elem with ScopedElemLike[XsdElem] with SubtypeAwareElemLike[XsdElem] with model.XsdElem {

  assert(
    childElems.map(_.bridgeElem.backingElem) == bridgeElem.findAllChildElems.map(_.backingElem),
    s"Corrupt element!")

  final override type XSE = XsdElem
  final override type GED = GlobalElementDeclaration
  final override type GAD = GlobalAttributeDeclaration
  final override type NTD = NamedTypeDefinition

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
    case _          => false
  }

  final override def hashCode: Int = bridgeElem.backingElem.hashCode

  final override def toString: String = bridgeElem.toString

  final def idOption: Option[String] = attributeOption(model.IdEName)

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
 *
 * There are hardly any checks on the validity of the schema root element and its content. This is just a type-safe DOM view
 * of an XML tree that is meant to be XML Schema content, whether it is correct XML Schema content or not.
 */
final class SchemaRootElem private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.SchemaRootElem {

  assert(resolvedName == model.XsSchemaEName, "The element must be a 'schema' element")

  require(
    externalTnsOption.isEmpty || internalTargetNamespaceOption.isEmpty,
    s"Only chameleon schema documents can get another target namespace")

  private val allGlobalElementDeclarationsMappedByEName: Map[EName, GlobalElementDeclaration] = {
    findAllChildElemsOfType(classTag[GlobalElementDeclaration]).map(e => (e.targetEName -> e)).toMap
  }

  private val allGlobalAttributeDeclarationsMappedByEName: Map[EName, GlobalAttributeDeclaration] = {
    findAllChildElemsOfType(classTag[GlobalAttributeDeclaration]).map(e => (e.targetEName -> e)).toMap
  }

  private val allNamedTypeDefinitionsMappedByEName: Map[EName, NamedTypeDefinition] = {
    findAllChildElemsOfType(classTag[NamedTypeDefinition]).map(e => (e.targetEName -> e)).toMap
  }

  final def internalTargetNamespaceOption: Option[String] =
    attributeOption(model.TargetNamespaceEName)

  final def targetNamespaceOption: Option[String] =
    internalTargetNamespaceOption.orElse(externalTnsOption)

  final def findAllGlobalElementDeclarationsMappedByEName: Map[EName, GlobalElementDeclaration] =
    allGlobalElementDeclarationsMappedByEName

  final def findAllGlobalElementDeclarations: immutable.IndexedSeq[GlobalElementDeclaration] =
    findAllChildElemsOfType(classTag[GlobalElementDeclaration])

  final def filterGlobalElementDeclarations(p: GlobalElementDeclaration => Boolean): immutable.IndexedSeq[GlobalElementDeclaration] =
    filterChildElemsOfType(classTag[GlobalElementDeclaration])(p)

  final def findAllGlobalAttributeDeclarationsMappedByEName: Map[EName, GlobalAttributeDeclaration] =
    allGlobalAttributeDeclarationsMappedByEName

  final def findAllGlobalAttributeDeclarations: immutable.IndexedSeq[GlobalAttributeDeclaration] =
    findAllChildElemsOfType(classTag[GlobalAttributeDeclaration])

  final def filterGlobalAttributeDeclarations(p: GlobalAttributeDeclaration => Boolean): immutable.IndexedSeq[GlobalAttributeDeclaration] =
    filterChildElemsOfType(classTag[GlobalAttributeDeclaration])(p)

  final def findAllNamedTypeDefinitionsMappedByEName: Map[EName, NamedTypeDefinition] =
    allNamedTypeDefinitionsMappedByEName

  final def findAllNamedTypeDefinitions: immutable.IndexedSeq[NamedTypeDefinition] = {
    findAllChildElemsOfType(classTag[NamedTypeDefinition])
  }

  final def filterNamedTypeDefinitions(p: NamedTypeDefinition => Boolean): immutable.IndexedSeq[NamedTypeDefinition] = {
    filterChildElemsOfType(classTag[NamedTypeDefinition])(p)
  }

  final def findAllDirectSubstitutables(p: EName => Boolean): immutable.IndexedSeq[GlobalElementDeclaration] = {
    filterGlobalElementDeclarations { e => e.substitutionGroupOption.isDefined && p(e.substitutionGroupOption.get) }
  }

  final def findAllImports: immutable.IndexedSeq[Import] =
    findAllChildElemsOfType(classTag[Import])

  final def findAllIncludes: immutable.IndexedSeq[Include] =
    findAllChildElemsOfType(classTag[Include])

  final def findAllRedefines: immutable.IndexedSeq[Redefine] =
    findAllChildElemsOfType(classTag[Redefine])
}

// Schema Components

/**
 * Particle, having a min and max occurs (possibly default).
 */
sealed trait Particle extends XsdElem with model.Particle {

  final def minOccurs: Int = minOccursAttrOption map (_.toInt) getOrElse 1

  final def maxOccurs: Int = {
    maxOccursAttrOption map { v =>
      if (v.toLowerCase(java.util.Locale.ENGLISH) == "unbounded") Particle.Unbounded else 1
    } getOrElse 1
  }

  final def minOccursAttrOption: Option[String] = attributeOption(model.MinOccursEName)

  final def maxOccursAttrOption: Option[String] = attributeOption(model.MaxOccursEName)
}

/**
 * Element declaration or element reference. That is, the "xs:element" XML element.
 */
sealed abstract class ElementDeclarationOrReference private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.ElementDeclarationOrReference {

  assert(resolvedName == model.XsElementEName, "The element must be an 'element' element")
}

/**
 * Element declaration. An element declaration is either a global or local element declaration.
 */
sealed abstract class ElementDeclaration private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends ElementDeclarationOrReference(bridgeElem, childElems, externalTnsOption) with HasName with model.ElementDeclaration {

  assert(attributeOption(model.RefEName).isEmpty, "Must not be a reference")
  assert(attributeOption(model.NameEName).isDefined, "Must have a name")

  /**
   * Returns the value of the 'type' attribute as expanded name, if any, wrapped in an Option.
   */
  final def typeAttributeOption: Option[EName] = {
    val typeAttrAttrOption = attributeOption(model.TypeEName)
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
      attributeOption(model.NillableEName) map (_.toBoolean)
    } catch {
      case e: Exception => None
    }
  }
}

/**
 * Global element declaration.
 */
final class GlobalElementDeclaration private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends ElementDeclaration(bridgeElem, childElems, externalTnsOption) with CanBeAbstract with model.GlobalElementDeclaration {

  assert(XsdElem.isSchemaRootChild(bridgeElem), "Must be global")

  def internalTargetNamespaceOption: Option[String] = XsdElem.getSchemaRoot(bridgeElem).attributeOption(model.TargetNamespaceEName)

  def targetNamespaceOption: Option[String] = internalTargetNamespaceOption.orElse(externalTnsOption)

  /**
   * Returns the value of the 'substitutionGroup' attribute as expanded name, if any, wrapped in an Option.
   */
  final def substitutionGroupOption: Option[EName] = {
    val substGroupAttrOption = attributeOption(model.SubstitutionGroupEName)
    substGroupAttrOption map { substGroup =>
      scope.resolveQNameOption(QName(substGroup)).getOrElse(
        sys.error("Could not resolve substitution group '%s' as expanded name".format(substGroup)))
    }
  }
}

/**
 * Local element declaration.
 */
final class LocalElementDeclaration private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends ElementDeclaration(bridgeElem, childElems, externalTnsOption) with Particle with model.LocalElementDeclaration {

  assert(!XsdElem.isSchemaRootChild(bridgeElem), "Must be local")

  final def targetNamespaceOption: Option[String] = {
    val tnsOption = (XsdElem.getSchemaRoot(bridgeElem) \@ model.TargetNamespaceEName).orElse(externalTnsOption)
    if (isQualified) tnsOption else None
  }

  private def isQualified: Boolean = isQualified(bridgeElem.path)

  private def isQualified(path: Path): Boolean = {
    if (path.isRoot || path.lastEntry.elementName == model.XsSchemaEName) {
      XsdElem.getSchemaRoot(bridgeElem).attributeOption(model.ElementFormDefaultEName) map {
        case "qualified"   => true
        case "unqualified" => false
      } getOrElse false
    } else {
      attributeOption(model.FormEName) map {
        case "qualified"   => true
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
final class ElementReference private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends ElementDeclarationOrReference(bridgeElem, childElems, externalTnsOption)
  with Particle with IsReference with model.ElementReference {

  assert(attributeOption(model.RefEName).isDefined, "Must be a reference")
  assert(!XsdElem.isSchemaRootChild(bridgeElem), "Must not be global")
}

/**
 * Attribute declaration or attribute reference. That is, the "xs:attribute" XML element.
 */
sealed abstract class AttributeDeclarationOrReference private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.AttributeDeclarationOrReference {

  assert(resolvedName == model.XsAttributeEName, "The element must be an 'attribute' element")
}

/**
 * Attribute declaration. An attribute declaration is either a global or local attribute declaration.
 */
sealed abstract class AttributeDeclaration private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends AttributeDeclarationOrReference(bridgeElem, childElems, externalTnsOption) with HasName with model.AttributeDeclaration {

  assert(attributeOption(model.RefEName).isEmpty, "Must not be a reference")
  assert(attributeOption(model.NameEName).isDefined, "Must have a name")

  /**
   * Returns the value of the 'type' attribute as expanded name, if any, wrapped in an Option.
   */
  final def typeAttributeOption: Option[EName] = {
    val typeAttrAttrOption = attributeOption(model.TypeEName)
    typeAttrAttrOption map { tpe =>
      scope.resolveQNameOption(QName(tpe)).getOrElse(
        sys.error("Could not resolve type '%s' as expanded name".format(tpe)))
    }
  }
}

/**
 * Global attribute declaration.
 */
final class GlobalAttributeDeclaration private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends AttributeDeclaration(bridgeElem, childElems, externalTnsOption) with model.GlobalAttributeDeclaration {

  assert(XsdElem.isSchemaRootChild(bridgeElem), "Must be global")

  def internalTargetNamespaceOption: Option[String] = XsdElem.getSchemaRoot(bridgeElem).attributeOption(model.TargetNamespaceEName)

  def targetNamespaceOption: Option[String] = internalTargetNamespaceOption.orElse(externalTnsOption)
}

/**
 * Local attribute declaration.
 */
final class LocalAttributeDeclaration private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends AttributeDeclaration(bridgeElem, childElems, externalTnsOption) with model.LocalAttributeDeclaration {

  assert(!XsdElem.isSchemaRootChild(bridgeElem), "Must be local")

  final def targetNamespaceOption: Option[String] = {
    val tnsOption = (XsdElem.getSchemaRoot(bridgeElem) \@ model.TargetNamespaceEName).orElse(externalTnsOption)
    if (isQualified) tnsOption else None
  }

  private def isQualified: Boolean = isQualified(bridgeElem.path)

  private def isQualified(path: Path): Boolean = {
    if (path.isRoot || path.lastEntry.elementName == model.XsSchemaEName) {
      XsdElem.getSchemaRoot(bridgeElem).attributeOption(model.AttributeFormDefaultEName) map {
        case "qualified"   => true
        case "unqualified" => false
      } getOrElse false
    } else {
      attributeOption(model.FormEName) map {
        case "qualified"   => true
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
final class AttributeReference private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends AttributeDeclarationOrReference(bridgeElem, childElems, externalTnsOption) with IsReference with model.AttributeReference {

  assert(attributeOption(model.RefEName).isDefined, "Must be a reference")
  assert(!XsdElem.isSchemaRootChild(bridgeElem), "Must not be global")
}

/**
 * Schema type definition, which is either a simple type or a complex type.
 */
sealed abstract class TypeDefinition private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.TypeDefinition {
}

sealed trait NamedTypeDefinition extends TypeDefinition with HasName with model.NamedTypeDefinition

sealed trait AnonymousTypeDefinition extends TypeDefinition with model.AnonymousTypeDefinition

/**
 * Simple type definition. That is, the "xs:simpleType" XML element.
 */
sealed abstract class SimpleTypeDefinition private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends TypeDefinition(bridgeElem, childElems, externalTnsOption) with model.SimpleTypeDefinition {

  assert(resolvedName == model.XsSimpleTypeEName, "The element must be an 'simpleType' element")
}

/**
 * Named simple type definition. That is, the "xs:simpleType" XML element.
 */
final class NamedSimpleTypeDefinition private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends SimpleTypeDefinition(bridgeElem, childElems, externalTnsOption) with NamedTypeDefinition with model.NamedSimpleTypeDefinition {

  assert(XsdElem.isSchemaRootChild(bridgeElem), "Must be global")

  def internalTargetNamespaceOption: Option[String] = XsdElem.getSchemaRoot(bridgeElem).attributeOption(model.TargetNamespaceEName)

  def targetNamespaceOption: Option[String] = internalTargetNamespaceOption.orElse(externalTnsOption)
}

/**
 * Anonymous simple type definition. That is, the "xs:simpleType" XML element.
 */
final class AnonymousSimpleTypeDefinition private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends SimpleTypeDefinition(bridgeElem, childElems, externalTnsOption) with AnonymousTypeDefinition with model.AnonymousSimpleTypeDefinition {

  assert(!XsdElem.isSchemaRootChild(bridgeElem), "Must not be global")
}

/**
 * Complex type definition. That is, the "xs:complexType" XML element.
 */
sealed abstract class ComplexTypeDefinition private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends TypeDefinition(bridgeElem, childElems, externalTnsOption) with model.ComplexTypeDefinition {

  assert(resolvedName == model.XsComplexTypeEName, "The element must be an 'complexType' element")
}

/**
 * Named complex type definition. That is, the "xs:complexType" XML element.
 */
final class NamedComplexTypeDefinition private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends ComplexTypeDefinition(bridgeElem, childElems, externalTnsOption) with NamedTypeDefinition with model.NamedComplexTypeDefinition {

  assert(XsdElem.isSchemaRootChild(bridgeElem), "Must be global")

  def internalTargetNamespaceOption: Option[String] = XsdElem.getSchemaRoot(bridgeElem).attributeOption(model.TargetNamespaceEName)

  def targetNamespaceOption: Option[String] = internalTargetNamespaceOption.orElse(externalTnsOption)
}

/**
 * Anonymous complex type definition. That is, the "xs:complexType" XML element.
 */
final class AnonymousComplexTypeDefinition private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends ComplexTypeDefinition(bridgeElem, childElems, externalTnsOption) with AnonymousTypeDefinition with model.AnonymousComplexTypeDefinition {

  assert(!XsdElem.isSchemaRootChild(bridgeElem), "Must not be global")
}

/**
 * Attribute group definition or reference. That is, the "xs:attributeGroup" XML element.
 */
sealed abstract class AttributeGroupDefinitionOrReference private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.AttributeGroupDefinitionOrReference {

  assert(resolvedName == model.XsAttributeGroupEName, "The element must be an 'attributeGroup' element")
}

/**
 * Attribute group definition. That is, the "xs:attributeGroup" XML element.
 */
final class AttributeGroupDefinition private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends AttributeGroupDefinitionOrReference(bridgeElem, childElems, externalTnsOption) with HasName with model.AttributeGroupDefinition {

  assert(attributeOption(model.RefEName).isEmpty, "Must not be a reference")
  assert(attributeOption(model.NameEName).isDefined, "Must have a name")

  def internalTargetNamespaceOption: Option[String] = XsdElem.getSchemaRoot(bridgeElem).attributeOption(model.TargetNamespaceEName)

  def targetNamespaceOption: Option[String] = internalTargetNamespaceOption.orElse(externalTnsOption)
}

/**
 * Attribute group reference. That is, the "xs:attributeGroup" XML element.
 */
final class AttributeGroupReference private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends AttributeGroupDefinitionOrReference(bridgeElem, childElems, externalTnsOption) with IsReference with model.AttributeGroupReference {

  assert(attributeOption(model.RefEName).isDefined, "Must be a reference")
  assert(attributeOption(model.NameEName).isEmpty, "Must not have a name")
}

/**
 * Identity constraint definition. That is, the "xs:key", "xs:keyref" or "xs:unique" XML element.
 */
sealed abstract class IdentityConstraintDefinition private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.IdentityConstraintDefinition {

  assert(
    Set(model.XsKeyEName, model.XsKeyrefEName, model.XsUniqueEName).contains(resolvedName),
    "The element must be an 'key', 'keyref' or 'unique' element")
}

/**
 * Identity constraint definition "xs:key".
 */
final class KeyConstraint private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends IdentityConstraintDefinition(bridgeElem, childElems, externalTnsOption) with model.KeyConstraint {

  assert(resolvedName == model.XsKeyEName, "The element must be a 'key' element")
}

/**
 * Identity constraint definition "xs:keyref".
 */
final class KeyrefConstraint private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends IdentityConstraintDefinition(bridgeElem, childElems, externalTnsOption) with model.KeyrefConstraint {

  assert(resolvedName == model.XsKeyrefEName, "The element must be a 'keyref' element")
}

/**
 * Identity constraint definition "xs:unique".
 */
final class UniqueConstraint private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends IdentityConstraintDefinition(bridgeElem, childElems, externalTnsOption) with model.UniqueConstraint {

  assert(resolvedName == model.XsUniqueEName, "The element must be a 'unique' element")
}

/**
 * Model group definition. That is, the "xs:group" XML element introducing a named model group.
 */
sealed abstract class ModelGroupDefinitionOrReference private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.ModelGroupDefinitionOrReference {

  assert(resolvedName == model.XsGroupEName, "The element must be a 'group' element")
}

/**
 * Model group definition. That is, the "xs:group" XML element introducing a named model group.
 */
final class ModelGroupDefinition private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends ModelGroupDefinitionOrReference(bridgeElem, childElems, externalTnsOption) with model.ModelGroupDefinition {

  assert(attributeOption(model.RefEName).isEmpty, "The element must have no 'ref' attribute")
}

/**
 * Model group reference. That is, the "xs:group" XML element referring to a named model group.
 */
final class ModelGroupReference private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends ModelGroupDefinitionOrReference(bridgeElem, childElems, externalTnsOption)
  with Particle with IsReference with model.ModelGroupReference {

  assert(attributeOption(model.RefEName).isDefined, "The element must have a 'ref' attribute")
}

/**
 * Notation declaration. That is, the "xs:notation" XML element.
 */
final class NotationDeclaration private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.NotationDeclaration {

  assert(resolvedName == model.XsNotationEName, "The element must be a 'notation' element")
}

/**
 * Model group. That is, the "xs:all", "xs:sequence" or "xs:choice" XML element.
 */
sealed abstract class ModelGroup private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with Particle with model.ModelGroup {

  assert(
    Set(model.XsAllEName, model.XsSequenceEName, model.XsChoiceEName).contains(bridgeElem.resolvedName),
    "The element must be an 'all', 'sequence' or 'choice' element")

  final def inNamedGroup: Boolean = {
    require(bridgeElem.path.parentPathOption.isDefined, s"Expected parent path of the model group")
    val parentPath = bridgeElem.path.parentPath

    val parent = bridgeElem.rootElem.getElemOrSelfByPath(parentPath)
    (parent.resolvedName == model.XsGroupEName) && ((parent \@ model.NameEName).isDefined)
  }
}

/**
 * Model group "all".
 */
final class AllGroup private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends ModelGroup(bridgeElem, childElems, externalTnsOption) with model.AllGroup {

  assert(resolvedName == model.XsAllEName, "The element must be an 'all' element")
}

/**
 * Model group "choice".
 */
final class ChoiceGroup private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends ModelGroup(bridgeElem, childElems, externalTnsOption) with model.ChoiceGroup {

  assert(resolvedName == model.XsChoiceEName, "The element must be a 'choice' element")
}

/**
 * Model group "sequence".
 */
final class SequenceGroup private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends ModelGroup(bridgeElem, childElems, externalTnsOption) with model.SequenceGroup {

  assert(resolvedName == model.XsSequenceEName, "The element must be a 'sequence' element")
}

/**
 * Wildcard "xs:any".
 */
final class AnyWildcard private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with Particle with model.AnyWildcard {

  assert(resolvedName == model.XsAnyEName, "The element must be an 'any' element")
}

/**
 * Wildcard "xs:anyAttribute".
 */
final class AnyAttributeWildcard private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.AnyAttributeWildcard {

  assert(resolvedName == model.XsAnyAttributeEName, "The element must be an 'anyAttribute' element")
}

/**
 * Annotation schema component. That is, the "xs:annotation" XML element.
 */
final class Annotation private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.Annotation {

  assert(resolvedName == model.XsAnnotationEName, "The element must be an 'annotation' element")
}

// Import, include, redefine

/**
 * The "xs:import" XML element.
 */
final class Import private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.Import {

  assert(resolvedName == model.XsImportEName, "The element must be an 'import' element")
}

/**
 * The "xs:include" XML element.
 */
final class Include private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.Include {

  assert(resolvedName == model.XsIncludeEName, "The element must be an 'include' element")
}

/**
 * The "xs:redefine" XML element.
 */
final class Redefine private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.Redefine {

  assert(resolvedName == model.XsRedefineEName, "The element must be a 'redefine' element")
}

// Other schema parts, that are not Schema Components themselves, such as extension, restriction, etc.

/**
 * The "xs:complexContent" XML element.
 */
final class ComplexContent private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.ComplexContent {

  assert(resolvedName == model.XsComplexContentEName, "The element must be a 'complexContent' element")
}

/**
 * The "xs:simpleContent" XML element.
 */
final class SimpleContent private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.SimpleContent {

  assert(resolvedName == model.XsSimpleContentEName, "The element must be a 'simpleContent' element")
}

/**
 * The "xs:extension" XML element.
 */
final class Extension private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.Extension {

  assert(resolvedName == model.XsExtensionEName, "The element must be an 'extension' element")
}

/**
 * The "xs:restriction" XML element.
 */
final class Restriction private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.Restriction {

  assert(resolvedName == model.XsRestrictionEName, "The element must be a 'restriction' element")
}

/**
 * The "xs:field" XML element.
 */
final class Field private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.Field {

  assert(resolvedName == model.XsFieldEName, "The element must be a 'field' element")
}

/**
 * The "xs:selector" XML element.
 */
final class Selector private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.Selector {

  assert(resolvedName == model.XsSelectorEName, "The element must be a 'selector' element")
}

/**
 * The "xs:appinfo" XML element.
 */
final class Appinfo private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.Appinfo {

  assert(resolvedName == model.XsAppinfoEName, "The element must be an 'appinfo' element")
}

/**
 * The "xs:documentation" XML element.
 */
final class Documentation private[bridged] (
  bridgeElem: IndexedBridgeElem,
  childElems: immutable.IndexedSeq[XsdElem],
  externalTnsOption: Option[String]) extends XsdElem(bridgeElem, childElems, externalTnsOption) with model.Documentation {

  assert(resolvedName == model.XsDocumentationEName, "The element must be a 'documentation' element")
}

// Companion objects

object SchemaRootElem {

  /**
   * Public factory method for SchemaRootElem instances. It returns `XsdElem(bridgeElem)` as a `SchemaRootElem`.
   *
   * This is an expensive method, but once a `SchemaRootElem` has been created, querying through the `ElemLike` API is very fast.
   */
  def apply(elem: IndexedBridgeElem, externalTnsOption: Option[String]): SchemaRootElem = {
    require(elem.resolvedName == model.XsSchemaEName, s"Expected ${model.XsSchemaEName} but got ${elem.resolvedName}")

    val childElems = elem.findAllChildElems.map(e => XsdElem.apply(e, externalTnsOption))
    new SchemaRootElem(elem, childElems, externalTnsOption)
  }
}

object XsdElem {

  // Capabilities

  trait CanBeAbstract extends model.XsdElem.CanBeAbstract { self: XsdElem =>

    /**
     * Returns true if and only if the element declaration is abstract.
     * Only global element declarations can be abstract.
     */
    final def isAbstract: Boolean = abstractOption(bridgeElem) == Some(true)

    final def abstractOption(elem: IndexedBridgeElem): Option[Boolean] = {
      try {
        self.attributeOption(model.AbstractEName) map (_.toBoolean)
      } catch {
        case e: Exception => None
      }
    }
  }

  trait HasName extends model.XsdElem.HasName { self: XsdElem =>

    def externalTnsOption: Option[String]

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
    final def nameAttribute: String = (self \@ model.NameEName).getOrElse(sys.error("Expected @name"))
  }

  trait IsReference extends model.XsdElem.IsReference { self: XsdElem =>

    /**
     * Returns the value of the 'ref' attribute as expanded name.
     */
    final def ref: EName =
      self.attributeAsResolvedQNameOption(model.RefEName).getOrElse(sys.error("Attribute references must have a ref attribute"))
  }

  /**
   * Recursive public factory method for XsdElem instances. Indeed, construction of an XsdElem is expensive,
   * but after construction querying is very fast, due to the stored child XsdElems.
   *
   * Almost no validations are performed, because this XML Schema model is very lenient.
   */
  def apply(elem: IndexedBridgeElem, externalTnsOption: Option[String]): XsdElem = {
    // Recursive calls
    val childElems = elem.findAllChildElems.map(e => XsdElem.apply(e, externalTnsOption))

    // Pattern match with common patterns first (the @switch optimization is not possible)

    elem.resolvedName match {
      case model.XsSchemaEName =>
        new SchemaRootElem(elem, childElems, externalTnsOption)
      case model.XsElementEName =>
        if (isSchemaRootChild(elem)) {
          require(attributeOption(elem, model.NameEName).isDefined, s"No @name found in ${elem.resolvedName}")
          new GlobalElementDeclaration(elem, childElems, externalTnsOption)
        } else if (attributeOption(elem, model.RefEName).isDefined) {
          require(attributeOption(elem, model.NameEName).isEmpty, s"No @name allowed in ${elem.resolvedName}")
          new ElementReference(elem, childElems, externalTnsOption)
        } else if (attributeOption(elem, model.NameEName).isDefined) {
          require(attributeOption(elem, model.RefEName).isEmpty, s"No @ref allowed in ${elem.resolvedName}")
          new LocalElementDeclaration(elem, childElems, externalTnsOption)
        } else {
          sys.error(s"Not an element declaration or reference")
        }
      case model.XsAttributeEName =>
        if (isSchemaRootChild(elem)) {
          require(attributeOption(elem, model.NameEName).isDefined, s"No @name found in ${elem.resolvedName}")
          new GlobalAttributeDeclaration(elem, childElems, externalTnsOption)
        } else if (attributeOption(elem, model.RefEName).isDefined) {
          require(attributeOption(elem, model.NameEName).isEmpty, s"No @name allowed in ${elem.resolvedName}")
          new AttributeReference(elem, childElems, externalTnsOption)
        } else if (attributeOption(elem, model.NameEName).isDefined) {
          require(attributeOption(elem, model.RefEName).isEmpty, s"No @ref allowed in ${elem.resolvedName}")
          new LocalAttributeDeclaration(elem, childElems, externalTnsOption)
        } else {
          sys.error(s"Not an attribute declaration or reference")
        }
      case model.XsSimpleTypeEName =>
        if (isSchemaRootChild(elem)) {
          require(attributeOption(elem, model.NameEName).isDefined, s"No @name found in ${elem.resolvedName}")
          new NamedSimpleTypeDefinition(elem, childElems, externalTnsOption)
        } else {
          require(attributeOption(elem, model.NameEName).isEmpty, s"No @name allowed in ${elem.resolvedName}")
          new AnonymousSimpleTypeDefinition(elem, childElems, externalTnsOption)
        }
      case model.XsComplexTypeEName =>
        if (isSchemaRootChild(elem)) {
          require(attributeOption(elem, model.NameEName).isDefined, s"No @name found in ${elem.resolvedName}")
          new NamedComplexTypeDefinition(elem, childElems, externalTnsOption)
        } else {
          require(attributeOption(elem, model.NameEName).isEmpty, s"No @name allowed in ${elem.resolvedName}")
          new AnonymousComplexTypeDefinition(elem, childElems, externalTnsOption)
        }
      case model.XsAttributeGroupEName =>
        if (attributeOption(elem, model.RefEName).isDefined) {
          require(attributeOption(elem, model.NameEName).isEmpty, s"No @name allowed in ${elem.resolvedName}")
          new AttributeGroupReference(elem, childElems, externalTnsOption)
        } else if (attributeOption(elem, model.NameEName).isDefined) {
          require(attributeOption(elem, model.RefEName).isEmpty, s"No @ref allowed in ${elem.resolvedName}")
          new AttributeGroupDefinition(elem, childElems, externalTnsOption)
        } else {
          sys.error(s"Not an attribute group definition or reference")
        }
      case model.XsGroupEName =>
        if (attributeOption(elem, model.RefEName).isDefined) {
          new ModelGroupReference(elem, childElems, externalTnsOption)
        } else {
          new ModelGroupDefinition(elem, childElems, externalTnsOption)
        }
      case model.XsAllEName =>
        new AllGroup(elem, childElems, externalTnsOption)
      case model.XsSequenceEName =>
        new SequenceGroup(elem, childElems, externalTnsOption)
      case model.XsChoiceEName =>
        new ChoiceGroup(elem, childElems, externalTnsOption)
      case model.XsAnnotationEName =>
        new Annotation(elem, childElems, externalTnsOption)
      case model.XsImportEName =>
        new Import(elem, childElems, externalTnsOption)
      case model.XsIncludeEName =>
        new Include(elem, childElems, externalTnsOption)
      case model.XsComplexContentEName =>
        new ComplexContent(elem, childElems, externalTnsOption)
      case model.XsSimpleContentEName =>
        new SimpleContent(elem, childElems, externalTnsOption)
      case model.XsAppinfoEName =>
        new Appinfo(elem, childElems, externalTnsOption)
      case model.XsDocumentationEName =>
        new Documentation(elem, childElems, externalTnsOption)
      case model.XsExtensionEName =>
        new Extension(elem, childElems, externalTnsOption)
      case model.XsRestrictionEName =>
        new Restriction(elem, childElems, externalTnsOption)
      case model.XsKeyEName =>
        new KeyConstraint(elem, childElems, externalTnsOption)
      case model.XsKeyrefEName =>
        new KeyrefConstraint(elem, childElems, externalTnsOption)
      case model.XsUniqueEName =>
        new UniqueConstraint(elem, childElems, externalTnsOption)
      case model.XsNotationEName =>
        new NotationDeclaration(elem, childElems, externalTnsOption)
      case model.XsAnyEName =>
        new AnyWildcard(elem, childElems, externalTnsOption)
      case model.XsAnyAttributeEName =>
        new AnyAttributeWildcard(elem, childElems, externalTnsOption)
      case model.XsRedefineEName =>
        new Redefine(elem, childElems, externalTnsOption)
      case model.XsFieldEName =>
        new Field(elem, childElems, externalTnsOption)
      case model.XsSelectorEName =>
        new Selector(elem, childElems, externalTnsOption)
      case _ =>
        new XsdElem(elem, childElems, externalTnsOption)
    }
  }

  def isSchemaRootChild(e: IndexedBridgeElem): Boolean = {
    val p = e.path

    // Short-circuit for efficiency
    p.entries.size == 1 || {
      (p.entries.size >= 2) && (p.parentPath.lastEntry.elementName == model.XsSchemaEName)
    }
  }

  def getSchemaRoot(e: IndexedBridgeElem): e.UnwrappedBackingElem = {
    val rootElem = e.rootElem

    if (rootElem.resolvedName == model.XsSchemaEName) rootElem
    else {
      val schemaRootPath =
        e.path.findAncestorPath(_.lastEntryOption.map(_.elementName).getOrElse(model.XsSchemaEName) == model.XsSchemaEName).getOrElse(
          sys.error(s"Missing schema root element"))
      rootElem.getElemOrSelfByPath(schemaRootPath)
    }
  }

  private def attributeOption(e: IndexedBridgeElem, ename: EName): Option[String] =
    e.resolvedAttributes.toMap.filterKeys(Set(ename)).map(_._2).headOption
}

object Particle {

  val Unbounded = -1
}
