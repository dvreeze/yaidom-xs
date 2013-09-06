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
 * correctly. The ancestry of an element is kept in much the same way as an `indexed.Elem` does in yaidom.
 *
 * @author Chris de Vreeze
 */
sealed class SchemaElem private[schema] (
  val indexedElem: indexed.Elem,
  val docUri: URI) extends ElemLike[SchemaElem] with HasText with Immutable {

  /**
   * The yaidom Elem itself, stored as a val
   */
  final val elem: Elem = indexedElem.elem

  require(indexedElem.rootElem.resolvedName == ENameSchema, "The root of the element tree must be a 'schema' element")
  require(
    (elem.resolvedName == ENameSchema) || (!indexedElem.elemPath.isRoot),
    "This element must either be a 'schema' element, or not be the root of the element tree")

  /**
   * Returns all child elements, in the correct order.
   *
   * These child elements share the same rootElem (and doc URI) with this element, but differ in the element paths, which have
   * one more "path entry".
   */
  final override def findAllChildElems: immutable.IndexedSeq[SchemaElem] = {
    // Remember: this function must be as fast as possible!
    val childElems = indexedElem.findAllChildElems
    childElems map { e => SchemaElem(e, docUri) }
  }

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

  final def idOption: Option[String] = elem \@ ENameId

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
final class SchemaRootElem(indexedElem: indexed.Elem, docUri: URI) extends SchemaElem(indexedElem, docUri) {
  require(elem.resolvedName == ENameSchema, "The element must be a 'schema' element")
  require(indexedElem.elemPath.isRoot, "The element must be the root of the element tree")

  final def targetNamespaceOption: Option[String] = elem \@ ENameTargetNamespace

  /**
   * Returns all global element declarations.
   */
  final def findAllGlobalElementDeclarations: immutable.IndexedSeq[GlobalElementDeclaration] =
    filterChildElems { e => e.resolvedName == ENameElement } collect { case e: GlobalElementDeclaration => e }

  /**
   * Returns all global element declarations obeying the given predicate.
   */
  final def filterGlobalElementDeclarations(p: GlobalElementDeclaration => Boolean): immutable.IndexedSeq[GlobalElementDeclaration] =
    filterChildElems { e => e.resolvedName == ENameElement } collect { case e: GlobalElementDeclaration if p(e) => e }

  /**
   * Finds the global element declaration with the given EName, if any, wrapped in an Option.
   */
  final def findGlobalElementDeclarationByEName(ename: EName): Option[GlobalElementDeclaration] =
    filterGlobalElementDeclarations(_.enameOption == Some(ename)).headOption

  /**
   * Returns all global attribute declarations.
   */
  final def findAllGlobalAttributeDeclarations: immutable.IndexedSeq[GlobalAttributeDeclaration] =
    filterChildElems { e => e.resolvedName == ENameAttribute } collect { case e: GlobalAttributeDeclaration => e }

  /**
   * Returns all global attribute declarations obeying the given predicate.
   */
  final def filterGlobalAttributeDeclarations(p: GlobalAttributeDeclaration => Boolean): immutable.IndexedSeq[GlobalAttributeDeclaration] =
    filterChildElems { e => e.resolvedName == ENameAttribute } collect { case e: GlobalAttributeDeclaration if p(e) => e }

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
    filterElems { e => e.resolvedName == ENameImport } collect { case e: Import => e }

  /**
   * Returns all includes.
   */
  final def findAllIncludes: immutable.IndexedSeq[Include] =
    filterElems { e => e.resolvedName == ENameInclude } collect { case e: Include => e }

  /**
   * Returns all redefines.
   */
  final def findAllRedefines: immutable.IndexedSeq[Redefine] =
    filterElems { e => e.resolvedName == ENameRedefine } collect { case e: Redefine => e }
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
abstract class SchemaComponent(indexedElem: indexed.Elem, docUri: URI) extends SchemaElem(indexedElem, docUri) {

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

  final def minOccursAttrOption: Option[String] = elem \@ ENameMinOccurs

  final def maxOccursAttrOption: Option[String] = elem \@ ENameMaxOccurs
}

/**
 * Element declaration or element reference. That is, the "xs:element" XML element.
 */
abstract class ElementDeclarationOrReference(indexedElem: indexed.Elem, docUri: URI) extends SchemaComponent(indexedElem, docUri) {
  require(elem.resolvedName == ENameElement, "The element must be an 'element' element")

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
  final def nameAttributeOption: Option[String] = elem \@ ENameName

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
    val typeAttrAttrOption = elem \@ ENameType
    typeAttrAttrOption map { tpe =>
      elem.scope.resolveQNameOption(QName(tpe)).getOrElse(
        sys.error("Could not resolve type '%s' as expanded name".format(tpe)))
    }
  }

  /**
   * Returns the value of the 'substitutionGroup' attribute as expanded name, if any, wrapped in an Option.
   */
  final def substitutionGroupOption: Option[EName] = {
    val substGroupAttrOption = elem \@ ENameSubstitutionGroup
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
      (elem \@ ENameAbstract) map (_.toBoolean)
    } catch {
      case e: Exception => None
    }
  }

  /**
   * Returns the value of the 'nillable' attribute, if any, wrapped in an Option.
   */
  final def nillableOption: Option[Boolean] = {
    try {
      (elem \@ ENameNillable) map (_.toBoolean)
    } catch {
      case e: Exception => None
    }
  }

  /**
   * Returns the value of the 'ref' attribute as expanded name, if any, wrapped in an Option.
   */
  final def refOption: Option[EName] = {
    val refOption = elem \@ ENameRef
    refOption map { ref =>
      elem.scope.resolveQNameOption(QName(ref)).getOrElse(
        sys.error("Could not resolve ref '%s' as expanded name".format(ref)))
    }
  }
}

/**
 * Element declaration. An element declaration is either a global or local element declaration.
 */
abstract class ElementDeclaration(indexedElem: indexed.Elem, docUri: URI) extends ElementDeclarationOrReference(indexedElem, docUri) {
  require(!isReference, "Must not be a reference")
}

/**
 * Global element declaration.
 */
final class GlobalElementDeclaration(indexedElem: indexed.Elem, docUri: URI) extends ElementDeclaration(indexedElem, docUri) {
  require(isGlobal, "Must be global")

  /**
   * Returns the target namespace, if any, wrapped in an Option. The target namespace of a global component is the target namespace
   * of the schema root element, if any.
   */
  final override def targetNamespaceOption: Option[String] = indexedElem.rootElem.attributeOption(ENameTargetNamespace)

  /**
   * Returns None as the non-existent scope, wrapped in an Option.
   */
  final override def scopeOption: Option[indexed.Elem] = None

  /**
   * Returns `enameOption.get`.
   */
  final def ename: EName = enameOption.getOrElse(sys.error("Global element declarations must have a name"))

  final def typeOption: Option[EName] = elem.attributeAsResolvedQNameOption(ENameType)
}

/**
 * Local element declaration.
 */
final class LocalElementDeclaration(indexedElem: indexed.Elem, docUri: URI) extends ElementDeclaration(indexedElem, docUri) with Particle {
  require(!isGlobal, "Must be local")

  /**
   * Returns the target namespace, if any, wrapped in an Option. The target namespace depends on the target namespace
   * of the schema root element, if any, and on the form and (schema root element) elementFormDefault attributes, if any.
   */
  final override def targetNamespaceOption: Option[String] = {
    val tnsOption = indexedElem.rootElem.attributeOption(ENameTargetNamespace)

    val formOption = elem.attributeOption(ENameForm)
    val elementFormDefaultOption = indexedElem.rootElem.attributeOption(ENameElementFormDefault)

    if (formOption == Some("qualified")) tnsOption
    else if (formOption.isEmpty && (elementFormDefaultOption == Some("qualified"))) tnsOption
    else None
  }

  /**
   * Returns the "scope", as a complex type definition, wrapped in an Option.
   */
  final override def scopeOption: Option[indexed.Elem] = {
    val complexTypeOption = indexedElem.elemPath findAncestorPath { p =>
      p.elementNameOption.getOrElse(ENameSchema) == ENameComplexType
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
final class ElementReference(indexedElem: indexed.Elem, docUri: URI) extends ElementDeclarationOrReference(indexedElem, docUri) with Particle {
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
abstract class AttributeDeclarationOrReference(indexedElem: indexed.Elem, docUri: URI) extends SchemaComponent(indexedElem, docUri) {
  require(elem.resolvedName == ENameAttribute, "The element must be an 'attribute' element")

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
  final def nameAttributeOption: Option[String] = elem \@ ENameName

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
    val typeAttrAttrOption = elem \@ ENameType
    typeAttrAttrOption map { tpe =>
      elem.scope.resolveQNameOption(QName(tpe)).getOrElse(
        sys.error("Could not resolve type '%s' as expanded name".format(tpe)))
    }
  }

  /**
   * Returns the value of the 'ref' attribute as expanded name, if any, wrapped in an Option.
   */
  final def refOption: Option[EName] = {
    val refOption = elem \@ ENameRef
    refOption map { ref =>
      elem.scope.resolveQNameOption(QName(ref)).getOrElse(
        sys.error("Could not resolve ref '%s' as expanded name".format(ref)))
    }
  }
}

/**
 * Attribute declaration. An attribute declaration is either a global or local attribute declaration.
 */
abstract class AttributeDeclaration(indexedElem: indexed.Elem, docUri: URI) extends AttributeDeclarationOrReference(indexedElem, docUri) {
  require(!isReference, "Must not be a reference")
}

/**
 * Global attribute declaration.
 */
final class GlobalAttributeDeclaration(indexedElem: indexed.Elem, docUri: URI) extends AttributeDeclaration(indexedElem, docUri) {
  require(isGlobal, "Must be global")

  /**
   * Returns the target namespace, if any, wrapped in an Option. The target namespace of a global component is the target namespace
   * of the schema root element, if any.
   */
  final override def targetNamespaceOption: Option[String] = indexedElem.rootElem.attributeOption(ENameTargetNamespace)

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
final class LocalAttributeDeclaration(indexedElem: indexed.Elem, docUri: URI) extends AttributeDeclaration(indexedElem, docUri) with AttributeUse {
  require(!isGlobal, "Must be local")

  /**
   * Returns the target namespace, if any, wrapped in an Option. The target namespace depends on the target namespace
   * of the schema root element, if any, and on the form and (schema root element) attributeFormDefault attributes, if any.
   */
  final override def targetNamespaceOption: Option[String] = {
    val tnsOption = indexedElem.rootElem.attributeOption(ENameTargetNamespace)

    val formOption = elem.attributeOption(ENameForm)
    val attributeFormDefaultOption = indexedElem.rootElem.attributeOption(ENameAttributeFormDefault)

    if (formOption == Some("qualified")) tnsOption
    else if (formOption.isEmpty && (attributeFormDefaultOption == Some("qualified"))) tnsOption
    else None
  }

  /**
   * Returns the "scope", as a complex type definition, wrapped in an Option.
   */
  final override def scopeOption: Option[indexed.Elem] = {
    val complexTypeOption = indexedElem.elemPath findAncestorPath { p =>
      p.elementNameOption.getOrElse(ENameSchema) == ENameComplexType
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
final class AttributeReference(indexedElem: indexed.Elem, docUri: URI) extends AttributeDeclarationOrReference(indexedElem, docUri) with AttributeUse {
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
abstract class TypeDefinition(indexedElem: indexed.Elem, docUri: URI) extends SchemaComponent(indexedElem, docUri) {

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
  final def nameAttributeOption: Option[String] = elem \@ ENameName
}

/**
 * Simple type definition. That is, the "xs:simpleType" XML element.
 */
final class SimpleTypeDefinition(indexedElem: indexed.Elem, docUri: URI) extends TypeDefinition(indexedElem, docUri) {
  require(elem.resolvedName == ENameSimpleType, "The element must be an 'simpleType' element")

  final override def targetNamespaceOption: Option[String] = {
    indexedElem.rootElem.attributeOption(ENameTargetNamespace)
  }
}

/**
 * Complex type definition. That is, the "xs:complexType" XML element.
 */
final class ComplexTypeDefinition(indexedElem: indexed.Elem, docUri: URI) extends TypeDefinition(indexedElem, docUri) {
  require(elem.resolvedName == ENameComplexType, "The element must be an 'complexType' element")

  final override def targetNamespaceOption: Option[String] = {
    indexedElem.rootElem.attributeOption(ENameTargetNamespace)
  }
}

/**
 * Attribute group definition. That is, the "xs:attributeGroup" XML element.
 */
final class AttributeGroupDefinition(indexedElem: indexed.Elem, docUri: URI) extends SchemaComponent(indexedElem, docUri) {
  require(elem.resolvedName == ENameAttributeGroup, "The element must be an 'attributeGroup' element")

  final override def targetNamespaceOption: Option[String] = {
    indexedElem.rootElem.attributeOption(ENameTargetNamespace)
  }
}

/**
 * Identity constraint definition. That is, the "xs:key", "xs:keyref" or "xs:unique" XML element.
 */
final class IdentityConstraintDefinition(indexedElem: indexed.Elem, docUri: URI) extends SchemaComponent(indexedElem, docUri) {
  require(
    Set(ENameKey, ENameKeyref, ENameUnique).contains(elem.resolvedName),
    "The element must be an 'key', 'keyref' or 'unique' element")

  final override def targetNamespaceOption: Option[String] = {
    indexedElem.rootElem.attributeOption(ENameTargetNamespace)
  }
}

/**
 * Model group definition. That is, the "xs:group" XML element.
 */
sealed class ModelGroupDefinition private[schema] (indexedElem: indexed.Elem, docUri: URI) extends SchemaComponent(indexedElem, docUri) {
  require(elem.resolvedName == ENameGroup, "The element must be a 'group' element")

  final override def targetNamespaceOption: Option[String] = {
    indexedElem.rootElem.attributeOption(ENameTargetNamespace)
  }

  /**
   * Returns the value of the 'ref' attribute as expanded name, if any, wrapped in an Option.
   */
  final def refOption: Option[EName] = {
    val refOption = elem \@ ENameRef
    refOption map { ref =>
      elem.scope.resolveQNameOption(QName(ref)).getOrElse(
        sys.error("Could not resolve ref '%s' as expanded name".format(ref)))
    }
  }
}

/**
 * Notation declaration. That is, the "xs:notation" XML element.
 */
final class NotationDeclaration(indexedElem: indexed.Elem, docUri: URI) extends SchemaComponent(indexedElem, docUri) {
  require(elem.resolvedName == ENameNotation, "The element must be a 'notation' element")

  final override def targetNamespaceOption: Option[String] = {
    indexedElem.rootElem.attributeOption(ENameTargetNamespace)
  }
}

/**
 * Model group. That is, the "xs:all", "xs:sequence" or "xs:choice" XML element.
 */
sealed class ModelGroup private[schema] (indexedElem: indexed.Elem, docUri: URI) extends SchemaComponent(indexedElem, docUri) {
  require(
    Set(ENameAll, ENameSequence, ENameChoice).contains(elem.resolvedName),
    "The element must be an 'all', 'sequence' or 'choice' element")

  final override def targetNamespaceOption: Option[String] = None

  final def inNamedGroup: Boolean = {
    assert(indexedElem.elemPath.parentPathOption.isDefined)
    val parentPath = indexedElem.elemPath.parentPath

    val parent = indexedElem.rootElem.getWithElemPath(parentPath)
    (parent.resolvedName == ENameGroup) && ((parent \@ ENameName).isDefined)
  }
}

/**
 * Wildcard. That is, the "xs:any" or "xs:anyAttribute" XML element.
 */
sealed class Wildcard private[schema] (indexedElem: indexed.Elem, docUri: URI) extends SchemaComponent(indexedElem, docUri) {
  require(
    Set(ENameAny, ENameAnyAttribute).contains(elem.resolvedName),
    "The element must be an 'any', 'anyAttribute' element")

  final override def targetNamespaceOption: Option[String] = None
}

/**
 * Annotation schema component. That is, the "xs:annotation" XML element.
 */
final class Annotation(indexedElem: indexed.Elem, docUri: URI) extends SchemaComponent(indexedElem, docUri) {
  require(elem.resolvedName == ENameAnnotation, "The element must be an 'annotation' element")

  final override def targetNamespaceOption: Option[String] = None
}

// Import, include, redefine

/**
 * The "xs:import" XML element.
 */
final class Import(indexedElem: indexed.Elem, docUri: URI) extends SchemaElem(indexedElem, docUri) {
  require(elem.resolvedName == ENameImport, "The element must be an 'import' element")
}

/**
 * The "xs:include" XML element.
 */
final class Include(indexedElem: indexed.Elem, docUri: URI) extends SchemaElem(indexedElem, docUri) {
  require(elem.resolvedName == ENameInclude, "The element must be an 'include' element")
}

/**
 * The "xs:redefine" XML element.
 */
final class Redefine(indexedElem: indexed.Elem, docUri: URI) extends SchemaElem(indexedElem, docUri) {
  require(elem.resolvedName == ENameRedefine, "The element must be a 'redefine' element")
}

// Other schema parts, that are not Schema Components themselves, such as extension, restriction, etc.

/**
 * The "xs:complexContent" XML element.
 */
final class ComplexContent(indexedElem: indexed.Elem, docUri: URI) extends SchemaElem(indexedElem, docUri) {
  require(elem.resolvedName == ENameComplexContent, "The element must be a 'complexContent' element")
}

/**
 * The "xs:simpleContent" XML element.
 */
final class SimpleContent(indexedElem: indexed.Elem, docUri: URI) extends SchemaElem(indexedElem, docUri) {
  require(elem.resolvedName == ENameSimpleContent, "The element must be a 'simpleContent' element")
}

/**
 * The "xs:extension" XML element.
 */
final class Extension(indexedElem: indexed.Elem, docUri: URI) extends SchemaElem(indexedElem, docUri) {
  require(elem.resolvedName == ENameExtension, "The element must be an 'extension' element")
}

/**
 * The "xs:restriction" XML element.
 */
final class Restriction(indexedElem: indexed.Elem, docUri: URI) extends SchemaElem(indexedElem, docUri) {
  require(elem.resolvedName == ENameRestriction, "The element must be a 'restriction' element")
}

/**
 * The "xs:appinfo" XML element.
 */
final class Appinfo(indexedElem: indexed.Elem, docUri: URI) extends SchemaElem(indexedElem, docUri) {
  require(elem.resolvedName == ENameAppinfo, "The element must be an 'appinfo' element")
}

/**
 * The "xs:documentation" XML element.
 */
final class Documentation(indexedElem: indexed.Elem, docUri: URI) extends SchemaElem(indexedElem, docUri) {
  require(elem.resolvedName == ENameDocumentation, "The element must be a 'documentation' element")
}

// Companion objects

object SchemaElem {

  def apply(indexedElem: indexed.Elem, docUri: URI): SchemaElem = {
    indexedElem.elemPath.elementNameOption.getOrElse(ENameSchema) match {
      case ENameSchema => new SchemaRootElem(indexedElem, docUri)
      case ENameElement if indexedElem.attributeOption(ENameRef).isDefined =>
        new ElementReference(indexedElem, docUri)
      case ENameElement if indexedElem.elemPath.entries.size == 1 =>
        new GlobalElementDeclaration(indexedElem, docUri)
      case ENameElement => new LocalElementDeclaration(indexedElem, docUri)
      case ENameAttribute if indexedElem.attributeOption(ENameRef).isDefined =>
        new AttributeReference(indexedElem, docUri)
      case ENameAttribute if indexedElem.elemPath.entries.size == 1 =>
        new GlobalAttributeDeclaration(indexedElem, docUri)
      case ENameAttribute => new LocalAttributeDeclaration(indexedElem, docUri)
      case ENameSimpleType => new SimpleTypeDefinition(indexedElem, docUri)
      case ENameComplexType => new ComplexTypeDefinition(indexedElem, docUri)
      case ENameAttributeGroup => new AttributeGroupDefinition(indexedElem, docUri)
      case ENameKey => new IdentityConstraintDefinition(indexedElem, docUri)
      case ENameKeyref => new IdentityConstraintDefinition(indexedElem, docUri)
      case ENameUnique => new IdentityConstraintDefinition(indexedElem, docUri)
      case ENameGroup if (indexedElem \@ ENameRef).isDefined =>
        new ModelGroupDefinition(indexedElem, docUri) with Particle
      case ENameGroup => new ModelGroupDefinition(indexedElem, docUri)
      case ENameAll if inNamedGroup(indexedElem) =>
        new ModelGroup(indexedElem, docUri)
      case ENameSequence if inNamedGroup(indexedElem) =>
        new ModelGroup(indexedElem, docUri)
      case ENameChoice if inNamedGroup(indexedElem) =>
        new ModelGroup(indexedElem, docUri)
      case ENameAll => new ModelGroup(indexedElem, docUri) with Particle
      case ENameSequence => new ModelGroup(indexedElem, docUri) with Particle
      case ENameChoice => new ModelGroup(indexedElem, docUri) with Particle
      case ENameNotation => new NotationDeclaration(indexedElem, docUri)
      case ENameAnnotation => new Annotation(indexedElem, docUri)
      case ENameAny => new Wildcard(indexedElem, docUri) with Particle
      case ENameAnyAttribute => new Wildcard(indexedElem, docUri)
      case ENameImport => new Import(indexedElem, docUri)
      case ENameInclude => new Include(indexedElem, docUri)
      case ENameRedefine => new Redefine(indexedElem, docUri)
      case ENameComplexContent => new ComplexContent(indexedElem, docUri)
      case ENameSimpleContent => new SimpleContent(indexedElem, docUri)
      case ENameAppinfo => new Appinfo(indexedElem, docUri)
      case ENameDocumentation => new Documentation(indexedElem, docUri)
      case ENameExtension => new Extension(indexedElem, docUri)
      case ENameRestriction => new Restriction(indexedElem, docUri)
      case _ => new SchemaElem(indexedElem, docUri)
    }
  }

  private def inNamedGroup(indexedElem: indexed.Elem): Boolean = {
    assert(indexedElem.elemPath.parentPathOption.isDefined)
    val parent = indexedElem.rootElem.getWithElemPath(indexedElem.elemPath.parentPath)

    (parent.resolvedName == ENameGroup) && ((parent \@ ENameName).isDefined)
  }
}

object Particle {

  val Unbounded = -1
}
