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
  val docUri: URI,
  val wrappedElem: indexed.Elem,
  val allChildElems: immutable.IndexedSeq[SchemaObject]) extends ElemLike[SchemaObject] with HasText with Immutable {

  require(docUri ne null)
  require(wrappedElem ne null)
  require(allChildElems ne null)

  require(wrappedElem.findAllChildElems == allChildElems.map(_.wrappedElem), "Inconsistent SchemaObject")

  require(allChildElems forall (e => e.docUri == this.docUri), s"All child elements must have the same document URI $docUri")

  require(wrappedElem.rootElem.resolvedName == ENameSchema, "The root of the element tree must be a 'schema' element")
  require(
    (wrappedElem.resolvedName == ENameSchema) || (!wrappedElem.elemPath.isRoot),
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
   * Returns the optional URI of this element, containing the id attribute value as URI fragment, if any.
   * If the id attribute is absent, None is returned.
   */
  final def uriOption: Option[URI] = {
    val idOption = wrappedElem.attributeOption(ENameId)
    idOption map { id => new URI(docUri.getScheme, docUri.getSchemeSpecificPart, id) }
  }

  /**
   * Returns all element declarations inside this SchemaObject (excluding self).
   */
  final def findAllElementDeclarationOrReferences: immutable.IndexedSeq[ElementDeclarationOrReference] =
    this filterElems { e => e.resolvedName == ENameElement } collect { case e: ElementDeclarationOrReference => e }

  /**
   * Returns all element declarations obeying the given predicate (excluding self).
   */
  final def filterElementDeclarationOrReferences(p: ElementDeclarationOrReference => Boolean): immutable.IndexedSeq[ElementDeclarationOrReference] =
    this filterElems { e =>
      e.resolvedName == ENameElement && p(e.asInstanceOf[ElementDeclarationOrReference])
    } collect { case e: ElementDeclarationOrReference => e }

  /**
   * Returns all topmost element declarations inside this SchemaObject (excluding self) obeying the given predicate.
   * Note that "topmost" is not the same as "global" (which only makes sense for the Schema object).
   */
  final def findTopmostElementDeclarationOrReferences(p: ElementDeclarationOrReference => Boolean): immutable.IndexedSeq[ElementDeclarationOrReference] =
    this findTopmostElems { e =>
      e.resolvedName == ENameElement && p(e.asInstanceOf[ElementDeclarationOrReference])
    } collect { case e: ElementDeclarationOrReference => e }

  /**
   * Returns all attribute declarations inside this SchemaObject (excluding self).
   */
  final def findAllAttributeDeclarationOrReferences: immutable.IndexedSeq[AttributeDeclarationOrReference] =
    this filterElems { e => e.resolvedName == ENameAttribute } collect { case e: AttributeDeclarationOrReference => e }

  /**
   * Returns all attribute declarations obeying the given predicate (excluding self).
   */
  final def filterAttributeDeclarationOrReferences(p: AttributeDeclarationOrReference => Boolean): immutable.IndexedSeq[AttributeDeclarationOrReference] =
    this filterElems { e =>
      e.resolvedName == ENameAttribute && p(e.asInstanceOf[AttributeDeclarationOrReference])
    } collect { case e: AttributeDeclarationOrReference => e }

  /**
   * Returns all topmost attribute declarations inside this SchemaObject (excluding self) obeying the given predicate.
   * Note that "topmost" is not the same as "global" (which only makes sense for the Schema object).
   */
  final def findTopmostAttributeDeclarationOrReferences(p: AttributeDeclarationOrReference => Boolean): immutable.IndexedSeq[AttributeDeclarationOrReference] =
    this findTopmostElems { e =>
      e.resolvedName == ENameAttribute && p(e.asInstanceOf[AttributeDeclarationOrReference])
    } collect { case e: AttributeDeclarationOrReference => e }

  /**
   * Returns all type definitions inside this SchemaObject (excluding self).
   */
  final def findAllTypeDefinitions: immutable.IndexedSeq[TypeDefinition] =
    this filterElems { e =>
      Set(ENameComplexType, ENameSimpleType).contains(e.resolvedName)
    } collect { case e: TypeDefinition => e }

  /**
   * Returns all type declarations obeying the given predicate (excluding self).
   */
  final def filterTypeDefinitions(p: TypeDefinition => Boolean): immutable.IndexedSeq[TypeDefinition] =
    this filterElems { e =>
      Set(ENameComplexType, ENameSimpleType).contains(e.resolvedName) && p(e.asInstanceOf[TypeDefinition])
    } collect { case e: TypeDefinition => e }

  /**
   * Returns all topmost type declarations inside this SchemaObject (excluding self) obeying the given predicate.
   * Note that "topmost" is not the same as "global" (which only makes sense for the Schema object).
   */
  final def findTopmostTypeDefinitions(p: TypeDefinition => Boolean): immutable.IndexedSeq[TypeDefinition] =
    this findTopmostElems { e =>
      Set(ENameComplexType, ENameSimpleType).contains(e.resolvedName) && p(e.asInstanceOf[TypeDefinition])
    } collect { case e: TypeDefinition => e }
}

/**
 * XML Schema (from one document). That is, the "xs:schema" XML element.
 *
 * This is what the XML Schema specification calls a schema document, or the document element thereof.
 * In the abstract schema model of the specification, a schema is represented by one or more of these "schema documents".
 */
final class Schema private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkSchemaElem(wrappedElem)

  final def targetNamespaceOption: Option[String] = wrappedElem \@ ENameTargetNamespace

  /**
   * Returns all global element declarations.
   */
  final def findAllGlobalElementDeclarations: immutable.IndexedSeq[GlobalElementDeclaration] =
    findTopmostElementDeclarationOrReferences { e => e.isGlobal } collect { case e: GlobalElementDeclaration => e }

  /**
   * Returns all global element declarations obeying the given predicate.
   */
  final def filterGlobalElementDeclarations(p: GlobalElementDeclaration => Boolean): immutable.IndexedSeq[GlobalElementDeclaration] =
    this filterElementDeclarationOrReferences { e => e.isGlobal && p(e.asInstanceOf[GlobalElementDeclaration]) } collect { case e: GlobalElementDeclaration => e }

  /**
   * Finds the global element declaration with the given EName, if any, wrapped in an Option.
   */
  final def findGlobalElementDeclarationByEName(ename: EName): Option[GlobalElementDeclaration] =
    filterGlobalElementDeclarations(_.enameOption == Some(ename)).headOption

  /**
   * Returns all global attribute declarations.
   */
  final def findAllGlobalAttributeDeclarations: immutable.IndexedSeq[GlobalAttributeDeclaration] =
    findTopmostAttributeDeclarationOrReferences { e => e.isGlobal } collect { case e: GlobalAttributeDeclaration => e }

  /**
   * Returns all global attribute declarations obeying the given predicate.
   */
  final def filterGlobalAttributeDeclarations(p: GlobalAttributeDeclaration => Boolean): immutable.IndexedSeq[GlobalAttributeDeclaration] =
    this filterAttributeDeclarationOrReferences { e => e.isGlobal && p(e.asInstanceOf[GlobalAttributeDeclaration]) } collect { case e: GlobalAttributeDeclaration => e }

  /**
   * Returns all global element declarations that have precisely the given substitution group.
   */
  final def findAllDirectSubstitutables(substGroup: EName): immutable.IndexedSeq[GlobalElementDeclaration] = {
    val substGroupOption = Some(substGroup)
    filterGlobalElementDeclarations { e => e.substitutionGroupOption == substGroupOption }
  }

  /**
   * Returns all global element declarations that have one of the given substitution groups.
   */
  final def findAllDirectSubstitutables(substGroups: Set[EName]): immutable.IndexedSeq[GlobalElementDeclaration] = {
    val substGroupOptions = substGroups map { sg => Option(sg) }
    filterGlobalElementDeclarations { e => substGroupOptions.contains(e.substitutionGroupOption) }
  }

  /**
   * Returns all imports.
   */
  final def findAllImports: immutable.IndexedSeq[Import] =
    this filterElems { e => e.resolvedName == ENameImport } collect { case e: Import => e }

  /**
   * Returns all includes.
   */
  final def findAllIncludes: immutable.IndexedSeq[Include] =
    this filterElems { e => e.resolvedName == ENameInclude } collect { case e: Include => e }

  /**
   * Returns all redefines.
   */
  final def findAllRedefines: immutable.IndexedSeq[Redefine] =
    this filterElems { e => e.resolvedName == ENameRedefine } collect { case e: Redefine => e }
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
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(docUri, wrappedElem, allChildElems) {

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
 * Element declaration or element reference. That is, the "xs:element" XML element.
 */
abstract class ElementDeclarationOrReference private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkElementDeclarationElem(wrappedElem)

  if (isGlobal) {
    SchemaObjects.checkNotAParticleElem(wrappedElem)
  } else {
    SchemaObjects.checkParticleElem(wrappedElem)
  }

  /**
   * Returns true if and only if the element declaration has the schema element as its parent.
   *
   * Element references are not considered global, because their parent is not the schema element, but they do always
   * refer to global element declarations.
   */
  final def isGlobal: Boolean = elemPath.entries.size == 1

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
  final def nameAttributeOption: Option[String] = this.wrappedElem \@ ENameName

  /**
   * Returns the "scope", if any, wrapped in an Option.
   *
   * That is, if this element declaration is not a reference, and has a complex type definition as ancestor, that complex
   * type definition is returned as indexed.Elem, wrapped in an Option. In all other cases, None is returned.
   */
  def scopeOption: Option[indexed.Elem]

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
 * Element declaration. An element declaration is either a global or local element declaration.
 */
abstract class ElementDeclaration private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends ElementDeclarationOrReference(docUri, wrappedElem, allChildElems) {

  require(!isReference, "Must not be a reference")
}

/**
 * Global element declaration.
 */
final class GlobalElementDeclaration private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends ElementDeclaration(docUri, wrappedElem, allChildElems) {

  require(isGlobal, "Must be global")

  /**
   * Returns the target namespace, if any, wrapped in an Option. The target namespace of a global component is the target namespace
   * of the schema root element, if any.
   */
  final override def targetNamespaceOption: Option[String] = this.rootElem.attributeOption(ENameTargetNamespace)

  /**
   * Returns None as the non-existent scope, wrapped in an Option.
   */
  final override def scopeOption: Option[indexed.Elem] = None

  /**
   * Returns `enameOption.get`.
   */
  final def ename: EName = enameOption.getOrElse(sys.error("Global element declarations must have a name"))
}

/**
 * Local element declaration.
 */
final class LocalElementDeclaration private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends ElementDeclaration(docUri, wrappedElem, allChildElems) with Particle {

  require(!isGlobal, "Must be local")

  /**
   * Returns the target namespace, if any, wrapped in an Option. The target namespace depends on the target namespace
   * of the schema root element, if any, and on the form and (schema root element) elementFormDefault attributes, if any.
   */
  final override def targetNamespaceOption: Option[String] = {
    val tnsOption = this.rootElem.attributeOption(ENameTargetNamespace)

    val formOption = this.wrappedElem.attributeOption(ENameForm)
    val elementFormDefaultOption = this.rootElem.attributeOption(ENameElementFormDefault)

    if (formOption == Some("qualified")) tnsOption
    else if (formOption.isEmpty && (elementFormDefaultOption == Some("qualified"))) tnsOption
    else None
  }

  /**
   * Returns the "scope", as a complex type definition, wrapped in an Option.
   */
  final override def scopeOption: Option[indexed.Elem] = {
    val complexTypeOption = this.wrappedElem findAncestor { e => e.resolvedName == ENameComplexType }
    complexTypeOption
  }

  /**
   * Returns `scopeOption.get`.
   */
  final def scope: indexed.Elem = scopeOption.getOrElse(sys.error("Local element declarations must have a scope"))

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
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends ElementDeclarationOrReference(docUri, wrappedElem, allChildElems) with Particle {

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

  def wrappedElem: indexed.Elem

  // TODO
}

/**
 * Attribute declaration or attribute reference. That is, the "xs:attribute" XML element.
 */
abstract class AttributeDeclarationOrReference private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkAttributeDeclarationElem(wrappedElem)

  if (isGlobal) {
    SchemaObjects.checkNotAnAttributeUseElem(wrappedElem)
  } else {
    SchemaObjects.checkAttributeUseElem(wrappedElem)
  }

  /**
   * Returns true if and only if the attribute declaration has the schema element as its parent.
   *
   * Attribute references are not considered global, because their parent is not the schema element, but they do always
   * refer to global attribute declarations.
   */
  final def isGlobal: Boolean = elemPath.entries.size == 1

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
  final def nameAttributeOption: Option[String] = this.wrappedElem \@ ENameName

  /**
   * Returns the "scope", if any, wrapped in an Option.
   *
   * That is, if this attribute declaration is not a reference, and has a complex type definition as ancestor, that complex
   * type definition is returned as indexed.Elem, wrapped in an Option. In all other cases, None is returned.
   */
  def scopeOption: Option[indexed.Elem]

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
 * Attribute declaration. An attribute declaration is either a global or local attribute declaration.
 */
abstract class AttributeDeclaration private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends AttributeDeclarationOrReference(docUri, wrappedElem, allChildElems) {

  require(!isReference, "Must not be a reference")
}

/**
 * Global attribute declaration.
 */
final class GlobalAttributeDeclaration private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends AttributeDeclaration(docUri, wrappedElem, allChildElems) {

  require(isGlobal, "Must be global")

  /**
   * Returns the target namespace, if any, wrapped in an Option. The target namespace of a global component is the target namespace
   * of the schema root element, if any.
   */
  final override def targetNamespaceOption: Option[String] = this.rootElem.attributeOption(ENameTargetNamespace)

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
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends AttributeDeclaration(docUri, wrappedElem, allChildElems) with AttributeUse {

  require(!isReference, "Must not be a reference")
  require(!isGlobal, "Must be local")

  /**
   * Returns the target namespace, if any, wrapped in an Option. The target namespace depends on the target namespace
   * of the schema root element, if any, and on the form and (schema root element) attributeFormDefault attributes, if any.
   */
  final override def targetNamespaceOption: Option[String] = {
    val tnsOption = this.rootElem.attributeOption(ENameTargetNamespace)

    val formOption = this.wrappedElem.attributeOption(ENameForm)
    val attributeFormDefaultOption = this.rootElem.attributeOption(ENameAttributeFormDefault)

    if (formOption == Some("qualified")) tnsOption
    else if (formOption.isEmpty && (attributeFormDefaultOption == Some("qualified"))) tnsOption
    else None
  }

  /**
   * Returns the "scope", as a complex type definition, wrapped in an Option.
   */
  final override def scopeOption: Option[indexed.Elem] = {
    val complexTypeOption = this.wrappedElem findAncestor { e => e.resolvedName == ENameComplexType }
    complexTypeOption
  }

  /**
   * Returns `scopeOption.get`.
   */
  final def scope: indexed.Elem = scopeOption.getOrElse(sys.error("Local attribute declarations must have a scope"))

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
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends AttributeDeclarationOrReference(docUri, wrappedElem, allChildElems) with AttributeUse {

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
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(docUri, wrappedElem, allChildElems) {

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
  final def nameAttributeOption: Option[String] = this.wrappedElem \@ ENameName
}

/**
 * Simple type definition. That is, the "xs:simpleType" XML element.
 */
final class SimpleTypeDefinition private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends TypeDefinition(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkSimpleTypeDefinitionElem(wrappedElem)

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(ENameTargetNamespace)
  }
}

/**
 * Complex type definition. That is, the "xs:complexType" XML element.
 */
final class ComplexTypeDefinition private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends TypeDefinition(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkComplexTypeDefinitionElem(wrappedElem)

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(ENameTargetNamespace)
  }
}

/**
 * Attribute group definition. That is, the "xs:attributeGroup" XML element.
 */
final class AttributeGroupDefinition private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkAttributeGroupDefinitionElem(wrappedElem)

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(ENameTargetNamespace)
  }
}

/**
 * Identity constraint definition. That is, the "xs:key", "xs:keyref" or "xs:unique" XML element.
 */
final class IdentityConstraintDefinition private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkIdentityConstraintDefinitionElem(wrappedElem)

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(ENameTargetNamespace)
  }
}

/**
 * Model group definition. That is, the "xs:group" XML element.
 */
class ModelGroupDefinition private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkModelGroupDefinitionElem(wrappedElem)

  if (refOption.isEmpty) {
    SchemaObjects.checkNotAParticleElem(wrappedElem)
  } else {
    SchemaObjects.checkParticleElem(wrappedElem)
  }

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(ENameTargetNamespace)
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
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkNotationDeclarationElem(wrappedElem)

  final override def targetNamespaceOption: Option[String] = {
    this.rootElem.attributeOption(ENameTargetNamespace)
  }
}

/**
 * Model group. That is, the "xs:all", "xs:sequence" or "xs:choice" XML element.
 */
class ModelGroup private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(docUri, wrappedElem, allChildElems) {

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

    (parent.resolvedName == ENameGroup) && ((parent \@ ENameName).isDefined)
  }
}

/**
 * Wildcard. That is, the "xs:any" or "xs:anyAttribute" XML element.
 */
class Wildcard private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkWildcardElem(wrappedElem)

  if (wrappedElem.resolvedName == ENameAnyAttribute) {
    SchemaObjects.checkNotAParticleElem(wrappedElem)
  } else {
    assert(wrappedElem.resolvedName == ENameAny)
    SchemaObjects.checkParticleElem(wrappedElem)
  }

  final override def targetNamespaceOption: Option[String] = None
}

/**
 * Annotation schema component. That is, the "xs:annotation" XML element.
 */
final class Annotation private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaComponent(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkAnnotationElem(wrappedElem)

  final override def targetNamespaceOption: Option[String] = None
}

// Import, include, redefine

/**
 * The "xs:import" XML element.
 */
final class Import private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkImportElem(wrappedElem)
}

/**
 * The "xs:include" XML element.
 */
final class Include private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkIncludeElem(wrappedElem)
}

/**
 * The "xs:redefine" XML element.
 */
final class Redefine private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkRedefineElem(wrappedElem)
}

// Other schema parts, that are not Schema Components themselves, such as extension, restriction, etc.

/**
 * The "xs:complexContent" XML element.
 */
final class ComplexContent private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkComplexContentElem(wrappedElem)
}

/**
 * The "xs:simpleContent" XML element.
 */
final class SimpleContent private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkSimpleContentElem(wrappedElem)
}

/**
 * The "xs:extension" XML element.
 */
final class Extension private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkExtensionElem(wrappedElem)
}

/**
 * The "xs:restriction" XML element.
 */
final class Restriction private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(docUri, wrappedElem, allChildElems) {

  SchemaObjects.checkRestrictionElem(wrappedElem)
}

/**
 * The "xs:appinfo" XML element.
 */
final class Appinfo private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(docUri, wrappedElem, allChildElems) {

  require(wrappedElem.resolvedName == ENameAppinfo, "The element must be an 'appinfo' element")
}

/**
 * The "xs:documentation" XML element.
 */
final class Documentation private[schema] (
  override val docUri: URI,
  override val wrappedElem: indexed.Elem,
  override val allChildElems: immutable.IndexedSeq[SchemaObject]) extends SchemaObject(docUri, wrappedElem, allChildElems) {

  require(wrappedElem.resolvedName == ENameDocumentation, "The element must be a 'documentation' element")
}

// Companion objects

object Schema {

  def apply(docUri: URI, elem: indexed.Elem): Schema =
    new Schema(docUri, elem, SchemaObject.childSchemaObjects(docUri, elem))
}

object ElementDeclarationOrReference {

  /**
   * Creates an `ElementDeclarationOrReference` from a URI and an `indexed.Elem`. If not global, the result is also a `Particle`.
   */
  def apply(docUri: URI, elem: indexed.Elem): ElementDeclarationOrReference = {
    def isGlobal: Boolean = elem.elemPath.entries.size == 1
    def isReference: Boolean = SchemaObjects.refOption(elem).isDefined

    if (isReference) new ElementReference(docUri, elem, childSchemaObjects(docUri, elem))
    else if (isGlobal) new GlobalElementDeclaration(docUri, elem, childSchemaObjects(docUri, elem))
    else new LocalElementDeclaration(docUri, elem, childSchemaObjects(docUri, elem))
  }
}

object ModelGroupDefinition {

  /**
   * Creates a `ModelGroupDefinition` from a URI and an `indexed.Elem`. If it has a "ref" attribute, the result is also a `Particle`.
   */
  def apply(docUri: URI, elem: indexed.Elem): ModelGroupDefinition = {
    if ((elem \@ ENameRef).isEmpty) new ModelGroupDefinition(docUri, elem, childSchemaObjects(docUri, elem))
    else new ModelGroupDefinition(docUri, elem, childSchemaObjects(docUri, elem)) with Particle
  }
}

object ModelGroup {

  /**
   * Creates a `ModelGroup` from a URI and an `indexed.Elem`. If not inside a named group (xs:group with "name" attribute),
   * the result is also a `Particle`.
   */
  def apply(docUri: URI, elem: indexed.Elem): ModelGroup = {
    if (inNamedGroup(elem)) new ModelGroup(docUri, elem, childSchemaObjects(docUri, elem))
    else new ModelGroup(docUri, elem, childSchemaObjects(docUri, elem)) with Particle
  }

  private def inNamedGroup(elem: indexed.Elem): Boolean = {
    assert(elem.parentOption.isDefined)
    val parent = elem.parent

    (parent.resolvedName == ENameGroup) && ((parent \@ ENameName).isDefined)
  }
}

object Wildcard {

  /**
   * Creates a `Wildcard` from a URI and an `indexed.Elem`. If it is an xs:any, the result is also a `Particle`.
   */
  def apply(docUri: URI, elem: indexed.Elem): Wildcard = {
    if (elem.resolvedName == ENameAnyAttribute) new Wildcard(docUri, elem, childSchemaObjects(docUri, elem))
    else new Wildcard(docUri, elem, childSchemaObjects(docUri, elem)) with Particle
  }
}

object AttributeDeclarationOrReference {

  /**
   * Creates an `AttributeDeclarationOrReference` from a URI and an `indexed.Elem`. If not global, the result is also an `AttributeUse`.
   */
  def apply(docUri: URI, elem: indexed.Elem): AttributeDeclarationOrReference = {
    def isGlobal: Boolean = elem.elemPath.entries.size == 1
    def isReference: Boolean = SchemaObjects.refOption(elem).isDefined

    if (isReference) new AttributeReference(docUri, elem, childSchemaObjects(docUri, elem))
    else if (isGlobal) new GlobalAttributeDeclaration(docUri, elem, childSchemaObjects(docUri, elem))
    else new LocalAttributeDeclaration(docUri, elem, childSchemaObjects(docUri, elem))
  }
}

object SchemaComponent {

  def apply(docUri: URI, elem: indexed.Elem): SchemaComponent = {
    wrapOption(docUri, elem).getOrElse(sys.error("%s is not a schema component".format(elem.resolvedName)))
  }

  def wrapOption(docUri: URI, elem: indexed.Elem): Option[SchemaComponent] = {
    import SchemaObject._

    elem match {
      case e if e.resolvedName == ENameElement => Some(ElementDeclarationOrReference(docUri, e))
      case e if e.resolvedName == ENameAttribute => Some(AttributeDeclarationOrReference(docUri, e))
      case e if e.resolvedName == ENameSimpleType => Some(new SimpleTypeDefinition(docUri, e, childSchemaObjects(docUri, e)))
      case e if e.resolvedName == ENameComplexType => Some(new ComplexTypeDefinition(docUri, e, childSchemaObjects(docUri, e)))
      case e if e.resolvedName == ENameAttributeGroup => Some(new AttributeGroupDefinition(docUri, e, childSchemaObjects(docUri, e)))
      case e if e.resolvedName == ENameKey => Some(new IdentityConstraintDefinition(docUri, e, childSchemaObjects(docUri, e)))
      case e if e.resolvedName == ENameKeyref => Some(new IdentityConstraintDefinition(docUri, e, childSchemaObjects(docUri, e)))
      case e if e.resolvedName == ENameUnique => Some(new IdentityConstraintDefinition(docUri, e, childSchemaObjects(docUri, e)))
      case e if e.resolvedName == ENameGroup => Some(ModelGroupDefinition(docUri, e))
      case e if e.resolvedName == ENameNotation => Some(new NotationDeclaration(docUri, e, childSchemaObjects(docUri, e)))
      case e if e.resolvedName == ENameAll => Some(ModelGroup(docUri, e))
      case e if e.resolvedName == ENameSequence => Some(ModelGroup(docUri, e))
      case e if e.resolvedName == ENameChoice => Some(ModelGroup(docUri, e))
      case e if e.resolvedName == ENameAny => Some(Wildcard(docUri, e))
      case e if e.resolvedName == ENameAnyAttribute => Some(Wildcard(docUri, e))
      case e if e.resolvedName == ENameAnnotation => Some(new Annotation(docUri, e, childSchemaObjects(docUri, e)))
      case e => None
    }
  }
}

object SchemaObject {

  def apply(docUri: URI, wrappedElem: indexed.Elem): SchemaObject = wrappedElem match {
    // TODO
    case e if e.resolvedName == ENameSchema => new Schema(docUri, wrappedElem, childSchemaObjects(docUri, wrappedElem))
    case e if Set(
      ENameElement,
      ENameAttribute,
      ENameSimpleType,
      ENameComplexType,
      ENameAttributeGroup,
      ENameKey,
      ENameKeyref,
      ENameUnique,
      ENameGroup,
      ENameNotation,
      ENameAll,
      ENameSequence,
      ENameChoice,
      ENameAny,
      ENameAnyAttribute,
      ENameAnnotation).contains(e.resolvedName) => SchemaComponent(docUri, wrappedElem)
    case e if e.resolvedName == ENameImport => new Import(docUri, wrappedElem, childSchemaObjects(docUri, wrappedElem))
    case e if e.resolvedName == ENameInclude => new Include(docUri, wrappedElem, childSchemaObjects(docUri, wrappedElem))
    case e if e.resolvedName == ENameRedefine => new Redefine(docUri, wrappedElem, childSchemaObjects(docUri, wrappedElem))
    case e if e.resolvedName == ENameComplexContent => new ComplexContent(docUri, wrappedElem, childSchemaObjects(docUri, wrappedElem))
    case e if e.resolvedName == ENameSimpleContent => new SimpleContent(docUri, wrappedElem, childSchemaObjects(docUri, wrappedElem))
    case e if e.resolvedName == ENameAppinfo => new Appinfo(docUri, wrappedElem, childSchemaObjects(docUri, wrappedElem))
    case e if e.resolvedName == ENameDocumentation => new Documentation(docUri, wrappedElem, childSchemaObjects(docUri, wrappedElem))
    case e if e.resolvedName == ENameExtension => new Extension(docUri, wrappedElem, childSchemaObjects(docUri, wrappedElem))
    case e if e.resolvedName == ENameRestriction => new Restriction(docUri, wrappedElem, childSchemaObjects(docUri, wrappedElem))
    case _ => new SchemaObject(docUri, wrappedElem, childSchemaObjects(docUri, wrappedElem)) {}
  }

  private[schema] def childSchemaObjects(docUri: URI, e: indexed.Elem): immutable.IndexedSeq[SchemaObject] =
    e.findAllChildElems.map(e => SchemaObject(docUri, e))
}

object Particle {

  val Unbounded = -1
}
