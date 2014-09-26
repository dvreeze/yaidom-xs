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
package schemaapi

import java.net.URI
import scala.collection.immutable
import scala.reflect.classTag
import eu.cdevreeze.yaidom._
import eu.cdevreeze.yaidom.subtypeaware.SubtypeAwareParentElemApi
import XsdElem._

/**
 * XML Schema or a part thereof. These elements offer the `SubtypeAwareParentElemApi` API.
 *
 * Terminology is taken as much as possible from the book Definitive XML Schema, 2nd Edition (by Priscilla Walmsley).
 *
 * @tparam A The concrete "XsdElem" super-type of all types in the concrete type hierarchy
 *
 * @author Chris de Vreeze
 */
trait XsdElem[XsdElemType <: XsdElem[XsdElemType]] extends ElemApi[XsdElemType] with SubtypeAwareParentElemApi[XsdElemType] with HasText { self: XsdElemType =>

  type GlobalElementDeclarationType <: XsdElemType

  type GlobalAttributeDeclarationType <: XsdElemType
}

/**
 * XML Schema (from one document). That is, the "xs:schema" XML element.
 *
 * This is what the XML Schema specification calls a schema document, or the document element thereof.
 * In the abstract schema model of the specification, a schema is represented by one or more of these "schema documents".
 */
trait SchemaRootElem[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>

  def targetNamespaceOption: Option[String]

  /**
   * Returns all global element declarations.
   */
  def findAllGlobalElementDeclarations: immutable.IndexedSeq[GlobalElementDeclarationType]

  /**
   * Returns all global element declarations obeying the given predicate.
   */
  def filterGlobalElementDeclarations(p: GlobalElementDeclarationType => Boolean): immutable.IndexedSeq[GlobalElementDeclarationType]

  /**
   * Returns all global attribute declarations.
   */
  def findAllGlobalAttributeDeclarations: immutable.IndexedSeq[GlobalAttributeDeclarationType]

  /**
   * Returns all global attribute declarations obeying the given predicate.
   */
  def filterGlobalAttributeDeclarations(p: GlobalAttributeDeclarationType => Boolean): immutable.IndexedSeq[GlobalAttributeDeclarationType]

  /**
   * Returns all global element declarations that have a substitution group matching the given predicate on the
   * substitution group.
   */
  def findAllDirectSubstitutables(p: EName => Boolean): immutable.IndexedSeq[GlobalElementDeclarationType]

  /**
   * Returns all imports.
   */
  def findAllImports: immutable.IndexedSeq[Import[XsdElemType]]

  /**
   * Returns all includes.
   */
  def findAllIncludes: immutable.IndexedSeq[Include[XsdElemType]]
}

// Schema Components

/**
 * Particle, having a min and max occurs (possibly default).
 */
trait Particle[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>

  def minOccurs: Int

  def maxOccurs: Int
}

/**
 * Element declaration or element reference. That is, the "xs:element" XML element.
 */
trait ElementDeclarationOrReference[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

/**
 * Element declaration. An element declaration is either a global or local element declaration.
 */
trait ElementDeclaration[XsdElemType <: XsdElem[XsdElemType]] extends ElementDeclarationOrReference[XsdElemType] with HasName { self: XsdElemType =>

  /**
   * Returns the value of the 'type' attribute as expanded name, if any, wrapped in an Option.
   */
  def typeAttributeOption: Option[EName]

  /**
   * Returns the value of the 'nillable' attribute, if any, wrapped in an Option.
   */
  def nillableOption: Option[Boolean]
}

/**
 * Global element declaration.
 */
trait GlobalElementDeclaration[XsdElemType <: XsdElem[XsdElemType]] extends ElementDeclaration[XsdElemType] with CanBeAbstract { self: XsdElemType =>

  def targetNamespaceOption: Option[String]

  /**
   * Returns the value of the 'substitutionGroup' attribute as expanded name, if any, wrapped in an Option.
   */
  def substitutionGroupOption: Option[EName]
}

/**
 * Local element declaration.
 */
trait LocalElementDeclaration[XsdElemType <: XsdElem[XsdElemType]] extends ElementDeclaration[XsdElemType] with Particle[XsdElemType] { self: XsdElemType =>

  def targetNamespaceOption: Option[String]
}

/**
 * Element reference. Strictly it is not an element declaration, but it can be considered an element declaration in that
 * it is represented by the same xs:element XML element.
 */
trait ElementReference[XsdElemType <: XsdElem[XsdElemType]] extends ElementDeclarationOrReference[XsdElemType] with Particle[XsdElemType] with IsReference { self: XsdElemType =>
}

/**
 * Attribute declaration or attribute reference. That is, the "xs:attribute" XML element.
 */
trait AttributeDeclarationOrReference[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

/**
 * Attribute declaration. An attribute declaration is either a global or local attribute declaration.
 */
trait AttributeDeclaration[XsdElemType <: XsdElem[XsdElemType]] extends AttributeDeclarationOrReference[XsdElemType] with HasName { self: XsdElemType =>

  /**
   * Returns the value of the 'type' attribute as expanded name, if any, wrapped in an Option.
   */
  def typeAttributeOption: Option[EName]
}

/**
 * Global attribute declaration.
 */
trait GlobalAttributeDeclaration[XsdElemType <: XsdElem[XsdElemType]] extends AttributeDeclaration[XsdElemType] { self: XsdElemType =>

  def targetNamespaceOption: Option[String]
}

/**
 * Local attribute declaration.
 */
trait LocalAttributeDeclaration[XsdElemType <: XsdElem[XsdElemType]] extends AttributeDeclaration[XsdElemType] { self: XsdElemType =>

  def targetNamespaceOption: Option[String]
}

/**
 * Attribute reference. Strictly it is not an attribute declaration, but it can be considered an attribute declaration in that
 * it is represented by the same xs:attribute XML element.
 */
trait AttributeReference[XsdElemType <: XsdElem[XsdElemType]] extends AttributeDeclarationOrReference[XsdElemType] with IsReference { self: XsdElemType =>
}

/**
 * Schema type definition, which is either a simple type or a complex type.
 */
trait TypeDefinition[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

trait NamedTypeDefinition[XsdElemType <: XsdElem[XsdElemType]] extends TypeDefinition[XsdElemType] with HasName { self: XsdElemType =>
}

trait AnonymousTypeDefinition[XsdElemType <: XsdElem[XsdElemType]] extends TypeDefinition[XsdElemType] { self: XsdElemType =>
}

/**
 * Simple type definition. That is, the "xs:simpleType" XML element.
 */
trait SimpleTypeDefinition[XsdElemType <: XsdElem[XsdElemType]] extends TypeDefinition[XsdElemType] { self: XsdElemType =>
}

/**
 * Named simple type definition. That is, the "xs:simpleType" XML element.
 */
trait NamedSimpleTypeDefinition[XsdElemType <: XsdElem[XsdElemType]] extends SimpleTypeDefinition[XsdElemType] with NamedTypeDefinition[XsdElemType] { self: XsdElemType =>

  def targetNamespaceOption: Option[String]
}

/**
 * Anonymous simple type definition. That is, the "xs:simpleType" XML element.
 */
trait AnonymousSimpleTypeDefinition[XsdElemType <: XsdElem[XsdElemType]] extends SimpleTypeDefinition[XsdElemType] with AnonymousTypeDefinition[XsdElemType] { self: XsdElemType =>
}

/**
 * Complex type definition. That is, the "xs:complexType" XML element.
 */
trait ComplexTypeDefinition[XsdElemType <: XsdElem[XsdElemType]] extends TypeDefinition[XsdElemType] { self: XsdElemType =>
}

/**
 * Named complex type definition. That is, the "xs:complexType" XML element.
 */
trait NamedComplexTypeDefinition[XsdElemType <: XsdElem[XsdElemType]] extends ComplexTypeDefinition[XsdElemType] with NamedTypeDefinition[XsdElemType] { self: XsdElemType =>

  def targetNamespaceOption: Option[String]
}

/**
 * Anonymous complex type definition. That is, the "xs:complexType" XML element.
 */
trait AnonymousComplexTypeDefinition[XsdElemType <: XsdElem[XsdElemType]] extends ComplexTypeDefinition[XsdElemType] with AnonymousTypeDefinition[XsdElemType] { self: XsdElemType =>
}

/**
 * Attribute group definition or reference. That is, the "xs:attributeGroup" XML element.
 */
trait AttributeGroupDefinitionOrReference[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

/**
 * Attribute group definition. That is, the "xs:attributeGroup" XML element.
 */
trait AttributeGroupDefinition[XsdElemType <: XsdElem[XsdElemType]] extends AttributeGroupDefinitionOrReference[XsdElemType] with HasName { self: XsdElemType =>

  def targetNamespaceOption: Option[String]
}

/**
 * Attribute group reference. That is, the "xs:attributeGroup" XML element.
 */
trait AttributeGroupReference[XsdElemType <: XsdElem[XsdElemType]] extends AttributeGroupDefinitionOrReference[XsdElemType] with IsReference { self: XsdElemType =>
}

/**
 * Identity constraint definition. That is, the "xs:key", "xs:keyref" or "xs:unique" XML element.
 */
trait IdentityConstraintDefinition[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

/**
 * Identity constraint definition "xs:key".
 */
trait KeyConstraint[XsdElemType <: XsdElem[XsdElemType]] extends IdentityConstraintDefinition[XsdElemType] { self: XsdElemType =>
}

/**
 * Identity constraint definition "xs:keyref".
 */
trait KeyrefConstraint[XsdElemType <: XsdElem[XsdElemType]] extends IdentityConstraintDefinition[XsdElemType] { self: XsdElemType =>
}

/**
 * Identity constraint definition "xs:unique".
 */
trait UniqueConstraint[XsdElemType <: XsdElem[XsdElemType]] extends IdentityConstraintDefinition[XsdElemType] { self: XsdElemType =>
}

/**
 * Model group definition. That is, the "xs:group" XML element introducing a named model group.
 */
trait ModelGroupDefinitionOrReference[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

/**
 * Model group definition. That is, the "xs:group" XML element introducing a named model group.
 */
trait ModelGroupDefinition[XsdElemType <: XsdElem[XsdElemType]] extends ModelGroupDefinitionOrReference[XsdElemType] { self: XsdElemType =>
}

/**
 * Model group reference. That is, the "xs:group" XML element referring to a named model group.
 */
trait ModelGroupReference[XsdElemType <: XsdElem[XsdElemType]] extends ModelGroupDefinitionOrReference[XsdElemType] with Particle[XsdElemType] with IsReference { self: XsdElemType =>
}

/**
 * Notation declaration. That is, the "xs:notation" XML element.
 */
trait NotationDeclaration[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

/**
 * Model group. That is, the "xs:all", "xs:sequence" or "xs:choice" XML element.
 */
trait ModelGroup[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] with Particle[XsdElemType] { self: XsdElemType =>
}

/**
 * Model group "all".
 */
trait AllGroup[XsdElemType <: XsdElem[XsdElemType]] extends ModelGroup[XsdElemType] { self: XsdElemType =>
}

/**
 * Model group "choice".
 */
trait ChoiceGroup[XsdElemType <: XsdElem[XsdElemType]] extends ModelGroup[XsdElemType] { self: XsdElemType =>
}

/**
 * Model group "sequence".
 */
trait SequenceGroup[XsdElemType <: XsdElem[XsdElemType]] extends ModelGroup[XsdElemType] { self: XsdElemType =>
}

/**
 * Wildcard "xs:any".
 */
trait AnyWildcard[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] with Particle[XsdElemType] { self: XsdElemType =>
}

/**
 * Wildcard "xs:anyAttribute".
 */
trait AnyAttributeWildcard[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

/**
 * Annotation schema component. That is, the "xs:annotation" XML element.
 */
trait Annotation[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

// Import, include, redefine

/**
 * The "xs:import" XML element.
 */
trait Import[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

/**
 * The "xs:include" XML element.
 */
trait Include[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

/**
 * The "xs:redefine" XML element.
 */
trait Redefine[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

// Other schema parts, that are not Schema Components themselves, such as extension, restriction, etc.

/**
 * The "xs:complexContent" XML element.
 */
trait ComplexContent[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

/**
 * The "xs:simpleContent" XML element.
 */
trait SimpleContent[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

/**
 * The "xs:extension" XML element.
 */
trait Extension[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

/**
 * The "xs:restriction" XML element.
 */
trait Restriction[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

/**
 * The "xs:field" XML element.
 */
trait Field[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

/**
 * The "xs:selector" XML element.
 */
trait Selector[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

/**
 * The "xs:appinfo" XML element.
 */
trait Appinfo[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

/**
 * The "xs:documentation" XML element.
 */
trait Documentation[XsdElemType <: XsdElem[XsdElemType]] extends XsdElem[XsdElemType] { self: XsdElemType =>
}

// Companion objects

object XsdElem {

  // Capabilities

  trait CanBeAbstract {

    /**
     * Returns true if and only if the element declaration is abstract.
     * Only global element declarations can be abstract.
     */
    def isAbstract: Boolean

    def abstractOption(elem: Elem): Option[Boolean]
  }

  trait HasName {

    def targetNamespaceOption: Option[String]

    /**
     * Returns the `EName` by combining the target namespace and the value of the "name" attribute.
     */
    def targetEName: EName

    /**
     * Returns the value of the "name" attribute
     */
    def nameAttribute: String
  }

  trait IsReference {

    /**
     * Returns the value of the 'ref' attribute as expanded name.
     */
    def ref: EName
  }
}

object Particle {

  val Unbounded = -1
}
