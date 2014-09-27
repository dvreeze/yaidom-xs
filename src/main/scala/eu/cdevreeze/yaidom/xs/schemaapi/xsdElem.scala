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
 * @tparam E The concrete "XsdElem" super-type of all types in the concrete type hierarchy
 *
 * @author Chris de Vreeze
 */
trait XsdElem[E <: XsdElem[E]] extends ElemApi[E] with SubtypeAwareParentElemApi[E] { self: E =>

  type GlobalElementDeclarationType <: E with GlobalElementDeclaration[E]

  type GlobalAttributeDeclarationType <: E with GlobalAttributeDeclaration[E]
}

/**
 * XML Schema (from one document). That is, the "xs:schema" XML element.
 *
 * This is what the XML Schema specification calls a schema document, or the document element thereof.
 * In the abstract schema model of the specification, a schema is represented by one or more of these "schema documents".
 */
trait SchemaRootElem[E <: XsdElem[E]] extends XsdElem[E] { self: E =>

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
  def findAllImports: immutable.IndexedSeq[Import[E]]

  /**
   * Returns all includes.
   */
  def findAllIncludes: immutable.IndexedSeq[Include[E]]
}

// Schema Components

/**
 * Particle, having a min and max occurs (possibly default).
 */
trait Particle[E <: XsdElem[E]] extends XsdElem[E] { self: E =>

  def minOccurs: Int

  def maxOccurs: Int
}

/**
 * Element declaration or element reference. That is, the "xs:element" XML element.
 */
trait ElementDeclarationOrReference[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

/**
 * Element declaration. An element declaration is either a global or local element declaration.
 */
trait ElementDeclaration[E <: XsdElem[E]] extends ElementDeclarationOrReference[E] with HasName { self: E =>

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
trait GlobalElementDeclaration[E <: XsdElem[E]] extends ElementDeclaration[E] with CanBeAbstract { self: E =>

  def targetNamespaceOption: Option[String]

  /**
   * Returns the value of the 'substitutionGroup' attribute as expanded name, if any, wrapped in an Option.
   */
  def substitutionGroupOption: Option[EName]
}

/**
 * Local element declaration.
 */
trait LocalElementDeclaration[E <: XsdElem[E]] extends ElementDeclaration[E] with Particle[E] { self: E =>

  def targetNamespaceOption: Option[String]
}

/**
 * Element reference. Strictly it is not an element declaration, but it can be considered an element declaration in that
 * it is represented by the same xs:element XML element.
 */
trait ElementReference[E <: XsdElem[E]] extends ElementDeclarationOrReference[E] with Particle[E] with IsReference { self: E =>
}

/**
 * Attribute declaration or attribute reference. That is, the "xs:attribute" XML element.
 */
trait AttributeDeclarationOrReference[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

/**
 * Attribute declaration. An attribute declaration is either a global or local attribute declaration.
 */
trait AttributeDeclaration[E <: XsdElem[E]] extends AttributeDeclarationOrReference[E] with HasName { self: E =>

  /**
   * Returns the value of the 'type' attribute as expanded name, if any, wrapped in an Option.
   */
  def typeAttributeOption: Option[EName]
}

/**
 * Global attribute declaration.
 */
trait GlobalAttributeDeclaration[E <: XsdElem[E]] extends AttributeDeclaration[E] { self: E =>

  def targetNamespaceOption: Option[String]
}

/**
 * Local attribute declaration.
 */
trait LocalAttributeDeclaration[E <: XsdElem[E]] extends AttributeDeclaration[E] { self: E =>

  def targetNamespaceOption: Option[String]
}

/**
 * Attribute reference. Strictly it is not an attribute declaration, but it can be considered an attribute declaration in that
 * it is represented by the same xs:attribute XML element.
 */
trait AttributeReference[E <: XsdElem[E]] extends AttributeDeclarationOrReference[E] with IsReference { self: E =>
}

/**
 * Schema type definition, which is either a simple type or a complex type.
 */
trait TypeDefinition[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

trait NamedTypeDefinition[E <: XsdElem[E]] extends TypeDefinition[E] with HasName { self: E =>
}

trait AnonymousTypeDefinition[E <: XsdElem[E]] extends TypeDefinition[E] { self: E =>
}

/**
 * Simple type definition. That is, the "xs:simpleType" XML element.
 */
trait SimpleTypeDefinition[E <: XsdElem[E]] extends TypeDefinition[E] { self: E =>
}

/**
 * Named simple type definition. That is, the "xs:simpleType" XML element.
 */
trait NamedSimpleTypeDefinition[E <: XsdElem[E]] extends SimpleTypeDefinition[E] with NamedTypeDefinition[E] { self: E =>

  def targetNamespaceOption: Option[String]
}

/**
 * Anonymous simple type definition. That is, the "xs:simpleType" XML element.
 */
trait AnonymousSimpleTypeDefinition[E <: XsdElem[E]] extends SimpleTypeDefinition[E] with AnonymousTypeDefinition[E] { self: E =>
}

/**
 * Complex type definition. That is, the "xs:complexType" XML element.
 */
trait ComplexTypeDefinition[E <: XsdElem[E]] extends TypeDefinition[E] { self: E =>
}

/**
 * Named complex type definition. That is, the "xs:complexType" XML element.
 */
trait NamedComplexTypeDefinition[E <: XsdElem[E]] extends ComplexTypeDefinition[E] with NamedTypeDefinition[E] { self: E =>

  def targetNamespaceOption: Option[String]
}

/**
 * Anonymous complex type definition. That is, the "xs:complexType" XML element.
 */
trait AnonymousComplexTypeDefinition[E <: XsdElem[E]] extends ComplexTypeDefinition[E] with AnonymousTypeDefinition[E] { self: E =>
}

/**
 * Attribute group definition or reference. That is, the "xs:attributeGroup" XML element.
 */
trait AttributeGroupDefinitionOrReference[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

/**
 * Attribute group definition. That is, the "xs:attributeGroup" XML element.
 */
trait AttributeGroupDefinition[E <: XsdElem[E]] extends AttributeGroupDefinitionOrReference[E] with HasName { self: E =>

  def targetNamespaceOption: Option[String]
}

/**
 * Attribute group reference. That is, the "xs:attributeGroup" XML element.
 */
trait AttributeGroupReference[E <: XsdElem[E]] extends AttributeGroupDefinitionOrReference[E] with IsReference { self: E =>
}

/**
 * Identity constraint definition. That is, the "xs:key", "xs:keyref" or "xs:unique" XML element.
 */
trait IdentityConstraintDefinition[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

/**
 * Identity constraint definition "xs:key".
 */
trait KeyConstraint[E <: XsdElem[E]] extends IdentityConstraintDefinition[E] { self: E =>
}

/**
 * Identity constraint definition "xs:keyref".
 */
trait KeyrefConstraint[E <: XsdElem[E]] extends IdentityConstraintDefinition[E] { self: E =>
}

/**
 * Identity constraint definition "xs:unique".
 */
trait UniqueConstraint[E <: XsdElem[E]] extends IdentityConstraintDefinition[E] { self: E =>
}

/**
 * Model group definition. That is, the "xs:group" XML element introducing a named model group.
 */
trait ModelGroupDefinitionOrReference[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

/**
 * Model group definition. That is, the "xs:group" XML element introducing a named model group.
 */
trait ModelGroupDefinition[E <: XsdElem[E]] extends ModelGroupDefinitionOrReference[E] { self: E =>
}

/**
 * Model group reference. That is, the "xs:group" XML element referring to a named model group.
 */
trait ModelGroupReference[E <: XsdElem[E]] extends ModelGroupDefinitionOrReference[E] with Particle[E] with IsReference { self: E =>
}

/**
 * Notation declaration. That is, the "xs:notation" XML element.
 */
trait NotationDeclaration[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

/**
 * Model group. That is, the "xs:all", "xs:sequence" or "xs:choice" XML element.
 */
trait ModelGroup[E <: XsdElem[E]] extends XsdElem[E] with Particle[E] { self: E =>
}

/**
 * Model group "all".
 */
trait AllGroup[E <: XsdElem[E]] extends ModelGroup[E] { self: E =>
}

/**
 * Model group "choice".
 */
trait ChoiceGroup[E <: XsdElem[E]] extends ModelGroup[E] { self: E =>
}

/**
 * Model group "sequence".
 */
trait SequenceGroup[E <: XsdElem[E]] extends ModelGroup[E] { self: E =>
}

/**
 * Wildcard "xs:any".
 */
trait AnyWildcard[E <: XsdElem[E]] extends XsdElem[E] with Particle[E] { self: E =>
}

/**
 * Wildcard "xs:anyAttribute".
 */
trait AnyAttributeWildcard[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

/**
 * Annotation schema component. That is, the "xs:annotation" XML element.
 */
trait Annotation[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

// Import, include, redefine

/**
 * The "xs:import" XML element.
 */
trait Import[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

/**
 * The "xs:include" XML element.
 */
trait Include[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

/**
 * The "xs:redefine" XML element.
 */
trait Redefine[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

// Other schema parts, that are not Schema Components themselves, such as extension, restriction, etc.

/**
 * The "xs:complexContent" XML element.
 */
trait ComplexContent[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

/**
 * The "xs:simpleContent" XML element.
 */
trait SimpleContent[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

/**
 * The "xs:extension" XML element.
 */
trait Extension[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

/**
 * The "xs:restriction" XML element.
 */
trait Restriction[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

/**
 * The "xs:field" XML element.
 */
trait Field[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

/**
 * The "xs:selector" XML element.
 */
trait Selector[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

/**
 * The "xs:appinfo" XML element.
 */
trait Appinfo[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
}

/**
 * The "xs:documentation" XML element.
 */
trait Documentation[E <: XsdElem[E]] extends XsdElem[E] { self: E =>
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
