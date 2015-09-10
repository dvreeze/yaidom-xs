package eu.cdevreeze.yaidomxs.model

import scala.collection.immutable

import eu.cdevreeze.yaidom.core.EName

/**
 * Any XML element in an XSD document, as purely abstract traits.
 *
 * Terminology is taken as much as possible from the book Definitive XML Schema, 2nd Edition (by Priscilla Walmsley).
 */
trait XsdElem {

  type XSE <: XsdElem
  type GED <: GlobalElementDeclaration
  type GAD <: GlobalAttributeDeclaration
  type NTD <: NamedTypeDefinition
}

/**
 * An "xs:schema" XML element.
 *
 * This is what the XML Schema specification calls a schema document, or the document element thereof.
 * In the abstract schema model of the specification, a schema is represented by one or more of these "schema documents".
 *
 * This does not necessarily mean that this xs:schema element must be the root element of the XML document, although
 * that is the most likely scenario.
 */
trait SchemaRootElem extends XsdElem {

  def targetNamespaceOption: Option[String]

  /**
   * Returns all global element declarations mapped by EName.
   */
  def findAllGlobalElementDeclarationsMappedByEName: Map[EName, GED]

  /**
   * Returns all global element declarations.
   */
  def findAllGlobalElementDeclarations: immutable.IndexedSeq[GED]

  /**
   * Returns all global element declarations obeying the given predicate.
   */
  def filterGlobalElementDeclarations(p: GED => Boolean): immutable.IndexedSeq[GED]

  /**
   * Returns all global attribute declarations mapped by EName.
   */
  def findAllGlobalAttributeDeclarationsMappedByEName: Map[EName, GAD]

  /**
   * Returns all global attribute declarations.
   */
  def findAllGlobalAttributeDeclarations: immutable.IndexedSeq[GAD]

  /**
   * Returns all global attribute declarations obeying the given predicate.
   */
  def filterGlobalAttributeDeclarations(p: GAD => Boolean): immutable.IndexedSeq[GAD]

  /**
   * Returns all named type definitions mapped by EName.
   */
  def findAllNamedTypeDefinitionsMappedByEName: Map[EName, NTD]

  /**
   * Returns all named type definitions.
   */
  def findAllNamedTypeDefinitions: immutable.IndexedSeq[NTD]

  /**
   * Returns all named type definitions obeying the given predicate.
   */
  def filterNamedTypeDefinitions(p: NTD => Boolean): immutable.IndexedSeq[NTD]

  /**
   * Returns all global element declarations that have a substitution group matching the given predicate on the
   * substitution group.
   */
  def findAllDirectSubstitutables(p: EName => Boolean): immutable.IndexedSeq[GED]

  /**
   * Returns all imports.
   */
  def findAllImports: immutable.IndexedSeq[Import]

  /**
   * Returns all includes.
   */
  def findAllIncludes: immutable.IndexedSeq[Include]

  /**
   * Returns all redefines.
   */
  def findAllRedefines: immutable.IndexedSeq[Redefine]
}

// Schema Components

/**
 * Particle, having a min and max occurs (possibly default).
 */
trait Particle extends XsdElem {

  def minOccurs: Int

  def maxOccurs: Int

  def minOccursAttrOption: Option[String]

  def maxOccursAttrOption: Option[String]
}

/**
 * Element declaration or element reference. That is, the "xs:element" XML element.
 */
trait ElementDeclarationOrReference extends XsdElem

/**
 * Element declaration. An element declaration is either a global or local element declaration.
 */
trait ElementDeclaration extends ElementDeclarationOrReference with XsdElem.HasName {

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
trait GlobalElementDeclaration extends ElementDeclaration with XsdElem.CanBeAbstract {

  def targetNamespaceOption: Option[String]

  /**
   * Returns the value of the 'substitutionGroup' attribute as expanded name, if any, wrapped in an Option.
   */
  def substitutionGroupOption: Option[EName]
}

/**
 * Local element declaration.
 */
trait LocalElementDeclaration extends ElementDeclaration with Particle {

  def targetNamespaceOption: Option[String]
}

/**
 * Element reference. Strictly it is not an element declaration, but it can be considered an element declaration in that
 * it is represented by the same xs:element XML element.
 */
trait ElementReference extends ElementDeclarationOrReference with Particle with XsdElem.IsReference

/**
 * Attribute declaration or attribute reference. That is, the "xs:attribute" XML element.
 */
trait AttributeDeclarationOrReference extends XsdElem

/**
 * Attribute declaration. An attribute declaration is either a global or local attribute declaration.
 */
trait AttributeDeclaration extends AttributeDeclarationOrReference with XsdElem.HasName {

  /**
   * Returns the value of the 'type' attribute as expanded name, if any, wrapped in an Option.
   */
  def typeAttributeOption: Option[EName]
}

/**
 * Global attribute declaration.
 */
trait GlobalAttributeDeclaration extends AttributeDeclaration {

  def targetNamespaceOption: Option[String]
}

/**
 * Local attribute declaration.
 */
trait LocalAttributeDeclaration extends AttributeDeclaration {

  def targetNamespaceOption: Option[String]
}

/**
 * Attribute reference. Strictly it is not an attribute declaration, but it can be considered an attribute declaration in that
 * it is represented by the same xs:attribute XML element.
 */
trait AttributeReference extends AttributeDeclarationOrReference with XsdElem.IsReference

/**
 * Schema type definition, which is either a simple type or a complex type.
 */
trait TypeDefinition extends XsdElem

trait NamedTypeDefinition extends TypeDefinition with XsdElem.HasName

trait AnonymousTypeDefinition extends TypeDefinition

/**
 * Simple type definition. That is, the "xs:simpleType" XML element.
 */
trait SimpleTypeDefinition extends TypeDefinition

/**
 * Named simple type definition. That is, the "xs:simpleType" XML element.
 */
trait NamedSimpleTypeDefinition extends SimpleTypeDefinition with NamedTypeDefinition {

  def targetNamespaceOption: Option[String]
}

/**
 * Anonymous simple type definition. That is, the "xs:simpleType" XML element.
 */
trait AnonymousSimpleTypeDefinition extends SimpleTypeDefinition with AnonymousTypeDefinition

/**
 * Complex type definition. That is, the "xs:complexType" XML element.
 */
trait ComplexTypeDefinition extends TypeDefinition

/**
 * Named complex type definition. That is, the "xs:complexType" XML element.
 */
trait NamedComplexTypeDefinition extends ComplexTypeDefinition with NamedTypeDefinition {

  def targetNamespaceOption: Option[String]
}

/**
 * Anonymous complex type definition. That is, the "xs:complexType" XML element.
 */
trait AnonymousComplexTypeDefinition extends ComplexTypeDefinition with AnonymousTypeDefinition

/**
 * Attribute group definition or reference. That is, the "xs:attributeGroup" XML element.
 */
trait AttributeGroupDefinitionOrReference extends XsdElem

/**
 * Attribute group definition. That is, the "xs:attributeGroup" XML element.
 */
trait AttributeGroupDefinition extends AttributeGroupDefinitionOrReference with XsdElem.HasName {

  def targetNamespaceOption: Option[String]
}

/**
 * Attribute group reference. That is, the "xs:attributeGroup" XML element.
 */
trait AttributeGroupReference extends AttributeGroupDefinitionOrReference with XsdElem.IsReference

/**
 * Identity constraint definition. That is, the "xs:key", "xs:keyref" or "xs:unique" XML element.
 */
trait IdentityConstraintDefinition extends XsdElem

/**
 * Identity constraint definition "xs:key".
 */
trait KeyConstraint extends IdentityConstraintDefinition

/**
 * Identity constraint definition "xs:keyref".
 */
trait KeyrefConstraint extends IdentityConstraintDefinition

/**
 * Identity constraint definition "xs:unique".
 */
trait UniqueConstraint extends IdentityConstraintDefinition

/**
 * Model group definition. That is, the "xs:group" XML element introducing a named model group.
 */
trait ModelGroupDefinitionOrReference extends XsdElem

/**
 * Model group definition. That is, the "xs:group" XML element introducing a named model group.
 */
trait ModelGroupDefinition extends ModelGroupDefinitionOrReference

/**
 * Model group reference. That is, the "xs:group" XML element referring to a named model group.
 */
trait ModelGroupReference extends ModelGroupDefinitionOrReference with Particle with XsdElem.IsReference

/**
 * Notation declaration. That is, the "xs:notation" XML element.
 */
trait NotationDeclaration extends XsdElem

/**
 * Model group. That is, the "xs:all", "xs:sequence" or "xs:choice" XML element.
 */
trait ModelGroup extends XsdElem with Particle {

  def inNamedGroup: Boolean
}

/**
 * Model group "all".
 */
trait AllGroup extends ModelGroup

/**
 * Model group "choice".
 */
trait ChoiceGroup extends ModelGroup

/**
 * Model group "sequence".
 */
trait SequenceGroup extends ModelGroup

/**
 * Wildcard "xs:any".
 */
trait AnyWildcard extends XsdElem with Particle

/**
 * Wildcard "xs:anyAttribute".
 */
trait AnyAttributeWildcard extends XsdElem

/**
 * Annotation schema component. That is, the "xs:annotation" XML element.
 */
trait Annotation extends XsdElem

// Import, include, redefine

/**
 * The "xs:import" XML element.
 */
trait Import extends XsdElem

/**
 * The "xs:include" XML element.
 */
trait Include extends XsdElem

/**
 * The "xs:redefine" XML element.
 */
trait Redefine extends XsdElem

// Other schema parts, that are not Schema Components themselves, such as extension, restriction, etc.

/**
 * The "xs:complexContent" XML element.
 */
trait ComplexContent extends XsdElem

/**
 * The "xs:simpleContent" XML element.
 */
trait SimpleContent extends XsdElem

/**
 * The "xs:extension" XML element.
 */
trait Extension extends XsdElem

/**
 * The "xs:restriction" XML element.
 */
trait Restriction extends XsdElem

/**
 * The "xs:field" XML element.
 */
trait Field extends XsdElem

/**
 * The "xs:selector" XML element.
 */
trait Selector extends XsdElem

/**
 * The "xs:appinfo" XML element.
 */
trait Appinfo extends XsdElem

/**
 * The "xs:documentation" XML element.
 */
trait Documentation extends XsdElem

object XsdElem {

  // Capabilities

  trait CanBeAbstract {

    /**
     * Returns true if and only if the element declaration is abstract.
     * Only global element declarations can be abstract.
     */
    def isAbstract: Boolean
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

  val Unbounded = java.lang.Integer.MAX_VALUE
}
