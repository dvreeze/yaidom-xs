package eu.cdevreeze.yaidom
package xs

/**
 * A type definition. Either a definition of a simple type or a definition of a complex type.
 *
 * Type definitions are "primary" XML Schema components, along with element declarations and attribute declarations.
 */
trait XSTypeDefinition extends XSObject {

  def baseTypeOption: Option[XSTypeDefinition]

  def finalFor: Set[DerivationMethod]
}
