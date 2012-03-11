package eu.cdevreeze.yaidom
package xs

trait XSTypeDefinition extends XSObject {

  def baseTypeOption: Option[XSTypeDefinition]

  def finalFor: Set[DerivationMethod]
}
