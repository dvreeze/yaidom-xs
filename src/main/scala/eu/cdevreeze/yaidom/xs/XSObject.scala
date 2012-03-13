package eu.cdevreeze.yaidom
package xs

/** Base object of the schema model. That is, an XML Schema component. */
trait XSObject extends Immutable {

  def nameOption: Option[String]

  def targetNamespaceOption: Option[String]
}
