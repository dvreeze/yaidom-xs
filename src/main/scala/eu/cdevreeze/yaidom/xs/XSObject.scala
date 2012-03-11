package eu.cdevreeze.yaidom
package xs

/** Base object of the schema model */
trait XSObject extends Immutable {

  def nameOption: Option[String]

  def targetNamespaceOption: Option[String]

  // def toElem: Elem

  // final override def toString: String = toElem.toString
}
