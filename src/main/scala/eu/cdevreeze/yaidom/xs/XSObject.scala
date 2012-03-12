package eu.cdevreeze.yaidom
package xs

/** Base object of the schema model. That is, an XML Schema component. */
trait XSObject extends Immutable {

  def nameOption: Option[String]

  def targetNamespaceOption: Option[String]

  /**
   * Returns the `Elem` representation of this schema component itself, wrapped in an Option, depending on its context
   * (`ElemPath` against the given root, which must be a schema element).
   */
  def toElemOption(root: Elem)(ownElemPath: ElemPath): Option[Elem] = sys.error("Not yet implemented")
}
