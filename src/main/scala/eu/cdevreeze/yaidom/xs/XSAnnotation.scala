package eu.cdevreeze.yaidom
package xs

final class XSAnnotation(val wrappedElem: Elem) extends XSObject {

  require(wrappedElem ne null)

  override def targetNamespaceOption: Option[String] = None

  override def nameOption: Option[String] = None
}
