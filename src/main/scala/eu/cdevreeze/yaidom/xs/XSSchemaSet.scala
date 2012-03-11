package eu.cdevreeze.yaidom
package xs

/** Collection of schemas. Inspired by XSOM. It contains a Map from target namespaces to `XSSchema` instances. */
final class XSSchemaSet(val schemas: Map[String, XSSchema]) extends Immutable {

  require(schemas ne null)
}
