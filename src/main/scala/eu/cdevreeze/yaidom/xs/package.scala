package eu.cdevreeze.yaidom

/**
 * This package contains the immutable somewhat Scala-ish XML Schema model.
 *
 * This is the "abstract" XML Schema model, without any knowledge about the schema document files that contain the schema(s).
 *
 * In the conversion from Xerces XSObject instances, non-schema attributes are lost. As a consequence,
 * the schema models in this package by themselves are not complete enough for use in an XBRL context, to represent
 * XBRL taxonomy schemas.
 *
 * Maybe we need support for SCDs (schema component designators).
 *
 * This API should become useful for querying schema data by itself, but also for finding element and attribute definitions
 * for XML instance data, the associated types, substitution groups (and the associated inheritance).
 */
package object xs {

  val NsXmlSchema = "http://www.w3.org/2001/XMLSchema".ns
}
