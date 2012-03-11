package eu.cdevreeze.yaidom

/**
 * This package contains the immutable somewhat Scala-ish XML Schema model.
 *
 * The classes in this package do not retain the original `Elem` tree, so information may be lost when
 * converting the schema models back to `Elem`s. In particular, non-schema attributes are lost. As a consequence,
 * the schema models in this package by themselves are not complete enough for use in an XBRL context, to represent
 * XBRL taxonomy schemas.
 *
 * Probably we need support for SCDs (schema component desgnators) in this API.
 */
package object xs
