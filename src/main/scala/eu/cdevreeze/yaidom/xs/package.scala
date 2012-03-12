package eu.cdevreeze.yaidom

/**
 * This package contains the immutable somewhat Scala-ish XML Schema model.
 *
 * The classes in this package do not (yet!) retain the original `Elem` tree, so information may be lost when
 * converting the schema models back to `Elem`s. In particular, non-schema attributes are lost. As a consequence,
 * the schema models in this package by themselves are not complete enough for use in an XBRL context, to represent
 * XBRL taxonomy schemas.
 *
 * Probably we need support for SCDs (schema component desgnators) in this API.
 * 
 * This API should become useful for querying schema data by itself, but also for finding element and attribute definitions
 * for XML instance data, the associated types, substitution groups (and the associated inheritance). Later this API should
 * also keep foreign attributes (at least), so that the original `Elem` (or an "equivalent") can be reconstructed.
 */
package object xs
