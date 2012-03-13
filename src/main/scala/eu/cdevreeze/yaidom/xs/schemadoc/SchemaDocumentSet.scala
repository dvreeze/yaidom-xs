package eu.cdevreeze.yaidom
package xs
package schemadoc

import java.net.URI

/**
 * A schema document set, closed under imports and includes.
 */
final class SchemaDocumentSet(val schemaDocuments: Map[URI, SchemaDocumentElem]) extends Immutable {

  require(schemaDocuments ne null)

  // TODO Make sure the document set is closed under imports and includes.
}
