/*
 * Copyright 2011 Chris de Vreeze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eu.cdevreeze.yaidom
package xs
package schema

import java.net.URI
import scala.collection.immutable

/**
 * Immutable collection of XML Schema Documents that belong together. Typically, a `SchemaDocumentSet` is a collection
 * of `SchemaDocument` instances that is closed under imports and includes.
 *
 * This class contains methods for obtaining type definitions, substitution group heads, etc. Such methods often require
 * the traversal of multiple schema documents, and are therefore part of this class.
 *
 * @author Chris de Vreeze
 */
final class SchemaDocumentSet(val schemaDocuments: immutable.IndexedSeq[SchemaDocument]) extends Immutable {
  require(schemaDocuments forall (doc => doc.wrappedDocument.baseUriOption.isDefined), "All schema documents must have a base URI")

  val schemaDocumentsByUri: Map[URI, SchemaDocument] = {
    val result = schemaDocuments map { doc => (doc.wrappedDocument.baseUriOption.get -> doc) }
    result.toMap
  }

  // TODO Query methods for types of element declarations, substitution groups of element declarations, etc.

  /**
   * Returns all element declarations in this SchemaDocumentSet.
   */
  final def findAllElementDeclarations: immutable.IndexedSeq[ElementDeclaration] =
    schemaDocuments flatMap { e => e.schema.findAllElementDeclarations }

  /**
   * Returns all top-level element declarations in this SchemaDocumentSet.
   */
  final def findAllTopLevelElementDeclarations: immutable.IndexedSeq[ElementDeclaration] =
    schemaDocuments flatMap { e => e.schema.findAllTopLevelElementDeclarations }

  /**
   * Returns all element declarations in this SchemaDocumentSet obeying the given predicate.
   */
  final def filterElementDeclarations(p: ElementDeclaration => Boolean): immutable.IndexedSeq[ElementDeclaration] =
    schemaDocuments flatMap { e => e.schema filterElementDeclarations p }

  /**
   * Returns all top-level element declarations in this SchemaDocumentSet obeying the given predicate.
   */
  final def filterTopLevelElementDeclarations(p: ElementDeclaration => Boolean): immutable.IndexedSeq[ElementDeclaration] =
    schemaDocuments flatMap { e => e.schema filterTopLevelElementDeclarations p }

  /**
   * Finds the top-level element declaration with the given EName, if any, wrapped in an Option.
   *
   * The implementation is inefficient in that the target namespace (at root level) of the schema documents is not
   * taken into account.
   */
  final def findTopLevelElementDeclarationByEName(ename: EName): Option[ElementDeclaration] =
    filterTopLevelElementDeclarations(_.enameOption == Some(ename)).headOption
}
