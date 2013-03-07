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
import eu.cdevreeze.yaidom._

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

  // TODO Query methods for element declarations, types of element declarations, substitution groups of element declarations, etc.
}
