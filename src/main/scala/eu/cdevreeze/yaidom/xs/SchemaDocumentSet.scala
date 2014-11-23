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

package eu.cdevreeze.yaidom.xs

import java.net.URI

import scala.Vector
import scala.collection.immutable
import scala.reflect.classTag

import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.core.QName

/**
 * Immutable collection of XML Schema Documents that belong together. Typically, a `SchemaDocumentSet` is a collection
 * of `SchemaDocument` instances that is closed under imports and includes.
 *
 * This class contains methods for obtaining type definitions, substitution group heads, etc. Such methods often require
 * the traversal of multiple schema documents, and are therefore part of this class.
 *
 * Per component type (element declaration, attribute declaration, type definition etc.) their ENames (in combination with scope)
 * must be unique in the SchemaDocumentSet. This is indeed required for "schemas" (as combination of schema documents).
 *
 * TODO Make this a trait, and make different subclasses/traits for "raw" schema document sets, schema document sets that
 * cache all found substitution groups, etc.
 *
 * @author Chris de Vreeze
 */
final class SchemaDocumentSet(val schemaDocuments: immutable.IndexedSeq[SchemaDocument]) extends Immutable {

  val schemaDocumentsByUri: Map[URI, SchemaDocument] = {
    val result = schemaDocuments map { doc => (doc.uri -> doc) }
    result.toMap
  }

  // TODO Query methods for types of element declarations, substitution groups of element declarations, etc.

  /**
   * Returns all element declarations in this SchemaDocumentSet.
   */
  final def findAllElementDeclarationOrReferences: immutable.IndexedSeq[ElementDeclarationOrReference] =
    schemaDocuments flatMap { e => e.schema.findAllElemsOfType(classTag[ElementDeclarationOrReference]) }

  /**
   * Returns all global element declarations in this SchemaDocumentSet.
   */
  final def findAllGlobalElementDeclarations: immutable.IndexedSeq[GlobalElementDeclaration] =
    schemaDocuments flatMap { e => e.schema.findAllGlobalElementDeclarations }

  /**
   * Returns all element declarations in this SchemaDocumentSet obeying the given predicate.
   */
  final def filterElementDeclarationOrReferences(p: ElementDeclarationOrReference => Boolean): immutable.IndexedSeq[ElementDeclarationOrReference] =
    schemaDocuments flatMap { e => e.schema.filterElemsOfType(classTag[ElementDeclarationOrReference])(p) }

  /**
   * Returns all global element declarations in this SchemaDocumentSet obeying the given predicate.
   */
  final def filterGlobalElementDeclarations(p: GlobalElementDeclaration => Boolean): immutable.IndexedSeq[GlobalElementDeclaration] =
    schemaDocuments flatMap { e => e.schema filterGlobalElementDeclarations p }

  /**
   * Finds the global element declaration with the given EName, if any, wrapped in an Option.
   *
   * The implementation is inefficient in that the target namespace (at root level) of the schema documents is not
   * taken into account.
   */
  final def findGlobalElementDeclarationByEName(ename: EName): Option[GlobalElementDeclaration] =
    filterGlobalElementDeclarations(_.targetEName == ename).headOption

  /**
   * Returns the substitution group "ancestry". The result always starts with the passed substitution group EName.
   *
   * This is an expensive method.
   */
  final def findSubstitutionGroupAncestry(substGroup: EName): immutable.IndexedSeq[EName] = {
    val substGroupDeclOption = findGlobalElementDeclarationByEName(substGroup)

    val ancestors: immutable.IndexedSeq[EName] = {
      val nextSubstGroupOption =
        for {
          substGroupDecl <- substGroupDeclOption
          attrValue <- substGroupDeclOption.get \@ EName("substitutionGroup")
          qname = QName(attrValue)
          ename <- substGroupDeclOption.get.scope.resolveQNameOption(qname)
        } yield ename

      if (nextSubstGroupOption.isEmpty) Vector()
      else {
        // Recursive (non-tail-recursive) call
        findSubstitutionGroupAncestry(nextSubstGroupOption.get)
      }
    }

    val result = substGroup +: ancestors

    assert(result.headOption == Some(substGroup))
    result
  }

  /**
   * Returns all global element declarations that have a substitution group matching the given predicate on the
   * substitution group.
   *
   * This is an expensive method.
   */
  final def findAllDirectSubstitutables(p: EName => Boolean): immutable.IndexedSeq[GlobalElementDeclaration] = {
    filterGlobalElementDeclarations { elemDecl => elemDecl.substitutionGroupOption.isDefined && p(elemDecl.substitutionGroupOption.get) }
  }
}