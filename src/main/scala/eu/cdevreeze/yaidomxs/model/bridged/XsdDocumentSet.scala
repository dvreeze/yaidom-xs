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

package eu.cdevreeze.yaidomxs.model.bridged

import java.net.URI

import scala.Vector
import scala.collection.immutable
import scala.reflect.classTag

import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.core.QName
import eu.cdevreeze.yaidomxs.model.SchemaApi

/**
 * Immutable collection of XML Schema Documents that belong together. Typically, an `XsdDocumentSet` is a collection
 * of `XsdDocument` instances that is closed under imports and includes.
 *
 * This class contains methods for obtaining type definitions, substitution group heads, etc. Such methods often require
 * the traversal of multiple schema documents, and are therefore part of this class.
 *
 * Per component type (element declaration, attribute declaration, type definition etc.) their ENames (in combination with scope)
 * must be unique in the XsdDocumentSet. This is indeed required for "schemas" (as combination of schema documents).
 *
 * TODO Make this a trait, and make different subclasses/traits for "raw" schema document sets, schema document sets that
 * cache all found substitution groups, etc.
 *
 * @author Chris de Vreeze
 */
final class XsdDocumentSet(val schemaDocuments: immutable.IndexedSeq[XsdDocument]) extends SchemaApi {

  type GlobalElemDecl = GlobalElementDeclaration
  type GlobalAttrDecl = GlobalAttributeDeclaration
  type NamedTypeDef = NamedTypeDefinition

  private val allGlobalElementDeclarationsMappedByEName: Map[EName, GlobalElementDeclaration] = {
    (schemaDocuments flatMap { doc =>
      doc.schemaRootElem.findAllChildElemsOfType(classTag[GlobalElementDeclaration]).map(e => (e.targetEName -> e))
    }).toMap
  }

  private val allGlobalAttributeDeclarationsMappedByEName: Map[EName, GlobalAttributeDeclaration] = {
    (schemaDocuments flatMap { doc =>
      doc.schemaRootElem.findAllChildElemsOfType(classTag[GlobalAttributeDeclaration]).map(e => (e.targetEName -> e))
    }).toMap
  }

  private val allNamedTypeDefinitionsMappedByEName: Map[EName, NamedTypeDefinition] = {
    (schemaDocuments flatMap { doc =>
      doc.schemaRootElem.findAllChildElemsOfType(classTag[NamedTypeDefinition]).map(e => (e.targetEName -> e))
    }).toMap
  }

  val schemaDocumentsByUri: Map[URI, XsdDocument] = {
    val result = schemaDocuments map { doc => (doc.uri -> doc) }
    result.toMap
  }

  // TODO Query methods for element declarations, substitution groups of element declarations, etc.

  final def findAllGlobalElementDeclarationsMappedByEName: Map[EName, GlobalElementDeclaration] =
    allGlobalElementDeclarationsMappedByEName

  final def findAllGlobalElementDeclarations: immutable.IndexedSeq[GlobalElementDeclaration] =
    schemaDocuments.flatMap(d => d.schemaRootElem.findAllChildElemsOfType(classTag[GlobalElementDeclaration]))

  final def filterGlobalElementDeclarations(p: GlobalElementDeclaration => Boolean): immutable.IndexedSeq[GlobalElementDeclaration] =
    schemaDocuments.flatMap(d => d.schemaRootElem.filterChildElemsOfType(classTag[GlobalElementDeclaration])(p))

  final def findAllGlobalAttributeDeclarationsMappedByEName: Map[EName, GlobalAttributeDeclaration] =
    allGlobalAttributeDeclarationsMappedByEName

  final def findAllGlobalAttributeDeclarations: immutable.IndexedSeq[GlobalAttributeDeclaration] =
    schemaDocuments.flatMap(d => d.schemaRootElem.findAllChildElemsOfType(classTag[GlobalAttributeDeclaration]))

  final def filterGlobalAttributeDeclarations(p: GlobalAttributeDeclaration => Boolean): immutable.IndexedSeq[GlobalAttributeDeclaration] =
    schemaDocuments.flatMap(d => d.schemaRootElem.filterChildElemsOfType(classTag[GlobalAttributeDeclaration])(p))

  final def findAllNamedTypeDefinitionsMappedByEName: Map[EName, NamedTypeDefinition] =
    allNamedTypeDefinitionsMappedByEName

  final def findAllNamedTypeDefinitions: immutable.IndexedSeq[NamedTypeDefinition] = {
    schemaDocuments.flatMap(d => d.schemaRootElem.findAllChildElemsOfType(classTag[NamedTypeDefinition]))
  }

  final def filterNamedTypeDefinitions(p: NamedTypeDefinition => Boolean): immutable.IndexedSeq[NamedTypeDefinition] = {
    schemaDocuments.flatMap(d => d.schemaRootElem.filterChildElemsOfType(classTag[NamedTypeDefinition])(p))
  }

  final def findAllDirectSubstitutables(p: EName => Boolean): immutable.IndexedSeq[GlobalElementDeclaration] = {
    filterGlobalElementDeclarations(e => e.substitutionGroupOption.isDefined && p(e.substitutionGroupOption.get))
  }

  final def findAllImports: immutable.IndexedSeq[Import] =
    schemaDocuments.flatMap(d => d.schemaRootElem.findAllChildElemsOfType(classTag[Import]))

  final def findAllIncludes: immutable.IndexedSeq[Include] =
    schemaDocuments.flatMap(d => d.schemaRootElem.findAllChildElemsOfType(classTag[Include]))

  final def findAllRedefines: immutable.IndexedSeq[Redefine] =
    schemaDocuments.flatMap(d => d.schemaRootElem.findAllChildElemsOfType(classTag[Redefine]))

  /**
   * Returns all element declarations in this SchemaDocumentSet.
   */
  final def findAllElementDeclarationOrReferences: immutable.IndexedSeq[ElementDeclarationOrReference] =
    schemaDocuments flatMap { e => e.schemaRootElem.findAllElemsOfType(classTag[ElementDeclarationOrReference]) }

  /**
   * Returns all element declarations in this SchemaDocumentSet obeying the given predicate.
   */
  final def filterElementDeclarationOrReferences(p: ElementDeclarationOrReference => Boolean): immutable.IndexedSeq[ElementDeclarationOrReference] =
    schemaDocuments flatMap { e => e.schemaRootElem.filterElemsOfType(classTag[ElementDeclarationOrReference])(p) }

  /**
   * Finds the global element declaration with the given EName, if any, wrapped in an Option.
   *
   * The implementation is inefficient in that the target namespace (at root level) of the schema documents is not
   * taken into account.
   */
  final def findGlobalElementDeclarationByEName(ename: EName): Option[GlobalElementDeclaration] =
    allGlobalElementDeclarationsMappedByEName.get(ename)

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
}
