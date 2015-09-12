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

package eu.cdevreeze.yaidomxs.model

import scala.collection.immutable

import eu.cdevreeze.yaidom.core.EName

/**
 * API shared by schema documents and collections of them ("schemas").
 *
 * @author Chris de Vreeze
 */
trait SchemaApi {

  type GlobalElemDecl
  type GlobalAttrDecl
  type NamedTypeDef

  /**
   * Returns all global element declarations mapped by EName.
   */
  def findAllGlobalElementDeclarationsMappedByEName: Map[EName, GlobalElemDecl]

  /**
   * Returns all global element declarations.
   */
  def findAllGlobalElementDeclarations: immutable.IndexedSeq[GlobalElemDecl]

  /**
   * Returns all global element declarations obeying the given predicate.
   */
  def filterGlobalElementDeclarations(p: GlobalElemDecl => Boolean): immutable.IndexedSeq[GlobalElemDecl]

  /**
   * Returns all global attribute declarations mapped by EName.
   */
  def findAllGlobalAttributeDeclarationsMappedByEName: Map[EName, GlobalAttrDecl]

  /**
   * Returns all global attribute declarations.
   */
  def findAllGlobalAttributeDeclarations: immutable.IndexedSeq[GlobalAttrDecl]

  /**
   * Returns all global attribute declarations obeying the given predicate.
   */
  def filterGlobalAttributeDeclarations(p: GlobalAttrDecl => Boolean): immutable.IndexedSeq[GlobalAttrDecl]

  /**
   * Returns all named type definitions mapped by EName.
   */
  def findAllNamedTypeDefinitionsMappedByEName: Map[EName, NamedTypeDef]

  /**
   * Returns all named type definitions.
   */
  def findAllNamedTypeDefinitions: immutable.IndexedSeq[NamedTypeDef]

  /**
   * Returns all named type definitions obeying the given predicate.
   */
  def filterNamedTypeDefinitions(p: NamedTypeDef => Boolean): immutable.IndexedSeq[NamedTypeDef]

  /**
   * Returns all global element declarations that have a substitution group matching the given predicate on the
   * substitution group.
   */
  def findAllDirectSubstitutables(p: EName => Boolean): immutable.IndexedSeq[GlobalElemDecl]

  /**
   * Returns all imports.
   */
  def findAllImports: immutable.IndexedSeq[Import]

  /**
   * Returns all includes.
   */
  def findAllIncludes: immutable.IndexedSeq[Include]

  /**
   * Returns all redefines.
   */
  def findAllRedefines: immutable.IndexedSeq[Redefine]
}
