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

/**
 * Constants such as namespaces and expanded names (in the XML Schema namespace).
 *
 * @author Chris de Vreeze
 */
package object xs {

  val XsNamespace = "http://www.w3.org/2001/XMLSchema"

  // ENames

  val XsSchemaEName = EName(XsNamespace, "schema")
  val XsElementEName = EName(XsNamespace, "element")
  val XsAttributeEName = EName(XsNamespace, "attribute")
  val XsComplexTypeEName = EName(XsNamespace, "complexType")
  val XsSimpleTypeEName = EName(XsNamespace, "simpleType")
  val XsAnnotationEName = EName(XsNamespace, "annotation")
  val XsComplexContentEName = EName(XsNamespace, "complexContent")
  val XsSimpleContentEName = EName(XsNamespace, "simpleContent")
  val XsGroupEName = EName(XsNamespace, "group")
  val XsAllEName = EName(XsNamespace, "all")
  val XsChoiceEName = EName(XsNamespace, "choice")
  val XsSequenceEName = EName(XsNamespace, "sequence")
  val XsAttributeGroupEName = EName(XsNamespace, "attributeGroup")
  val XsAnyAttributeEName = EName(XsNamespace, "anyAttribute")
  val XsUniqueEName = EName(XsNamespace, "unique")
  val XsKeyEName = EName(XsNamespace, "key")
  val XsKeyrefEName = EName(XsNamespace, "keyref")
  val XsNotationEName = EName(XsNamespace, "notation")
  val XsImportEName = EName(XsNamespace, "import")
  val XsIncludeEName = EName(XsNamespace, "include")
  val XsRedefineEName = EName(XsNamespace, "redefine")
  val XsRestrictionEName = EName(XsNamespace, "restriction")
  val XsExtensionEName = EName(XsNamespace, "extension")
  val XsListEName = EName(XsNamespace, "list")
  val XsUnionEName = EName(XsNamespace, "union")
  val XsAppinfoEName = EName(XsNamespace, "appinfo")
  val XsDocumentationEName = EName(XsNamespace, "documentation")
  val XsSelectorEName = EName(XsNamespace, "selector")
  val XsFieldEName = EName(XsNamespace, "field")
  val XsAnyEName = EName(XsNamespace, "any")
  val XsAnyTypeEName = EName(XsNamespace, "anyType")

  val XsMinExclusiveEName = EName(XsNamespace, "minExclusive")
  val XsMinInclusiveEName = EName(XsNamespace, "minInclusive")
  val XsMaxExclusiveEName = EName(XsNamespace, "maxExclusive")
  val XsMaxInclusiveEName = EName(XsNamespace, "maxInclusive")
  val XsTotalDigitsEName = EName(XsNamespace, "totalDigits")
  val XsFractionDigitsEName = EName(XsNamespace, "fractionDigits")
  val XsLengthEName = EName(XsNamespace, "length")
  val XsMinLengthEName = EName(XsNamespace, "minLength")
  val XsMaxLengthEName = EName(XsNamespace, "maxLength")
  val XsEnumerationEName = EName(XsNamespace, "enumeration")
  val XsWhiteSpaceEName = EName(XsNamespace, "whiteSpace")
  val XsPatternEName = EName(XsNamespace, "pattern")

  val NameEName = EName("name")
  val IdEName = EName("id")
  val FormEName = EName("form")
  val AbstractEName = EName("abstract")
  val FinalEName = EName("final")
  val DefaultEName = EName("default")
  val FixedEName = EName("fixed")
  val TypeEName = EName("type")
  val NillableEName = EName("nillable")
  val BlockEName = EName("block")
  val UseEName = EName("use")
  val SubstitutionGroupEName = EName("substitutionGroup")
  val RefEName = EName("ref")
  val ReferEName = EName("refer")
  val SchemaLocationEName = EName("schemaLocation")
  val MinOccursEName = EName("minOccurs")
  val MaxOccursEName = EName("maxOccurs")
  val BaseEName = EName("base")
  val TargetNamespaceEName = EName("targetNamespace")
  val ElementFormDefaultEName = EName("elementFormDefault")
  val AttributeFormDefaultEName = EName("attributeFormDefault")
}
