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

/**
 * This package represents XML Schema document content as immutable yaidom "elements". These schema objects offer the
 * `ElemLike` query API.
 *
 * This API only models schemas. It is not capable of validating instance documents against schemas.
 *
 * Only version 1.0 of XML Schema is modeled.
 *
 * @author Chris de Vreeze
 */
package object schema {

  val NS = "http://www.w3.org/2001/XMLSchema"

  // ENames

  val ENameSchema = EName(NS, "schema")
  val ENameElement = EName(NS, "element")
  val ENameAttribute = EName(NS, "attribute")
  val ENameComplexType = EName(NS, "complexType")
  val ENameSimpleType = EName(NS, "simpleType")
  val ENameAnnotation = EName(NS, "annotation")
  val ENameComplexContent = EName(NS, "complexContent")
  val ENameSimpleContent = EName(NS, "simpleContent")
  val ENameGroup = EName(NS, "group")
  val ENameAll = EName(NS, "all")
  val ENameChoice = EName(NS, "choice")
  val ENameSequence = EName(NS, "sequence")
  val ENameAttributeGroup = EName(NS, "attributeGroup")
  val ENameAnyAttribute = EName(NS, "anyAttribute")
  val ENameUnique = EName(NS, "unique")
  val ENameKey = EName(NS, "key")
  val ENameKeyref = EName(NS, "keyref")
  val ENameNotation = EName(NS, "notation")
  val ENameImport = EName(NS, "import")
  val ENameInclude = EName(NS, "include")
  val ENameRedefine = EName(NS, "redefine")
  val ENameRestriction = EName(NS, "restriction")
  val ENameExtension = EName(NS, "extension")
  val ENameList = EName(NS, "list")
  val ENameUnion = EName(NS, "union")
  val ENameAppinfo = EName(NS, "appinfo")
  val ENameDocumentation = EName(NS, "documentation")
  val ENameSelector = EName(NS, "selector")
  val ENameField = EName(NS, "field")
  val ENameAny = EName(NS, "any")
  val ENameAnyType = EName(NS, "anyType")

  val ENameMinExclusive = EName(NS, "minExclusive")
  val ENameMinInclusive = EName(NS, "minInclusive")
  val ENameMaxExclusive = EName(NS, "maxExclusive")
  val ENameMaxInclusive = EName(NS, "maxInclusive")
  val ENameTotalDigits = EName(NS, "totalDigits")
  val ENameFractionDigits = EName(NS, "fractionDigits")
  val ENameLength = EName(NS, "length")
  val ENameMinLength = EName(NS, "minLength")
  val ENameMaxLength = EName(NS, "maxLength")
  val ENameEnumeration = EName(NS, "enumeration")
  val ENameWhiteSpace = EName(NS, "whiteSpace")
  val ENamePattern = EName(NS, "pattern")

  val ENameName = EName("name")
  val ENameId = EName("id")
  val ENameForm = EName("form")
  val ENameAbstract = EName("abstract")
  val ENameFinal = EName("final")
  val ENameDefault = EName("default")
  val ENameFixed = EName("fixed")
  val ENameType = EName("type")
  val ENameNillable = EName("nillable")
  val ENameBlock = EName("block")
  val ENameUse = EName("use")
  val ENameSubstitutionGroup = EName("substitutionGroup")
  val ENameRef = EName("ref")
  val ENameRefer = EName("refer")
  val ENameSchemaLocation = EName("schemaLocation")
  val ENameMinOccurs = EName("minOccurs")
  val ENameMaxOccurs = EName("maxOccurs")
  val ENameBase = EName("base")
  val ENameTargetNamespace = EName("targetNamespace")
  val ENameElementFormDefault = EName("elementFormDefault")
  val ENameAttributeFormDefault = EName("attributeFormDefault")
}
