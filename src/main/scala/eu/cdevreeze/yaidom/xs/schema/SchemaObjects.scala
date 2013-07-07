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
 * Constraints on XML Schema parts, expressed at the level of `indexed.Elem`, and some helper functions.
 *
 * @author Chris de Vreeze
 */
private[schema] object SchemaObjects {

  /**
   * Checks the XML element as XML Schema (root element), throwing an exception if invalid.
   */
  def checkSchemaElem(elem: indexed.Elem): Unit = {
    checkSchemaElemAgainstSchema(elem)
  }

  /**
   * Checks the XML element as XML Schema element declaration, throwing an exception if invalid.
   */
  def checkElementDeclarationElem(elem: indexed.Elem): Unit = {
    def isGlobal: Boolean = elem.elemPath.entries.size == 1

    def isReference: Boolean = refOption(elem).isDefined

    def isAbstract: Boolean = abstractOption(elem) == Some(true)

    if (isGlobal) {
      checkGlobalElementDeclarationElemAgainstSchema(elem)
    } else {
      checkLocalElementDeclarationElemAgainstSchema(elem)
    }

    if (!isGlobal) {
      require(refOption(elem).isDefined || nameOption(elem).isDefined, "One of 'ref' or 'name' must be present")
      require(refOption(elem).isEmpty || nameOption(elem).isEmpty, "One of 'ref' or 'name' must be absent")
    }

    require(
      (elem \@ ENameDefault).isEmpty || (elem \@ ENameFixed).isEmpty,
      "Element declarations can not have both 'default' and 'fixed'")

    require(
      ((elem \@ ENameType).isEmpty) ||
        ((elem \ ENameSimpleType).isEmpty && (elem \ ENameComplexType).isEmpty),
      "Element declarations can not have both a 'type' attribute and a 'complexType' or 'simpleType' child element")

    if (isReference) {
      assert(!isGlobal)

      val noNsAttributeNames = elem.resolvedAttributes.toMap.keySet filter { attr => attr.namespaceUriOption.isEmpty }
      require(
        noNsAttributeNames.subsetOf(Set(ENameRef, ENameMinOccurs, ENameMaxOccurs, ENameId)),
        "Expected only 'ref', 'minOccurs', 'maxOccurs' and 'id' attributes, but got %s".format(noNsAttributeNames.mkString(", ")))
    }

    if (isReference) {
      assert(!isGlobal)

      val xsdChildElemNames = elem.findAllChildElems collect { case e if e.resolvedName.namespaceUriOption == Some(NS) => e.resolvedName }
      require(
        xsdChildElemNames.toSet.subsetOf(Set(ENameAnnotation)),
        "Element declarations that are references must only have 'annotation' child elements")
    }

    if (elem.attributeOption(ENameTargetNamespace).isDefined) {
      require((elem \@ ENameName).isDefined, "Element declarations with targetNamespace must have a 'name' attribute")
      require((elem \@ ENameForm).isEmpty, "Element declarations with targetNamespace must have no 'form' attribute")

      if (elem.rootElem.attributeOption(ENameTargetNamespace) != elem.attributeOption(ENameTargetNamespace)) {
        val complexTypeOption = elem findAncestor { e => e.resolvedName == ENameComplexType }
        require(complexTypeOption.isDefined, "Expected complexType ancestor")

        val restrictionOption = elem findAncestor { e =>
          e.resolvedName == ENameRestriction &&
            e.elemPath.ancestorPaths.contains(complexTypeOption.get.elemPath)
        }
        require(restrictionOption.isDefined, "Expected restriction between this element and the nearest complexType ancestor")

        val base = restrictionOption.get.attribute(ENameBase)
        val baseENameOption = restrictionOption.get.elem.scope.resolveQNameOption(QName(base))
        require(baseENameOption != Some(ENameAnyType), "Expected restriction base other than xs:anyType")
      }
    }
  }

  /**
   * Checks the XML element as XML Schema particle, throwing an exception if invalid.
   */
  def checkParticleElem(elem: indexed.Elem): Unit = {
    require(minOccursAttrOption(elem).getOrElse("") forall (_.isDigit), "@minOccurs must be a non-negative integer")
    require((maxOccursAttrOption(elem).getOrElse("").toLowerCase(java.util.Locale.ENGLISH) == "unbounded") ||
      (maxOccursAttrOption(elem).getOrElse("") forall (_.isDigit)),
      "@maxOccurs must be a non-negative integer, or 'unbounded'")
  }

  /**
   * Checks that the XML element is not an XML Schema particle, throwing an exception otherwise.
   */
  def checkNotAParticleElem(elem: indexed.Elem): Unit = {
    require(minOccursAttrOption(elem).isEmpty, "no @minOccurs expected")
    require(maxOccursAttrOption(elem).isEmpty, "no @maxOccurs expected")
  }

  /**
   * Checks the XML element as XML Schema attribute declaration, throwing an exception if invalid.
   */
  def checkAttributeDeclarationElem(elem: indexed.Elem): Unit = {
    def isGlobal: Boolean = elem.elemPath.entries.size == 1

    def isReference: Boolean = refOption(elem).isDefined

    if (isGlobal) {
      checkGlobalAttributeDeclarationElemAgainstSchema(elem)
    } else {
      checkLocalAttributeDeclarationElemAgainstSchema(elem)
    }

    // TODO

    if (isGlobal) {
      // TODO Attribute 'use' prohibited

      require(!isReference, "Top level attribute declarations can not be references")
      require(elem.attributeOption(ENameForm).isEmpty, "Top level attribute declarations can not have a 'form' attribute")

      require(nameOption(elem).isDefined, "Top level attribute declarations must have a name attribute")
    } else {
      require(refOption(elem).isDefined || nameOption(elem).isDefined, "One of 'ref' or 'name' must be present")
      require(refOption(elem).isEmpty || nameOption(elem).isEmpty, "One of 'ref' or 'name' must be absent")
    }

    require(
      (elem \@ ENameDefault).isEmpty || (elem \@ ENameFixed).isEmpty,
      "Attribute declarations can not have both 'default' and 'fixed'")

    if ((elem \@ ENameDefault).isDefined && (elem \@ ENameUse).isDefined) {
      require(elem.attribute(ENameUse) == "optional", "If attributes 'default' and 'use' are both present, 'use' must have value 'optional'")
    }

    if (isReference) {
      assert(!isGlobal)
      require((elem.resolvedAttributes.toMap.keySet intersect Set(ENameForm, ENameType)).isEmpty,
        "Attribute declarations that are references must not have attributes 'form', 'type'")
    }

    if (isReference) {
      assert(!isGlobal)
      require((elem.findAllChildElems.map(_.resolvedName).toSet intersect Set(ENameSimpleType)).isEmpty,
        "Attribute declarations that are references must not have child element 'simpleType'")
    }
  }

  /**
   * Checks the XML element as XML Schema attribute-use, throwing an exception if invalid.
   */
  def checkAttributeUseElem(elem: indexed.Elem): Unit = {
    // TODO
  }

  /**
   * Checks that the XML element is not an XML Schema attribute-use, throwing an exception otherwise.
   */
  def checkNotAnAttributeUseElem(elem: indexed.Elem): Unit = {
    // TODO
    require((elem \@ ENameUse).isEmpty, "Expected no attribute 'use'")
  }

  /**
   * Checks the XML element as XML Schema simple type definition, throwing an exception if invalid.
   */
  def checkSimpleTypeDefinitionElem(elem: indexed.Elem): Unit = {
    def isGlobal: Boolean = elem.elemPath.entries.size == 1

    if (isGlobal) {
      checkGlobalSimpleTypeDefinitionElemAgainstSchema(elem)
    } else {
      checkLocalSimpleTypeDefinitionElemAgainstSchema(elem)
    }
  }

  /**
   * Checks the XML element as XML Schema complex type definition, throwing an exception if invalid.
   */
  def checkComplexTypeDefinitionElem(elem: indexed.Elem): Unit = {
    def isGlobal: Boolean = elem.elemPath.entries.size == 1

    if (isGlobal) {
      checkGlobalComplexTypeDefinitionElemAgainstSchema(elem)
    } else {
      checkLocalComplexTypeDefinitionElemAgainstSchema(elem)
    }
  }

  /**
   * Checks the XML element as XML Schema attribute group definition, throwing an exception if invalid.
   */
  def checkAttributeGroupDefinitionElem(elem: indexed.Elem): Unit = {
    def isGlobal: Boolean = elem.elemPath.entries.size == 1

    def isRedefineChild: Boolean = elem.parentOption.map(_.resolvedName) == Some(ENameRedefine)

    if (isGlobal || isRedefineChild) {
      checkNamedAttributeGroupElemAgainstSchema(elem)
    } else {
      checkNamedAttributeGroupRefElemAgainstSchema(elem)
    }
  }

  /**
   * Checks the XML element as XML Schema identity constraint definition, throwing an exception if invalid.
   */
  def checkIdentityConstraintDefinitionElem(elem: indexed.Elem): Unit = {
    val expectedENames = Set(ENameKey, ENameKeyref, ENameUnique)
    require(expectedENames.contains(elem.resolvedName), "The element must be a 'key', 'keyref' or 'unique' element")

    if (elem.resolvedName == ENameUnique) checkUniqueElemAgainstSchema(elem)
    else if (elem.resolvedName == ENameKey) checkKeyElemAgainstSchema(elem)
    else checkKeyrefElemAgainstSchema(elem)
  }

  /**
   * Checks the XML element as XML Schema model group definition, throwing an exception if invalid.
   */
  def checkModelGroupDefinitionElem(elem: indexed.Elem): Unit = {
    def isGlobal: Boolean = elem.elemPath.entries.size == 1

    def isRedefineChild: Boolean = elem.parentOption.map(_.resolvedName) == Some(ENameRedefine)

    if (isGlobal || isRedefineChild) {
      checkNamedModelGroupElemAgainstSchema(elem)
    } else {
      checkNamedModelGroupRefElemAgainstSchema(elem)
    }
  }

  /**
   * Checks the XML element as XML Schema notation declaration, throwing an exception if invalid.
   */
  def checkNotationDeclarationElem(elem: indexed.Elem): Unit = {
    checkNotationDeclarationElemAgainstSchema(elem)
  }

  /**
   * Checks the XML element as XML Schema model group, throwing an exception if invalid.
   */
  def checkModelGroupElem(elem: indexed.Elem): Unit = {
    val expectedENames = Set(ENameAll, ENameSequence, ENameChoice)
    require(expectedENames.contains(elem.resolvedName), "The element must be a 'all', 'sequence' or 'choice' element")

    if (elem.resolvedName == ENameAll) checkAllElemAgainstSchema(elem)
    else if (elem.resolvedName == ENameSequence) checkSequenceElemAgainstSchema(elem)
    else checkChoiceElemAgainstSchema(elem)
  }

  /**
   * Checks the XML element as XML Schema wildcard, throwing an exception if invalid.
   */
  def checkWildcardElem(elem: indexed.Elem): Unit = {
    val expectedENames = Set(ENameAny, ENameAnyAttribute)
    require(expectedENames.contains(elem.resolvedName), "The element must be a 'any' or 'anyAttribute' element")

    if (elem.resolvedName == ENameAny) checkAnyElemAgainstSchema(elem)
    else checkAnyAttributeElemAgainstSchema(elem)
  }

  /**
   * Checks the XML element as XML Schema annotation, throwing an exception if invalid.
   */
  def checkAnnotationElem(elem: indexed.Elem): Unit = {
    checkAnnotationElemAgainstSchema(elem)
  }

  /**
   * Checks the XML element as "xs:import", throwing an exception if invalid.
   */
  def checkImportElem(elem: indexed.Elem): Unit = {
    checkImportElemAgainstSchema(elem)
  }

  /**
   * Checks the XML element as "xs:include", throwing an exception if invalid.
   */
  def checkIncludeElem(elem: indexed.Elem): Unit = {
    checkIncludeElemAgainstSchema(elem)
  }

  /**
   * Checks the XML element as "xs:redefine", throwing an exception if invalid.
   */
  def checkRedefineElem(elem: indexed.Elem): Unit = {
    checkRedefineElemAgainstSchema(elem)
  }

  /**
   * Checks the XML element as "xs:complexContent", throwing an exception if invalid.
   */
  def checkComplexContentElem(elem: indexed.Elem): Unit = {
    checkComplexContentElemAgainstSchema(elem)
  }

  /**
   * Checks the XML element as "xs:simpleContent", throwing an exception if invalid.
   */
  def checkSimpleContentElem(elem: indexed.Elem): Unit = {
    checkSimpleContentElemAgainstSchema(elem)
  }

  /**
   * Checks the XML element as "xs:extension", throwing an exception if invalid.
   */
  def checkExtensionElem(elem: indexed.Elem): Unit = {
    if (elem.parentOption.map(_.resolvedName) == Some(ENameSimpleContent)) checkSimpleExtensionElemAgainstSchema(elem)
    else checkExtensionElemAgainstSchema(elem)
  }

  /**
   * Checks the XML element as "xs:restriction", throwing an exception if invalid.
   */
  def checkRestrictionElem(elem: indexed.Elem): Unit = {
    if (elem.parentOption.map(_.resolvedName) == Some(ENameSimpleContent)) checkSimpleRestrictionElemAgainstSchema(elem)
    else if (elem.parentOption.map(_.resolvedName) == Some(ENameComplexContent)) checkComplexRestrictionElemAgainstSchema(elem)
    else checkDerivationRestrictionElemAgainstSchema(elem)
  }

  // Public helper methods

  /**
   * Returns the value of the 'name' attribute, if any, wrapped in an Option.
   */
  def nameOption(elem: indexed.Elem): Option[String] = elem \@ ENameName

  /**
   * Returns the value of the 'id' attribute, if any, wrapped in an Option.
   */
  def idOption(elem: indexed.Elem): Option[String] = elem \@ ENameId

  /**
   * Returns the value of the 'type' attribute as expanded name, if any, wrapped in an Option.
   */
  def typeAttributeOption(elem: indexed.Elem): Option[EName] = {
    val typeAttrAttrOption = elem \@ ENameType
    typeAttrAttrOption map { tpe =>
      elem.elem.scope.resolveQNameOption(QName(tpe)).getOrElse(
        sys.error("Could not resolve type '%s' as expanded name".format(tpe)))
    }
  }

  /**
   * Returns the value of the 'substitutionGroup' attribute as expanded name, if any, wrapped in an Option.
   */
  def substitutionGroupOption(elem: indexed.Elem): Option[EName] = {
    val substGroupAttrOption = elem \@ ENameSubstitutionGroup
    substGroupAttrOption map { substGroup =>
      elem.elem.scope.resolveQNameOption(QName(substGroup)).getOrElse(
        sys.error("Could not resolve substitution group '%s' as expanded name".format(substGroup)))
    }
  }

  /**
   * Returns the value of the 'abstract' attribute, if any, wrapped in an Option.
   */
  def abstractOption(elem: indexed.Elem): Option[Boolean] = {
    try {
      (elem \@ ENameAbstract) map (_.toBoolean)
    } catch {
      case e: Exception => None
    }
  }

  /**
   * Returns the value of the 'nillable' attribute, if any, wrapped in an Option.
   */
  def nillableOption(elem: indexed.Elem): Option[Boolean] = {
    try {
      (elem \@ ENameNillable) map (_.toBoolean)
    } catch {
      case e: Exception => None
    }
  }

  /**
   * Returns the value of the 'ref' attribute as expanded name, if any, wrapped in an Option.
   */
  def refOption(elem: indexed.Elem): Option[EName] = {
    val refOption = elem \@ ENameRef
    refOption map { ref =>
      elem.elem.scope.resolveQNameOption(QName(ref)).getOrElse(
        sys.error("Could not resolve ref '%s' as expanded name".format(ref)))
    }
  }

  /**
   * Returns the value of the 'minOccurs' attribute, if any, wrapped in an Option.
   */
  def minOccursAttrOption(elem: indexed.Elem): Option[String] = (elem \@ ENameMinOccurs)

  /**
   * Returns the value of the 'maxOccurs' attribute, if any, wrapped in an Option.
   */
  def maxOccursAttrOption(elem: indexed.Elem): Option[String] = (elem \@ ENameMaxOccurs)

  // Validations against only the Schema of XML Schema itself, rewritten in Scala

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_schema.html
   */
  private def checkSchemaElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameSchema, "The element must be a 'schema' element")
    require(elem.elemPath.isRoot, "The element must be the root of the element tree")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(
      ENameInclude,
      ENameImport,
      ENameRedefine,
      ENameAnnotation,
      ENameElement,
      ENameAttribute,
      ENameNotation,
      ENameSimpleType,
      ENameComplexType,
      ENameGroup,
      ENameAttributeGroup)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'schema' child elements: %s".format(expectedChildENames.mkString(", ")))

    val expectedFirstChildNames = Set(ENameInclude, ENameImport, ENameRedefine)
    val expectedRemainingChildNames = (expectedChildENames - ENameAnnotation) diff expectedFirstChildNames

    val childElemsWithoutAnnotations = childElems filterNot { e => e.resolvedName == ENameAnnotation }

    require(
      isCorrectlyOrdered(
        childElemsWithoutAnnotations,
        Seq(
          expectedFirstChildNames,
          expectedRemainingChildNames)),
      "Expected 'include', 'import' and 'redefine' child elements to come before the others (ignoring annotations)")

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_element.html
   */
  private def checkGlobalElementDeclarationElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.elemPath.entries.size == 1)

    checkElementDeclarationElemAgainstSchema(elem)

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameName).isDefined, "Missing attribute 'name'")

    require(elem.attributeOption(ENameRef).isEmpty, "Attribute 'ref' prohibited")
    require(elem.attributeOption(ENameForm).isEmpty, "Attribute 'form' prohibited")
    require(elem.attributeOption(ENameMinOccurs).isEmpty, "Attribute 'minOccurs' prohibited")
    require(elem.attributeOption(ENameMaxOccurs).isEmpty, "Attribute 'maxOccurs' prohibited")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_element-1.html
   */
  private def checkLocalElementDeclarationElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.elemPath.entries.size >= 2)

    checkElementDeclarationElemAgainstSchema(elem)

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameSubstitutionGroup).isEmpty, "Attribute 'substitutionGroup' prohibited")
    require(elem.attributeOption(ENameFinal).isEmpty, "Attribute 'final' prohibited")
    require(elem.attributeOption(ENameAbstract).isEmpty, "Attribute 'abstract' prohibited")
  }

  private def checkElementDeclarationElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameElement, "The element must be an 'element' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(
      ENameSimpleType,
      ENameComplexType,
      ENameAnnotation,
      ENameUnique,
      ENameKey,
      ENameKeyref)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'element' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameSimpleType, ENameComplexType).contains(e.resolvedName)) <= 1,
      "At most one complex/simple type definition child allowed")

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(
      isCorrectlyOrdered(
        childElems,
        Seq(
          Set(ENameAnnotation),
          Set(ENameSimpleType, ENameComplexType),
          Set(ENameUnique, ENameKey, ENameKeyref))),
      "Expected 'simpleType' and 'complexType' child elements to come before the others (ignoring annotations)")

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_attribute.html
   */
  private def checkGlobalAttributeDeclarationElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.elemPath.entries.size == 1)

    checkAttributeDeclarationElemAgainstSchema(elem)

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameName).isDefined, "Missing attribute 'name'")

    require(elem.attributeOption(ENameRef).isEmpty, "Attribute 'ref' prohibited")
    require(elem.attributeOption(ENameForm).isEmpty, "Attribute 'form' prohibited")
    require(elem.attributeOption(ENameUse).isEmpty, "Attribute 'use' prohibited")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_attribute-1.html
   */
  private def checkLocalAttributeDeclarationElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.elemPath.entries.size >= 2)

    checkAttributeDeclarationElemAgainstSchema(elem)

    // TODO Validate attributes and their types
  }

  private def checkAttributeDeclarationElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameAttribute, "The element must be an 'attribute' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(ENameSimpleType, ENameAnnotation)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'attribute' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameSimpleType).contains(e.resolvedName)) <= 1,
      "At most one complex/simple type definition child allowed")

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(
      isCorrectlyOrdered(
        childElems,
        Seq(
          Set(ENameAnnotation),
          Set(ENameSimpleType))),
      "Expected 'simpleType' child element to come after the annotation, if any")

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_complexType.html
   */
  private def checkGlobalComplexTypeDefinitionElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.elemPath.entries.size == 1)

    checkComplexTypeDefinitionElemAgainstSchema(elem)

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameName).isDefined, "Missing attribute 'name'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_complexType-1.html
   */
  private def checkLocalComplexTypeDefinitionElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.elemPath.entries.size >= 2)

    checkComplexTypeDefinitionElemAgainstSchema(elem)

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameName).isEmpty, "Attribute 'name' prohibited")
    require(elem.attributeOption(ENameAbstract).isEmpty, "Attribute 'abstract' prohibited")
    require(elem.attributeOption(ENameFinal).isEmpty, "Attribute 'final' prohibited")
    require(elem.attributeOption(ENameBlock).isEmpty, "Attribute 'block' prohibited")
  }

  private def checkComplexTypeDefinitionElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameComplexType, "The element must be a 'complexType' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(
      ENameAnnotation,
      ENameSimpleContent,
      ENameComplexContent,
      ENameGroup,
      ENameAll,
      ENameChoice,
      ENameSequence,
      ENameAttribute,
      ENameAttributeGroup,
      ENameAnyAttribute)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'complexType' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(childElems.count(e => Set(ENameSimpleContent, ENameComplexContent).contains(e.resolvedName)) <= 1,
      "At most one complex/simple content child allowed")

    require(childElems.count(e => Set(ENameGroup).contains(e.resolvedName)) <= 1,
      "At most one group child allowed")

    require(childElems.count(e => Set(ENameAll).contains(e.resolvedName)) <= 1,
      "At most one all child allowed")

    require(childElems.count(e => Set(ENameChoice).contains(e.resolvedName)) <= 1,
      "At most one choice child allowed")

    require(childElems.count(e => Set(ENameSequence).contains(e.resolvedName)) <= 1,
      "At most one sequence child allowed")

    require(childElems.count(e => Set(ENameAnyAttribute).contains(e.resolvedName)) <= 1,
      "At most one anyAttribute child allowed")

    require(
      isCorrectlyOrdered(
        childElems,
        Seq(
          Set(ENameAnnotation),
          (expectedChildENames - ENameAnnotation))),
      "Expected 'annotation' child element, if any, to come before the others")

    val childElemsButAnnotation = childElems filterNot (e => e.resolvedName == ENameAnnotation)

    require(
      isOptionalChoice(
        childElemsButAnnotation,
        Set(
          Set(ENameSimpleContent),
          Set(ENameComplexContent),
          (expectedChildENames diff Set(ENameAnnotation, ENameSimpleContent, ENameComplexContent)))),
      "Expected choice between 'simpleContent', 'complexContent' and a sequence starting with a model group (definition)")

    val childElemsButAnnotationAndContent = childElems filterNot { e =>
      Set(ENameAnnotation, ENameSimpleContent, ENameComplexContent).contains(e.resolvedName)
    }

    require(
      isCorrectlyOrdered(
        childElemsButAnnotationAndContent,
        Seq(
          Set(ENameGroup, ENameAll, ENameChoice, ENameSequence),
          Set(ENameAttribute, ENameAttributeGroup),
          Set(ENameAnyAttribute))),
      "Expected 'attribute' and 'attributeGroup' child elements, if any, to come after 'group', 'all', 'choice' and 'sequence'")

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_simpleType.html
   */
  private def checkGlobalSimpleTypeDefinitionElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.elemPath.entries.size == 1)

    checkSimpleTypeDefinitionElemAgainstSchema(elem)

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameName).isDefined, "Missing attribute 'name'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_simpleType-1.html
   */
  private def checkLocalSimpleTypeDefinitionElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.elemPath.entries.size >= 2)

    checkSimpleTypeDefinitionElemAgainstSchema(elem)

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameName).isEmpty, "Attribute 'name' prohibited")
    require(elem.attributeOption(ENameFinal).isEmpty, "Attribute 'final' prohibited")
  }

  private def checkSimpleTypeDefinitionElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameSimpleType, "The element must be a 'simpleType' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(
      ENameAnnotation,
      ENameRestriction,
      ENameList,
      ENameUnion)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'simpleType' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(childElems.count(e => Set(ENameRestriction).contains(e.resolvedName)) <= 1,
      "At most one restriction child allowed")

    require(childElems.count(e => Set(ENameList).contains(e.resolvedName)) <= 1,
      "At most one list child allowed")

    require(childElems.count(e => Set(ENameUnion).contains(e.resolvedName)) <= 1,
      "At most one union child allowed")

    require(
      isCorrectlyOrdered(
        childElems,
        Seq(
          Set(ENameAnnotation),
          (expectedChildENames - ENameAnnotation))),
      "Expected 'annotation' child element, if any, to come before the others")

    val childElemsButAnnotation = childElems filterNot (e => e.resolvedName == ENameAnnotation)

    require(
      isChoice(
        childElemsButAnnotation,
        Set(
          Set(ENameRestriction),
          Set(ENameList),
          Set(ENameUnion))),
      "Expected choice between 'restriction', 'list' and 'union'")

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_group.html
   */
  private def checkNamedModelGroupElemAgainstSchema(elem: indexed.Elem): Unit = {
    require((elem.elemPath.entries.size == 1) || (elem.parentOption.map(_.resolvedName) == Some(ENameRedefine)))

    require(elem.resolvedName == ENameGroup, "The element must be a 'group' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(
      ENameAnnotation,
      ENameAll,
      ENameChoice,
      ENameSequence)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'group' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(childElems.count(e => Set(ENameAll).contains(e.resolvedName)) <= 1,
      "At most one all child allowed")

    require(childElems.count(e => Set(ENameChoice).contains(e.resolvedName)) <= 1,
      "At most one choice child allowed")

    require(childElems.count(e => Set(ENameSequence).contains(e.resolvedName)) <= 1,
      "At most one sequence child allowed")

    require(
      isCorrectlyOrdered(
        childElems,
        Seq(
          Set(ENameAnnotation),
          (expectedChildENames - ENameAnnotation))),
      "Expected 'annotation' child element, if any, to come before the others")

    val childElemsButAnnotation = childElems filterNot (e => e.resolvedName == ENameAnnotation)

    require(
      isChoice(
        childElemsButAnnotation,
        Set(
          Set(ENameAll),
          Set(ENameChoice),
          Set(ENameSequence))),
      "Expected choice between 'all', 'choice' and 'sequence'")

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameName).isDefined, "Missing attribute 'name'")

    require(elem.attributeOption(ENameRef).isEmpty, "Attribute 'ref' prohibited")
    require(elem.attributeOption(ENameMinOccurs).isEmpty, "Attribute 'minOccurs' prohibited")
    require(elem.attributeOption(ENameMaxOccurs).isEmpty, "Attribute 'maxOccurs' prohibited")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_group-1.html
   */
  private def checkNamedModelGroupRefElemAgainstSchema(elem: indexed.Elem): Unit = {
    require((elem.elemPath.entries.size >= 2) && (elem.parent.resolvedName != ENameRedefine))

    require(elem.resolvedName == ENameGroup, "The element must be a 'group' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(ENameAnnotation)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'group' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameRef).isDefined, "Missing attribute 'ref'")

    require(elem.attributeOption(ENameName).isEmpty, "Attribute 'name' prohibited")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_attributeGroup.html
   */
  private def checkNamedAttributeGroupElemAgainstSchema(elem: indexed.Elem): Unit = {
    require((elem.elemPath.entries.size == 1) || (elem.parentOption.map(_.resolvedName) == Some(ENameRedefine)))

    require(elem.resolvedName == ENameAttributeGroup, "The element must be an 'attributeGroup' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(
      ENameAnnotation,
      ENameAttribute,
      ENameAttributeGroup,
      ENameAnyAttribute)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'attributeGroup' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(childElems.count(e => Set(ENameAnyAttribute).contains(e.resolvedName)) <= 1,
      "At most one anyAttribute child allowed")

    require(
      isCorrectlyOrdered(
        childElems,
        Seq(
          Set(ENameAnnotation),
          Set(ENameAttribute, ENameAttributeGroup),
          Set(ENameAnyAttribute))),
      "Expected 'attribute' and 'attributeGroup' child elements, if any, to come after 'annotation' and before 'anyAttribute'")

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameName).isDefined, "Missing attribute 'name'")

    require(elem.attributeOption(ENameRef).isEmpty, "Attribute 'ref' prohibited")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_attributeGroup-1.html
   */
  private def checkNamedAttributeGroupRefElemAgainstSchema(elem: indexed.Elem): Unit = {
    require((elem.elemPath.entries.size >= 2) && (elem.parent.resolvedName != ENameRedefine))

    require(elem.resolvedName == ENameAttributeGroup, "The element must be an 'attributeGroup' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(ENameAnnotation)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'attributeGroup' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameRef).isDefined, "Missing attribute 'ref'")

    require(elem.attributeOption(ENameName).isEmpty, "Attribute 'name' prohibited")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_notation.html
   */
  private def checkNotationDeclarationElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameNotation, "The element must be a 'notation' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(ENameAnnotation)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'notation' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameName).isDefined, "Missing attribute 'name'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_annotation.html
   */
  private def checkAnnotationElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameAnnotation, "The element must be an 'annotation' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(ENameAppinfo, ENameDocumentation)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'annotation' child elements: %s".format(expectedChildENames.mkString(", ")))

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_include.html
   */
  private def checkIncludeElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameInclude, "The element must be an 'include' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(ENameAnnotation)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'include' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameSchemaLocation).isDefined, "Missing attribute 'schemaLocation'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_import.html
   */
  private def checkImportElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameImport, "The element must be an 'import' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(ENameAnnotation)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'import' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_redefine.html
   */
  private def checkRedefineElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameRedefine, "The element must be a 'redefine' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(
      ENameSimpleType,
      ENameComplexType,
      ENameGroup,
      ENameAttributeGroup,
      ENameAnnotation)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'redefine' child elements: %s".format(expectedChildENames.mkString(", ")))

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameSchemaLocation).isDefined, "Missing attribute 'schemaLocation'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_unique.html
   */
  private def checkUniqueElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameUnique, "The element must be a 'unique' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(
      ENameSelector,
      ENameField,
      ENameAnnotation)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'unique' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(childElems.count(e => Set(ENameSelector).contains(e.resolvedName)) == 1,
      "Precisely one selector child expected")

    require(childElems.count(e => Set(ENameField).contains(e.resolvedName)) >= 1,
      "At least one field child expected")

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameName).isDefined, "Missing attribute 'name'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_key.html
   */
  private def checkKeyElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameKey, "The element must be a 'key' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(
      ENameSelector,
      ENameField,
      ENameAnnotation)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'key' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(childElems.count(e => Set(ENameSelector).contains(e.resolvedName)) == 1,
      "Precisely one selector child expected")

    require(childElems.count(e => Set(ENameField).contains(e.resolvedName)) >= 1,
      "At least one field child expected")

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameName).isDefined, "Missing attribute 'name'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_keyref.html
   */
  private def checkKeyrefElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameKeyref, "The element must be a 'keyref' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(
      ENameSelector,
      ENameField,
      ENameAnnotation)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'keyref' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(childElems.count(e => Set(ENameSelector).contains(e.resolvedName)) == 1,
      "Precisely one selector child expected")

    require(childElems.count(e => Set(ENameField).contains(e.resolvedName)) >= 1,
      "At least one field child expected")

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameName).isDefined, "Missing attribute 'name'")

    require(elem.attributeOption(ENameRefer).isDefined, "Missing attribute 'refer'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_all.html
   */
  private def checkAllElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameAll, "The element must be an 'all' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(ENameElement, ENameAnnotation)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'all' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(
      isCorrectlyOrdered(
        childElems,
        Seq(
          Set(ENameAnnotation),
          Set(ENameElement))),
      "Expected 'element' child elements to come after 'annotation', if any")

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_sequence.html
   */
  private def checkSequenceElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameSequence, "The element must be a 'sequence' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(
      ENameAnnotation,
      ENameElement,
      ENameGroup,
      ENameChoice,
      ENameSequence,
      ENameAny)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'sequence' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(
      isCorrectlyOrdered(
        childElems,
        Seq(
          Set(ENameAnnotation),
          Set(ENameElement, ENameGroup, ENameChoice, ENameSequence, ENameAny))),
      "Expected 'annotation' child element, if any, to come before the other elements")

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_choice.html
   */
  private def checkChoiceElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameChoice, "The element must be a 'choice' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(
      ENameAnnotation,
      ENameElement,
      ENameGroup,
      ENameChoice,
      ENameSequence,
      ENameAny)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'choice' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(
      isCorrectlyOrdered(
        childElems,
        Seq(
          Set(ENameAnnotation),
          Set(ENameElement, ENameGroup, ENameChoice, ENameSequence, ENameAny))),
      "Expected 'annotation' child element, if any, to come before the other elements")

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_any.html
   */
  private def checkAnyElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameAny, "The element must be an 'any' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(ENameAnnotation)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'any' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_anyAttribute.html
   */
  private def checkAnyAttributeElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameAnyAttribute, "The element must be an 'anyAttribute' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(ENameAnnotation)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'anyAttribute' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_complexContent.html
   */
  private def checkComplexContentElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameComplexContent, "The element must be a 'complexContent' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(
      ENameAnnotation,
      ENameRestriction,
      ENameExtension)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'complexContent' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(
      isCorrectlyOrdered(
        childElems,
        Seq(
          Set(ENameAnnotation),
          Set(ENameRestriction, ENameExtension))),
      "Expected 'annotation' child element, if any, to come before the other elements")

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_simpleContent.html
   */
  private def checkSimpleContentElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameSimpleContent, "The element must be a 'simpleContent' element")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(
      ENameAnnotation,
      ENameRestriction,
      ENameExtension)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'simpleContent' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(
      isCorrectlyOrdered(
        childElems,
        Seq(
          Set(ENameAnnotation),
          Set(ENameRestriction, ENameExtension))),
      "Expected 'annotation' child element, if any, to come before the other elements")

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_extension-1.html
   */
  private def checkExtensionElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameExtension, "The element must be an 'extension' element")

    require(elem.parentOption.map(_.resolvedName) == Some(ENameComplexContent), "Expected 'complexContent' parent")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(
      ENameAnnotation,
      ENameGroup,
      ENameAll,
      ENameChoice,
      ENameSequence,
      ENameAttribute,
      ENameAttributeGroup,
      ENameAnyAttribute)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'extension' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(childElems.count(e => Set(ENameGroup).contains(e.resolvedName)) <= 1,
      "At most one group child allowed")

    require(childElems.count(e => Set(ENameAll).contains(e.resolvedName)) <= 1,
      "At most one all child allowed")

    require(childElems.count(e => Set(ENameChoice).contains(e.resolvedName)) <= 1,
      "At most one choice child allowed")

    require(childElems.count(e => Set(ENameSequence).contains(e.resolvedName)) <= 1,
      "At most one sequence child allowed")

    require(childElems.count(e => Set(ENameAnyAttribute).contains(e.resolvedName)) <= 1,
      "At most one anyAttribute child allowed")

    require(
      isCorrectlyOrdered(
        childElems,
        Seq(
          Set(ENameAnnotation),
          Set(ENameGroup, ENameAll, ENameChoice, ENameSequence),
          Set(ENameAttribute, ENameAttributeGroup),
          Set(ENameAnyAttribute))),
      "Expected specific order of child elements for an extension")

    require(
      isOptionalChoice(
        childElems filter (e => Set(ENameGroup, ENameAll, ENameChoice, ENameSequence).contains(e.resolvedName)),
        Set(
          Set(ENameGroup),
          Set(ENameAll),
          Set(ENameChoice),
          Set(ENameSequence))),
      "Expected optional choice between 'group', 'all', 'choice' and 'sequence'")

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameBase).isDefined, "Missing attribute 'base'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_extension-2.html
   */
  private def checkSimpleExtensionElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameExtension, "The element must be an 'extension' element")

    require(elem.parentOption.map(_.resolvedName) == Some(ENameSimpleContent), "Expected 'simpleContent' parent")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(
      ENameAnnotation,
      ENameAttribute,
      ENameAttributeGroup,
      ENameAnyAttribute)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'extension' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(childElems.count(e => Set(ENameAnyAttribute).contains(e.resolvedName)) <= 1,
      "At most one anyAttribute child allowed")

    require(
      isCorrectlyOrdered(
        childElems,
        Seq(
          Set(ENameAnnotation),
          Set(ENameAttribute, ENameAttributeGroup),
          Set(ENameAnyAttribute))),
      "Expected 'attribute' and 'attributeGroup' child elements, if any, to come after 'annotation' and before 'anyAttribute'")

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameBase).isDefined, "Missing attribute 'base'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_restriction-1.html
   */
  private def checkComplexRestrictionElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameRestriction, "The element must be a 'restriction' element")

    require(elem.parentOption.map(_.resolvedName) == Some(ENameComplexContent), "Expected 'complexContent' parent")

    val childElems = elem.findAllChildElems

    val expectedChildENames = Set(
      ENameAnnotation,
      ENameGroup,
      ENameAll,
      ENameChoice,
      ENameSequence,
      ENameAttribute,
      ENameAttributeGroup,
      ENameAnyAttribute)
    require(isWithin(childElems, expectedChildENames),
      "Expected 'restriction' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(childElems.count(e => Set(ENameGroup).contains(e.resolvedName)) <= 1,
      "At most one group child allowed")

    require(childElems.count(e => Set(ENameAll).contains(e.resolvedName)) <= 1,
      "At most one all child allowed")

    require(childElems.count(e => Set(ENameChoice).contains(e.resolvedName)) <= 1,
      "At most one choice child allowed")

    require(childElems.count(e => Set(ENameSequence).contains(e.resolvedName)) <= 1,
      "At most one sequence child allowed")

    require(childElems.count(e => Set(ENameAnyAttribute).contains(e.resolvedName)) <= 1,
      "At most one anyAttribute child allowed")

    require(
      isCorrectlyOrdered(
        childElems,
        Seq(
          Set(ENameAnnotation),
          Set(ENameGroup, ENameAll, ENameChoice, ENameSequence),
          Set(ENameAttribute, ENameAttributeGroup),
          Set(ENameAnyAttribute))),
      "Expected specific order of child elements for a restriction")

    require(
      isOptionalChoice(
        childElems filter (e => Set(ENameGroup, ENameAll, ENameChoice, ENameSequence).contains(e.resolvedName)),
        Set(
          Set(ENameGroup),
          Set(ENameAll),
          Set(ENameChoice),
          Set(ENameSequence))),
      "Expected optional choice between 'group', 'all', 'choice' and 'sequence'")

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameBase).isDefined, "Missing attribute 'base'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_restriction-2.html
   */
  private def checkSimpleRestrictionElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameRestriction, "The element must be a 'restriction' element")

    require(elem.parentOption.map(_.resolvedName) == Some(ENameSimpleContent), "Expected 'simpleContent' parent")

    val childElems = elem.findAllChildElems

    val facetENames = Set(
      ENameMinExclusive,
      ENameMinInclusive,
      ENameMaxExclusive,
      ENameMaxInclusive,
      ENameTotalDigits,
      ENameFractionDigits,
      ENameLength,
      ENameMinLength,
      ENameMaxLength,
      ENameEnumeration,
      ENameWhiteSpace,
      ENamePattern)

    val expectedChildENames = Set(
      ENameAnnotation,
      ENameSimpleType,
      ENameAttribute,
      ENameAttributeGroup,
      ENameAnyAttribute) ++ facetENames
    require(isWithin(childElems, expectedChildENames),
      "Expected 'restriction' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(childElems.count(e => Set(ENameSimpleType).contains(e.resolvedName)) <= 1,
      "At most one simpleType child allowed")

    require(childElems.count(e => Set(ENameAnyAttribute).contains(e.resolvedName)) <= 1,
      "At most one anyAttribute child allowed")

    require(
      isCorrectlyOrdered(
        childElems,
        Seq(
          Set(ENameAnnotation),
          Set(ENameSimpleType),
          facetENames,
          Set(ENameAttribute, ENameAttributeGroup),
          Set(ENameAnyAttribute))),
      "Expected specific order of child elements for a restriction")

    // TODO Validate attributes and their types

    require(elem.attributeOption(ENameBase).isDefined, "Missing attribute 'base'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_restriction.html
   */
  private def checkDerivationRestrictionElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == ENameRestriction, "The element must be a 'restriction' element")

    require(elem.parentOption.map(_.resolvedName) == Some(ENameSimpleType), "Expected 'simpleType' parent")

    val childElems = elem.findAllChildElems

    val facetENames = Set(
      ENameMinExclusive,
      ENameMinInclusive,
      ENameMaxExclusive,
      ENameMaxInclusive,
      ENameTotalDigits,
      ENameFractionDigits,
      ENameLength,
      ENameMinLength,
      ENameMaxLength,
      ENameEnumeration,
      ENameWhiteSpace,
      ENamePattern)

    val expectedChildENames = Set(
      ENameAnnotation,
      ENameSimpleType) ++ facetENames
    require(isWithin(childElems, expectedChildENames),
      "Expected 'restriction' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(ENameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(childElems.count(e => Set(ENameSimpleType).contains(e.resolvedName)) <= 1,
      "At most one simpleType child allowed")

    require(
      isCorrectlyOrdered(
        childElems,
        Seq(
          Set(ENameAnnotation),
          Set(ENameSimpleType),
          facetENames)),
      "Expected specific order of child elements for a restriction")

    // TODO Validate attributes and their types
  }

  // Private constraint helper methods

  /**
   * Returns true if the passed Elems have only ENames within the given EName set.
   */
  private def isWithin(elems: Seq[indexed.Elem], enameSet: Set[EName]): Boolean = {
    elems.map(_.resolvedName).toSet.subsetOf(enameSet)
  }

  /**
   * Returns true if the passed Elems are ordered correctly, given the ordered sequence of EName sets.
   * That is, the first Elems start with ENames from `enameSetOrder(0)`, the next Elems have ENames from `enameSetOrder(1)`, etc.
   *
   * An empty Elem sequence is also considered to be correctly ordered.
   */
  private def isCorrectlyOrdered(elems: Seq[indexed.Elem], enameSetOrder: Seq[Set[EName]]): Boolean = {
    require(enameSetOrder.map(_.size).sum == enameSetOrder.flatten.toSet.size, "The ordered EName sets must not be overlapping")

    val orderedElems = enameSetOrder.foldLeft(Seq[indexed.Elem]()) { (accumulatedElems, enameSet) =>
      accumulatedElems ++ (elems filter (e => enameSet.contains(e.resolvedName)))
    }

    elems == orderedElems
  }

  /**
   * Returns true if the passed Elems are a choice between the given EName sets.
   * That is, the ENames of the Elems must come from precisely one of the passed EName sets.
   *
   * As a consequence, an empty Elem sequence is not considered to be a choice.
   */
  private def isChoice(elems: Seq[indexed.Elem], enameSets: Set[Set[EName]]): Boolean = {
    require(enameSets.toSeq.map(_.size).sum == enameSets.flatten.toSet.size, "The EName sets must not be overlapping")

    val usedENames = elems.map(_.resolvedName).toSet
    val usedENameSets = enameSets filter { enameSet => !usedENames.intersect(enameSet).isEmpty }

    usedENameSets.size == 1
  }

  /**
   * Returns `(elems.isEmpty) || isChoice(elems, enameSets)`
   */
  private def isOptionalChoice(elems: Seq[indexed.Elem], enameSets: Set[Set[EName]]): Boolean = {
    (elems.isEmpty) || isChoice(elems, enameSets)
  }

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
