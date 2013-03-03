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

import java.net.URI
import scala.collection.immutable

/**
 * Constraints on XML Schema parts, expressed at the level of `indexed.Elem`, and some helper functions.
 *
 * @author Chris de Vreeze
 */
private[xs] object SchemaObjects {

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
    def isTopLevel: Boolean = elem.elemPath.entries.size == 1

    def isReference: Boolean = refOption(elem).isDefined

    def isAbstract: Boolean = abstractOption(elem) == Some(true)

    if (isTopLevel) {
      checkTopLevelElementDeclarationElemAgainstSchema(elem)
    } else {
      checkLocalElementDeclarationElemAgainstSchema(elem)
    }

    if (isTopLevel) {
      require(minOccursAttrOption(elem).isEmpty, "Top level element declarations are not particles, so have no 'minOccurs'")
      require(maxOccursAttrOption(elem).isEmpty, "Top level element declarations are not particles, so have no 'maxOccurs'")

      require(!isReference, "Top level element declarations can not be references")
      require(elem.attributeOption(EName("form")).isEmpty, "Top level element declarations can not have a 'form' attribute")

      require(nameOption(elem).isDefined, "Top level element declarations must have a name attribute")
    } else {
      require(refOption(elem).isDefined || nameOption(elem).isDefined, "One of 'ref' or 'name' must be present")
      require(refOption(elem).isEmpty || nameOption(elem).isEmpty, "One of 'ref' or 'name' must be absent")

      require(substitutionGroupOption(elem).isEmpty, "Local element declarations must not have a 'substitutionGroup'")
      require(elem.attributeOption(EName("abstract")).isEmpty, "Local element declarations must not have attribute 'abstract'")
      require(elem.attributeOption(EName("final")).isEmpty, "Local element declarations must not have attribute 'final'")
    }

    require(
      (elem \@ EName("default")).isEmpty || (elem \@ EName("fixed")).isEmpty,
      "Element declarations can not have both 'default' and 'fixed'")

    require(
      ((elem \@ EName("type")).isEmpty) ||
        ((elem \ EName("simpleType")).isEmpty && (elem \ EName("complexType")).isEmpty),
      "Element declarations can not have both a 'type' attribute and a 'complexType' or 'simpleType' child element")

    if (isReference) {
      assert(!isTopLevel)
      require((elem.resolvedAttributes.toMap.keySet intersect Set(
        EName("nillable"),
        EName("default"),
        EName("fixed"),
        EName("form"),
        EName("block"),
        EName("type"))).isEmpty,
        "Element declarations that are references must not have attributes 'nillable', 'default', 'fixed', 'form', 'block', 'type'")
    }

    if (isReference) {
      assert(!isTopLevel)
      require((elem.allChildElems.map(_.resolvedName).toSet intersect Set(
        enameComplexType,
        enameSimpleType,
        enameKey,
        enameKeyref,
        enameUnique)).isEmpty,
        "Element declarations that are references must not have child elements 'complexType', 'simpleType', 'key', 'keyref', 'unique'")
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
   * Checks the XML element as XML Schema attribute declaration, throwing an exception if invalid.
   */
  def checkAttributeDeclarationElem(elem: indexed.Elem): Unit = {
    def isTopLevel: Boolean = elem.elemPath.entries.size == 1

    def isReference: Boolean = refOption(elem).isDefined

    if (isTopLevel) {
      checkTopLevelAttributeDeclarationElemAgainstSchema(elem)
    } else {
      checkLocalAttributeDeclarationElemAgainstSchema(elem)
    }

    // TODO

    if (isTopLevel) {
      // TODO Attribute 'use' prohibited

      require(!isReference, "Top level attribute declarations can not be references")
      require(elem.attributeOption(EName("form")).isEmpty, "Top level attribute declarations can not have a 'form' attribute")

      require(nameOption(elem).isDefined, "Top level attribute declarations must have a name attribute")
    } else {
      require(refOption(elem).isDefined || nameOption(elem).isDefined, "One of 'ref' or 'name' must be present")
      require(refOption(elem).isEmpty || nameOption(elem).isEmpty, "One of 'ref' or 'name' must be absent")
    }

    require(
      (elem \@ EName("default")).isEmpty || (elem \@ EName("fixed")).isEmpty,
      "Attribute declarations can not have both 'default' and 'fixed'")

    if ((elem \@ EName("default")).isDefined && (elem \@ EName("use")).isDefined) {
      require(elem.attribute(EName("use")) == "optional", "If attributes 'default' and 'use' are both present, 'use' must have value 'optional'")
    }

    if (isReference) {
      assert(!isTopLevel)
      require((elem.resolvedAttributes.toMap.keySet intersect Set(EName("form"), EName("type"))).isEmpty,
        "Attribute declarations that are references must not have attributes 'form', 'type'")
    }

    if (isReference) {
      assert(!isTopLevel)
      require((elem.allChildElems.map(_.resolvedName).toSet intersect Set(enameSimpleType)).isEmpty,
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
   * Checks the XML element as XML Schema simple type definition, throwing an exception if invalid.
   */
  def checkSimpleTypeDefinitionElem(elem: indexed.Elem): Unit = {
    def isTopLevel: Boolean = elem.elemPath.entries.size == 1

    if (isTopLevel) {
      checkTopLevelSimpleTypeDefinitionElemAgainstSchema(elem)
    } else {
      checkLocalSimpleTypeDefinitionElemAgainstSchema(elem)
    }
  }

  /**
   * Checks the XML element as XML Schema complex type definition, throwing an exception if invalid.
   */
  def checkComplexTypeDefinitionElem(elem: indexed.Elem): Unit = {
    def isTopLevel: Boolean = elem.elemPath.entries.size == 1

    if (isTopLevel) {
      checkTopLevelComplexTypeDefinitionElemAgainstSchema(elem)
    } else {
      checkLocalComplexTypeDefinitionElemAgainstSchema(elem)
    }
  }

  /**
   * Checks the XML element as XML Schema attribute group definition, throwing an exception if invalid.
   */
  def checkAttributeGroupDefinitionElem(elem: indexed.Elem): Unit = {
    def isTopLevel: Boolean = elem.elemPath.entries.size == 1

    def isRedefineChild: Boolean = elem.parentOption.map(_.resolvedName) == Some(EName(ns, "redefine"))

    if (isTopLevel || isRedefineChild) {
      checkNamedAttributeGroupElemAgainstSchema(elem)
    } else {
      checkNamedAttributeGroupRefElemAgainstSchema(elem)
    }
  }

  /**
   * Checks the XML element as XML Schema identity constraint definition, throwing an exception if invalid.
   */
  def checkIdentityConstraintDefinitionElem(elem: indexed.Elem): Unit = {
    val expectedENames = Set(enameKey, enameKeyref, enameUnique)
    require(expectedENames.contains(elem.resolvedName), "The element must be a 'key', 'keyref' or 'unique' element")
  }

  /**
   * Checks the XML element as XML Schema model group definition, throwing an exception if invalid.
   */
  def checkModelGroupDefinitionElem(elem: indexed.Elem): Unit = {
    def isTopLevel: Boolean = elem.elemPath.entries.size == 1

    def isRedefineChild: Boolean = elem.parentOption.map(_.resolvedName) == Some(EName(ns, "redefine"))

    if (isTopLevel || isRedefineChild) {
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
    val expectedENames = Set(enameAll, enameSequence, enameChoice)
    require(expectedENames.contains(elem.resolvedName), "The element must be a 'all', 'sequence' or 'choice' element")
  }

  /**
   * Checks the XML element as XML Schema wildcard, throwing an exception if invalid.
   */
  def checkWildcardElem(elem: indexed.Elem): Unit = {
    val expectedENames = Set(EName(ns, "any"), enameAnyAttribute)
    require(expectedENames.contains(elem.resolvedName), "The element must be a 'any' or 'anyAttribute' element")
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
    require(elem.resolvedName == enameImport,
      "Expected <xs:import> but got %s instead".format(elem.resolvedName))
  }

  /**
   * Checks the XML element as "xs:include", throwing an exception if invalid.
   */
  def checkIncludeElem(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == enameInclude,
      "Expected <xs:include> but got %s instead".format(elem.resolvedName))

    require(elem.attributeOption(EName("schemaLocation")).isDefined, "Expected attribute 'schemaLocation'")
  }

  /**
   * Checks the XML element as "xs:redefine", throwing an exception if invalid.
   */
  def checkRedefineElem(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == enameRedefine,
      "Expected <xs:redefine> but got %s instead".format(elem.resolvedName))

    require(elem.attributeOption(EName("schemaLocation")).isDefined, "Expected attribute 'schemaLocation'")
  }

  // Public helper methods

  /**
   * Returns the value of the 'name' attribute, if any, wrapped in an Option.
   */
  def nameOption(elem: indexed.Elem): Option[String] = elem \@ EName("name")

  /**
   * Returns the value of the 'id' attribute, if any, wrapped in an Option.
   */
  def idOption(elem: indexed.Elem): Option[String] = elem \@ EName("id")

  /**
   * Returns the value of the 'type' attribute as expanded name, if any, wrapped in an Option.
   */
  def typeAttributeOption(elem: indexed.Elem): Option[EName] = {
    val typeAttrAttrOption = elem \@ EName("type")
    typeAttrAttrOption map { tpe =>
      elem.elem.scope.resolveQNameOption(QName(tpe)).getOrElse(
        sys.error("Could not resolve type '%s' as expanded name".format(tpe)))
    }
  }

  /**
   * Returns the value of the 'substitutionGroup' attribute as expanded name, if any, wrapped in an Option.
   */
  def substitutionGroupOption(elem: indexed.Elem): Option[EName] = {
    val substGroupAttrOption = elem \@ EName("substitutionGroup")
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
      (elem \@ EName("abstract")) map (_.toBoolean)
    } catch {
      case e: Exception => None
    }
  }

  /**
   * Returns the value of the 'nillable' attribute, if any, wrapped in an Option.
   */
  def nillableOption(elem: indexed.Elem): Option[Boolean] = {
    try {
      (elem \@ EName("nillable")) map (_.toBoolean)
    } catch {
      case e: Exception => None
    }
  }

  /**
   * Returns the value of the 'ref' attribute as expanded name, if any, wrapped in an Option.
   */
  def refOption(elem: indexed.Elem): Option[EName] = {
    val refOption = elem \@ EName("ref")
    refOption map { ref =>
      elem.elem.scope.resolveQNameOption(QName(ref)).getOrElse(
        sys.error("Could not resolve ref '%s' as expanded name".format(ref)))
    }
  }

  /**
   * Returns the value of the 'minOccurs' attribute, if any, wrapped in an Option.
   */
  def minOccursAttrOption(elem: indexed.Elem): Option[String] = (elem \@ EName("minOccurs"))

  /**
   * Returns the value of the 'maxOccurs' attribute, if any, wrapped in an Option.
   */
  def maxOccursAttrOption(elem: indexed.Elem): Option[String] = (elem \@ EName("maxOccurs"))

  // Validations against only the Schema of XML Schema itself, rewritten in Scala

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_schema.html
   */
  private def checkSchemaElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == enameSchema, "The element must be a 'schema' element")
    require(elem.elemPath.isRoot, "The element must be the root of the element tree")

    val childElems = elem.allChildElems

    val expectedChildENames = Set(
      enameInclude,
      enameImport,
      enameRedefine,
      enameAnnotation,
      enameElement,
      enameAttribute,
      enameNotation,
      enameSimpleType,
      enameComplexType,
      enameGroup,
      enameAttributeGroup)
    require(childElems.map(_.resolvedName).toSet.subsetOf(expectedChildENames),
      "Expected 'schema' child elements: %s".format(expectedChildENames.mkString(", ")))

    val expectedFirstChildNames = Set(enameInclude, enameImport, enameRedefine)
    val expectedRemainingChildNames = (expectedChildENames - enameAnnotation) diff expectedFirstChildNames

    val childElemsWithoutAnnotations = childElems filterNot { e => e.resolvedName == enameAnnotation }

    require(
      correctlyOrdered(
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
  private def checkTopLevelElementDeclarationElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.elemPath.entries.size == 1)

    checkElementDeclarationElemAgainstSchema(elem)

    // TODO Validate attributes and their types

    require(elem.attributeOption(EName("name")).isDefined, "Missing attribute 'name'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_element-1.html
   */
  private def checkLocalElementDeclarationElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.elemPath.entries.size >= 2)

    checkElementDeclarationElemAgainstSchema(elem)

    // TODO Validate attributes and their types
  }

  private def checkElementDeclarationElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == enameElement, "The element must be an 'element' element")

    val childElems = elem.allChildElems

    val expectedChildENames = Set(
      enameSimpleType,
      enameComplexType,
      enameAnnotation,
      enameUnique,
      enameKey,
      enameKeyref)
    require(childElems.map(_.resolvedName).toSet.subsetOf(expectedChildENames),
      "Expected 'element' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(enameSimpleType, enameComplexType).contains(e.resolvedName)) <= 1,
      "At most one complex/simple type definition child allowed")

    require(childElems.count(e => Set(enameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(
      correctlyOrdered(
        childElems,
        Seq(
          Set(enameAnnotation),
          Set(enameSimpleType, enameComplexType),
          Set(enameUnique, enameKey, enameKeyref))),
      "Expected 'simpleType' and 'complexType' child elements to come before the others (ignoring annotations)")

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_attribute.html
   */
  private def checkTopLevelAttributeDeclarationElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.elemPath.entries.size == 1)

    checkAttributeDeclarationElemAgainstSchema(elem)

    // TODO Validate attributes and their types

    require(elem.attributeOption(EName("name")).isDefined, "Missing attribute 'name'")
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
    require(elem.resolvedName == enameAttribute, "The element must be an 'attribute' element")

    val childElems = elem.allChildElems

    val expectedChildENames = Set(
      enameSimpleType,
      enameAnnotation)
    require(childElems.map(_.resolvedName).toSet.subsetOf(expectedChildENames),
      "Expected 'attribute' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(enameSimpleType).contains(e.resolvedName)) <= 1,
      "At most one complex/simple type definition child allowed")

    require(childElems.count(e => Set(enameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(
      correctlyOrdered(
        childElems,
        Seq(
          Set(enameAnnotation),
          Set(enameSimpleType))),
      "Expected 'simpleType' child element to come after the annotation, if any")

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_complexType.html
   */
  private def checkTopLevelComplexTypeDefinitionElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.elemPath.entries.size == 1)

    checkComplexTypeDefinitionElemAgainstSchema(elem)

    // TODO Validate attributes and their types

    require(elem.attributeOption(EName("name")).isDefined, "Missing attribute 'name'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_complexType-1.html
   */
  private def checkLocalComplexTypeDefinitionElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.elemPath.entries.size >= 2)

    checkComplexTypeDefinitionElemAgainstSchema(elem)

    // TODO Validate attributes and their types
  }

  private def checkComplexTypeDefinitionElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == enameComplexType, "The element must be a 'complexType' element")

    val childElems = elem.allChildElems

    val expectedChildENames = Set(
      enameAnnotation,
      enameSimpleContent,
      enameComplexContent,
      enameGroup,
      enameAll,
      enameChoice,
      enameSequence,
      enameAttribute,
      enameAttributeGroup,
      enameAnyAttribute)
    require(childElems.map(_.resolvedName).toSet.subsetOf(expectedChildENames),
      "Expected 'complexType' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(enameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(childElems.count(e => Set(enameSimpleContent, enameComplexContent).contains(e.resolvedName)) <= 1,
      "At most one complex/simple content child allowed")

    require(childElems.count(e => Set(enameGroup).contains(e.resolvedName)) <= 1,
      "At most one group child allowed")

    require(childElems.count(e => Set(enameAll).contains(e.resolvedName)) <= 1,
      "At most one all child allowed")

    require(childElems.count(e => Set(enameChoice).contains(e.resolvedName)) <= 1,
      "At most one choice child allowed")

    require(childElems.count(e => Set(enameSequence).contains(e.resolvedName)) <= 1,
      "At most one sequence child allowed")

    require(childElems.count(e => Set(enameAnyAttribute).contains(e.resolvedName)) <= 1,
      "At most one anyAttribute child allowed")

    require(
      correctlyOrdered(
        childElems,
        Seq(
          Set(enameAnnotation),
          (expectedChildENames - enameAnnotation))),
      "Expected 'annotation' child element, if any, to come before the others")

    val childElemsButAnnotation = childElems filterNot (e => e.resolvedName == enameAnnotation)

    require(
      isChoice(
        childElemsButAnnotation,
        Set(
          Set(enameSimpleContent),
          Set(enameComplexContent),
          (expectedChildENames diff Set(enameAnnotation, enameSimpleContent, enameComplexContent)))),
      "Expected choice between 'simpleContent', 'complexContent' and a sequence starting with a model group (definition)")

    val childElemsButAnnotationAndContent = childElems filterNot { e =>
      Set(enameAnnotation, enameSimpleContent, enameComplexContent).contains(e.resolvedName)
    }

    require(
      correctlyOrdered(
        childElemsButAnnotationAndContent,
        Seq(
          Set(enameGroup, enameAll, enameChoice, enameSequence),
          Set(enameAttribute, enameAttributeGroup),
          Set(enameAnyAttribute))),
      "Expected 'attribute' and 'attributeGroup' child elements, if any, to come after 'group', 'all', 'choice' and 'sequence'")

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_simpleType.html
   */
  private def checkTopLevelSimpleTypeDefinitionElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.elemPath.entries.size == 1)

    checkSimpleTypeDefinitionElemAgainstSchema(elem)

    // TODO Validate attributes and their types

    require(elem.attributeOption(EName("name")).isDefined, "Missing attribute 'name'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_simpleType-1.html
   */
  private def checkLocalSimpleTypeDefinitionElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.elemPath.entries.size >= 2)

    checkSimpleTypeDefinitionElemAgainstSchema(elem)

    // TODO Validate attributes and their types
  }

  private def checkSimpleTypeDefinitionElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == enameSimpleType, "The element must be a 'simpleType' element")

    val childElems = elem.allChildElems

    val expectedChildENames = Set(
      enameAnnotation,
      enameRestriction,
      enameList,
      enameUnion)
    require(childElems.map(_.resolvedName).toSet.subsetOf(expectedChildENames),
      "Expected 'simpleType' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(enameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(childElems.count(e => Set(enameRestriction).contains(e.resolvedName)) <= 1,
      "At most one restriction child allowed")

    require(childElems.count(e => Set(enameList).contains(e.resolvedName)) <= 1,
      "At most one list child allowed")

    require(childElems.count(e => Set(enameUnion).contains(e.resolvedName)) <= 1,
      "At most one union child allowed")

    require(
      correctlyOrdered(
        childElems,
        Seq(
          Set(enameAnnotation),
          (expectedChildENames - enameAnnotation))),
      "Expected 'annotation' child element, if any, to come before the others")

    val childElemsButAnnotation = childElems filterNot (e => e.resolvedName == enameAnnotation)

    require(
      isChoice(
        childElemsButAnnotation,
        Set(
          Set(enameRestriction),
          Set(enameList),
          Set(enameUnion))),
      "Expected choice between 'restriction', 'list' and 'union'")

    // TODO Validate attributes and their types
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_group.html
   */
  private def checkNamedModelGroupElemAgainstSchema(elem: indexed.Elem): Unit = {
    require((elem.elemPath.entries.size == 1) || (elem.parentOption.map(_.resolvedName) == Some(EName(ns, "redefine"))))

    require(elem.resolvedName == enameGroup, "The element must be a 'group' element")

    val childElems = elem.allChildElems

    val expectedChildENames = Set(
      enameAnnotation,
      enameAll,
      enameChoice,
      enameSequence)
    require(childElems.map(_.resolvedName).toSet.subsetOf(expectedChildENames),
      "Expected 'group' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(enameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(childElems.count(e => Set(enameAll).contains(e.resolvedName)) <= 1,
      "At most one all child allowed")

    require(childElems.count(e => Set(enameChoice).contains(e.resolvedName)) <= 1,
      "At most one choice child allowed")

    require(childElems.count(e => Set(enameSequence).contains(e.resolvedName)) <= 1,
      "At most one sequence child allowed")

    require(
      correctlyOrdered(
        childElems,
        Seq(
          Set(enameAnnotation),
          (expectedChildENames - enameAnnotation))),
      "Expected 'annotation' child element, if any, to come before the others")

    val childElemsButAnnotation = childElems filterNot (e => e.resolvedName == enameAnnotation)

    require(
      isChoice(
        childElemsButAnnotation,
        Set(
          Set(enameAll),
          Set(enameChoice),
          Set(enameSequence))),
      "Expected choice between 'all', 'choice' and 'sequence'")

    // TODO Validate attributes and their types

    require(elem.attributeOption(EName("name")).isDefined, "Missing attribute 'name'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_group-1.html
   */
  private def checkNamedModelGroupRefElemAgainstSchema(elem: indexed.Elem): Unit = {
    require((elem.elemPath.entries.size >= 2) && (elem.parent.resolvedName != EName(ns, "redefine")))

    require(elem.resolvedName == enameGroup, "The element must be a 'group' element")

    val childElems = elem.allChildElems

    val expectedChildENames = Set(
      enameAnnotation)
    require(childElems.map(_.resolvedName).toSet.subsetOf(expectedChildENames),
      "Expected 'group' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(enameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    // TODO Validate attributes and their types

    require(elem.attributeOption(EName("ref")).isDefined, "Missing attribute 'ref'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_attributeGroup.html
   */
  private def checkNamedAttributeGroupElemAgainstSchema(elem: indexed.Elem): Unit = {
    require((elem.elemPath.entries.size == 1) || (elem.parentOption.map(_.resolvedName) == Some(EName(ns, "redefine"))))

    require(elem.resolvedName == enameAttributeGroup, "The element must be an 'attributeGroup' element")

    val childElems = elem.allChildElems

    val expectedChildENames = Set(
      enameAnnotation,
      enameAttribute,
      enameAttributeGroup,
      enameAnyAttribute)
    require(childElems.map(_.resolvedName).toSet.subsetOf(expectedChildENames),
      "Expected 'attributeGroup' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(enameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    require(childElems.count(e => Set(enameAnyAttribute).contains(e.resolvedName)) <= 1,
      "At most one anyAttribute child allowed")

    require(
      correctlyOrdered(
        childElems,
        Seq(
          Set(enameAnnotation),
          Set(enameAttribute, enameAttributeGroup),
          Set(enameAnyAttribute))),
      "Expected 'attribute' and 'attributeGroup' child elements, if any, to come after 'annotation' and before 'anyAttribute'")

    // TODO Validate attributes and their types

    require(elem.attributeOption(EName("name")).isDefined, "Missing attribute 'name'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_attributeGroup-1.html
   */
  private def checkNamedAttributeGroupRefElemAgainstSchema(elem: indexed.Elem): Unit = {
    require((elem.elemPath.entries.size >= 2) && (elem.parent.resolvedName != EName(ns, "redefine")))

    require(elem.resolvedName == enameAttributeGroup, "The element must be an 'attributeGroup' element")

    val childElems = elem.allChildElems

    val expectedChildENames = Set(
      enameAnnotation)
    require(childElems.map(_.resolvedName).toSet.subsetOf(expectedChildENames),
      "Expected 'attributeGroup' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(enameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    // TODO Validate attributes and their types

    require(elem.attributeOption(EName("ref")).isDefined, "Missing attribute 'ref'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_notation.html
   */
  private def checkNotationDeclarationElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == enameNotation, "The element must be a 'notation' element")

    val childElems = elem.allChildElems

    val expectedChildENames = Set(
      enameAnnotation)
    require(childElems.map(_.resolvedName).toSet.subsetOf(expectedChildENames),
      "Expected 'notation' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(childElems.count(e => Set(enameAnnotation).contains(e.resolvedName)) <= 1,
      "At most one annotation child allowed")

    // TODO Validate attributes and their types

    require(elem.attributeOption(EName("name")).isDefined, "Missing attribute 'name'")
  }

  /**
   * See http://www.schemacentral.com/sc/xsd/e-xsd_annotation.html
   */
  private def checkAnnotationElemAgainstSchema(elem: indexed.Elem): Unit = {
    require(elem.resolvedName == enameAnnotation, "The element must be an 'annotation' element")

    val childElems = elem.allChildElems

    val expectedChildENames = Set(
      enameAppinfo, enameDocumentation)
    require(childElems.map(_.resolvedName).toSet.subsetOf(expectedChildENames),
      "Expected 'annotation' child elements: %s".format(expectedChildENames.mkString(", ")))

    // TODO Validate attributes and their types
  }

  // Private constraint helper methods

  /**
   * Returns true if the passed Elems are ordered correctly, given the ordered sequence of EName sets.
   * That is, the first Elems start with ENames from `enameSetOrder(0)`, the next Elems have ENames from `enameSetOrder(1)`, etc.
   *
   * An empty Elem sequence is also considered to be correctly ordered.
   */
  private def correctlyOrdered(elems: Seq[indexed.Elem], enameSetOrder: Seq[Set[EName]]): Boolean = {
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

  // ENames

  val enameSchema = EName(ns, "schema")
  val enameElement = EName(ns, "element")
  val enameAttribute = EName(ns, "attribute")
  val enameComplexType = EName(ns, "complexType")
  val enameSimpleType = EName(ns, "simpleType")
  val enameAnnotation = EName(ns, "annotation")
  val enameComplexContent = EName(ns, "complexContent")
  val enameSimpleContent = EName(ns, "simpleContent")
  val enameGroup = EName(ns, "group")
  val enameAll = EName(ns, "all")
  val enameChoice = EName(ns, "choice")
  val enameSequence = EName(ns, "sequence")
  val enameAttributeGroup = EName(ns, "attributeGroup")
  val enameAnyAttribute = EName(ns, "anyAttribute")
  val enameUnique = EName(ns, "unique")
  val enameKey = EName(ns, "key")
  val enameKeyref = EName(ns, "keyref")
  val enameNotation = EName(ns, "notation")
  val enameImport = EName(ns, "import")
  val enameInclude = EName(ns, "include")
  val enameRedefine = EName(ns, "redefine")
  val enameRestriction = EName(ns, "restriction")
  val enameExtension = EName(ns, "extension")
  val enameList = EName(ns, "list")
  val enameUnion = EName(ns, "union")
  val enameAppinfo = EName(ns, "appinfo")
  val enameDocumentation = EName(ns, "documentation")
}
