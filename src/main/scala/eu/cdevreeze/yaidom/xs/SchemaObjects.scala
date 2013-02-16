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
  def checkSchemaElem(e: indexed.Elem): Unit = {
    require(e.resolvedName == EName(ns, "schema"), "The element must be a 'schema' element")
    require(e.elemPath.isRoot, "The element must be the root of the element tree")

    val childElems = e.allChildElems

    val expectedChildENames = Set(
      EName(ns, "include"),
      EName(ns, "import"),
      EName(ns, "redefine"),
      EName(ns, "annotation"),
      EName(ns, "element"),
      EName(ns, "attribute"),
      EName(ns, "notation"),
      EName(ns, "simpleType"),
      EName(ns, "complexType"),
      EName(ns, "group"),
      EName(ns, "attributeGroup"))
    require(childElems.map(_.resolvedName).toSet.subsetOf(expectedChildENames),
      "Expected 'schema' child elements: %s".format(expectedChildENames.mkString(", ")))

    val expectedFirstChildNames = Set(EName(ns, "include"), EName(ns, "import"), EName(ns, "redefine"))

    val childElemsWithoutAnnotations = childElems filterNot { e => e.resolvedName == EName(ns, "annotation") }
    val childElems1 = childElemsWithoutAnnotations filter (e => expectedFirstChildNames.contains(e.resolvedName))
    val childElems2 = childElemsWithoutAnnotations filterNot (e => expectedFirstChildNames.contains(e.resolvedName))

    require(
      childElems1.map(_.resolvedName) ++ childElems2.map(_.resolvedName) == childElemsWithoutAnnotations.map(_.resolvedName),
      "Expected 'include', 'import' and 'redefine' child elements to come before the others (ignoring annotations)")
  }

  /**
   * Checks the XML element as XML Schema element declaration, throwing an exception if invalid.
   */
  def checkElementDeclarationElem(e: indexed.Elem): Unit = {
    def isTopLevel: Boolean = e.elemPath.entries.size == 1

    def isReference: Boolean = refOption(e).isDefined

    def isAbstract: Boolean = abstractOption(e) == Some(true)

    require(e.resolvedName == EName(ns, "element"), "The element must be an 'element' element")

    val childElems = e.allChildElems

    val expectedChildENames = Set(
      EName(ns, "simpleType"),
      EName(ns, "complexType"),
      EName(ns, "annotation"),
      EName(ns, "unique"),
      EName(ns, "key"),
      EName(ns, "keyref"))
    require(childElems.map(_.resolvedName).toSet.subsetOf(expectedChildENames),
      "Expected 'element' child elements: %s".format(expectedChildENames.mkString(", ")))

    val expectedFirstChildNames = Set(EName(ns, "simpleType"), EName(ns, "complexType"))

    require(childElems.count(e => expectedFirstChildNames.contains(e.resolvedName)) <= 1,
      "At most one complex/simple type definition child allowed")

    val childElemsWithoutAnnotations = childElems filterNot { e => e.resolvedName == EName(ns, "annotation") }
    val childElems1 = childElemsWithoutAnnotations filter (e => expectedFirstChildNames.contains(e.resolvedName))
    val childElems2 = childElemsWithoutAnnotations filterNot (e => expectedFirstChildNames.contains(e.resolvedName))

    require(
      childElems1.map(_.resolvedName) ++ childElems2.map(_.resolvedName) == childElemsWithoutAnnotations.map(_.resolvedName),
      "Expected 'simpleType' and 'complexType' child elements to come before the others (ignoring annotations)")

    if (isTopLevel) {
      require(minOccursAttrOption(e).isEmpty, "Top level element declarations are not particles, so have no 'minOccurs'")
      require(maxOccursAttrOption(e).isEmpty, "Top level element declarations are not particles, so have no 'maxOccurs'")

      require(!isReference, "Top level element declarations can not be references")
      require(e.attributeOption(EName("form")).isEmpty, "Top level element declarations can not have a 'form' attribute")

      require(nameOption(e).isDefined, "Top level element declarations must have a name attribute")
    } else {
      require(refOption(e).isDefined || nameOption(e).isDefined, "One of 'ref' or 'name' must be present")
      require(refOption(e).isEmpty || nameOption(e).isEmpty, "One of 'ref' or 'name' must be absent")

      require(substitutionGroupOption(e).isEmpty, "Local element declarations must not have a 'substitutionGroup'")
      require(e.attributeOption(EName("abstract")).isEmpty, "Local element declarations must not have attribute 'abstract'")
      require(e.attributeOption(EName("final")).isEmpty, "Local element declarations must not have attribute 'final'")
    }

    require(
      (e \@ EName("default")).isEmpty || (e \@ EName("fixed")).isEmpty,
      "Element declarations can not have both 'default' and 'fixed'")

    require(
      ((e \@ EName("type")).isEmpty) ||
        ((e \ EName("simpleType")).isEmpty && (e \ EName("complexType")).isEmpty),
      "Element declarations can not have both a 'type' attribute and a 'complexType' or 'simpleType' child element")

    if (isReference) {
      assert(!isTopLevel)
      require((e.resolvedAttributes.toMap.keySet intersect Set(
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
      require((e.allChildElems.map(_.resolvedName).toSet intersect Set(
        EName(ns, "complexType"),
        EName(ns, "simpleType"),
        EName(ns, "key"),
        EName(ns, "keyref"),
        EName(ns, "unique"))).isEmpty,
        "Element declarations that are references must not have child elements 'complexType', 'simpleType', 'key', 'keyref', 'unique'")
    }
  }

  /**
   * Checks the XML element as XML Schema particle, throwing an exception if invalid.
   */
  def checkParticleElem(e: indexed.Elem): Unit = {
    require(minOccursAttrOption(e).getOrElse("") forall (_.isDigit), "@minOccurs must be a non-negative integer")
    require((maxOccursAttrOption(e).getOrElse("").toLowerCase(java.util.Locale.ENGLISH) == "unbounded") ||
      (maxOccursAttrOption(e).getOrElse("") forall (_.isDigit)),
      "@maxOccurs must be a non-negative integer, or 'unbounded'")
  }

  /**
   * Checks the XML element as XML Schema attribute declaration, throwing an exception if invalid.
   */
  def checkAttributeDeclarationElem(e: indexed.Elem): Unit = {
    def isTopLevel: Boolean = e.elemPath.entries.size == 1

    def isReference: Boolean = refOption(e).isDefined

    require(e.resolvedName == EName(ns, "attribute"), "The element must be an 'attribute' element")

    val childElems = e.allChildElems

    val expectedChildENames = Set(EName(ns, "annotation"), EName(ns, "simpleType"))
    require(childElems.map(_.resolvedName).toSet.subsetOf(expectedChildENames),
      "Expected 'element' child elements: %s".format(expectedChildENames.mkString(", ")))

    require(
      childElems.filter(_.resolvedName == EName(ns, "annotation")).map(_.resolvedName) ++
        childElems.filter(_.resolvedName == EName(ns, "simpleType")).map(_.resolvedName) ==
        childElems.map(_.resolvedName),
      "Expected optional 'annotation' before optional 'simpleType'")

    // TODO

    if (isTopLevel) {
      // TODO Attribute 'use' prohibited

      require(!isReference, "Top level attribute declarations can not be references")
      require(e.attributeOption(EName("form")).isEmpty, "Top level attribute declarations can not have a 'form' attribute")

      require(nameOption(e).isDefined, "Top level attribute declarations must have a name attribute")
    } else {
      require(refOption(e).isDefined || nameOption(e).isDefined, "One of 'ref' or 'name' must be present")
      require(refOption(e).isEmpty || nameOption(e).isEmpty, "One of 'ref' or 'name' must be absent")
    }

    require(
      (e \@ EName("default")).isEmpty || (e \@ EName("fixed")).isEmpty,
      "Attribute declarations can not have both 'default' and 'fixed'")

    if ((e \@ EName("default")).isDefined && (e \@ EName("use")).isDefined) {
      require(e.attribute(EName("use")) == "optional", "If attributes 'default' and 'use' are both present, 'use' must have value 'optional'")
    }

    if (isReference) {
      assert(!isTopLevel)
      require((e.resolvedAttributes.toMap.keySet intersect Set(EName("form"), EName("type"))).isEmpty,
        "Attribute declarations that are references must not have attributes 'form', 'type'")
    }

    if (isReference) {
      assert(!isTopLevel)
      require((e.allChildElems.map(_.resolvedName).toSet intersect Set(EName(ns, "simpleType"))).isEmpty,
        "Attribute declarations that are references must not have child element 'simpleType'")
    }
  }

  /**
   * Checks the XML element as XML Schema attribute-use, throwing an exception if invalid.
   */
  def checkAttributeUseElem(e: indexed.Elem): Unit = {
    // TODO
  }

  /**
   * Checks the XML element as XML Schema simple type definition, throwing an exception if invalid.
   */
  def checkSimpleTypeDefinitionElem(e: indexed.Elem): Unit = {
    require(e.resolvedName == EName(ns, "simpleType"), "The element must be a 'simpleType' element")
  }

  /**
   * Checks the XML element as XML Schema complex type definition, throwing an exception if invalid.
   */
  def checkComplexTypeDefinitionElem(e: indexed.Elem): Unit = {
    require(e.resolvedName == EName(ns, "complexType"), "The element must be a 'complexType' element")
  }

  /**
   * Checks the XML element as XML Schema attribute group definition, throwing an exception if invalid.
   */
  def checkAttributeGroupDefinitionElem(e: indexed.Elem): Unit = {
    require(e.resolvedName == EName(ns, "attributeGroup"), "The element must be a 'attributeGroup' element")
  }

  /**
   * Checks the XML element as XML Schema identity constraint definition, throwing an exception if invalid.
   */
  def checkIdentityConstraintDefinitionElem(e: indexed.Elem): Unit = {
    val expectedENames = Set(EName(ns, "key"), EName(ns, "keyref"), EName(ns, "unique"))
    require(expectedENames.contains(e.resolvedName), "The element must be a 'key', 'keyref' or 'unique' element")
  }

  /**
   * Checks the XML element as XML Schema model group definition, throwing an exception if invalid.
   */
  def checkModelGroupDefinitionElem(e: indexed.Elem): Unit = {
    require(e.resolvedName == EName(ns, "group"), "The element must be a 'group' element")
  }

  /**
   * Checks the XML element as XML Schema notation declaration, throwing an exception if invalid.
   */
  def checkNotationDeclarationElem(e: indexed.Elem): Unit = {
    require(e.resolvedName == EName(ns, "notation"), "The element must be a 'notation' element")
  }

  /**
   * Checks the XML element as XML Schema model group, throwing an exception if invalid.
   */
  def checkModelGroupElem(e: indexed.Elem): Unit = {
    val expectedENames = Set(EName(ns, "all"), EName(ns, "sequence"), EName(ns, "choice"))
    require(expectedENames.contains(e.resolvedName), "The element must be a 'all', 'sequence' or 'choice' element")
  }

  /**
   * Checks the XML element as XML Schema wildcard, throwing an exception if invalid.
   */
  def checkWildcardElem(e: indexed.Elem): Unit = {
    val expectedENames = Set(EName(ns, "any"), EName(ns, "anyAttribute"))
    require(expectedENames.contains(e.resolvedName), "The element must be a 'any' or 'anyAttribute' element")
  }

  /**
   * Checks the XML element as XML Schema annotation, throwing an exception if invalid.
   */
  def checkAnnotationElem(e: indexed.Elem): Unit = {
    require(e.resolvedName == EName(ns, "annotation"),
      "Expected <xs:annotation> but got %s instead".format(e.resolvedName))

    val expectedChildNames = Set(EName(ns, "appinfo"), EName(ns, "documentation"))
    val unexpectedChildNames = (e.allChildElems map (_.resolvedName)).toSet diff expectedChildNames

    require(e.allChildElems forall (e =>
      expectedChildNames.contains(e.resolvedName)),
      "Unexpected child elements %s of <xs:annotation>".format(unexpectedChildNames))
  }

  /**
   * Returns the value of the 'name' attribute, if any, wrapped in an Option.
   */
  def nameOption(e: indexed.Elem): Option[String] = e \@ EName("name")

  /**
   * Returns the value of the 'id' attribute, if any, wrapped in an Option.
   */
  def idOption(e: indexed.Elem): Option[String] = e \@ EName("id")

  /**
   * Returns the value of the 'type' attribute as expanded name, if any, wrapped in an Option.
   */
  def typeAttributeOption(e: indexed.Elem): Option[EName] = {
    val typeAttrAttrOption = e \@ EName("type")
    typeAttrAttrOption map { tpe =>
      e.elem.scope.resolveQNameOption(QName(tpe)).getOrElse(
        sys.error("Could not resolve type '%s' as expanded name".format(tpe)))
    }
  }

  /**
   * Returns the value of the 'substitutionGroup' attribute as expanded name, if any, wrapped in an Option.
   */
  def substitutionGroupOption(e: indexed.Elem): Option[EName] = {
    val substGroupAttrOption = e \@ EName("substitutionGroup")
    substGroupAttrOption map { substGroup =>
      e.elem.scope.resolveQNameOption(QName(substGroup)).getOrElse(
        sys.error("Could not resolve substitution group '%s' as expanded name".format(substGroup)))
    }
  }

  /**
   * Returns the value of the 'abstract' attribute, if any, wrapped in an Option.
   */
  def abstractOption(e: indexed.Elem): Option[Boolean] = {
    try {
      (e \@ EName("abstract")) map (_.toBoolean)
    } catch {
      case e: Exception => None
    }
  }

  /**
   * Returns the value of the 'nillable' attribute, if any, wrapped in an Option.
   */
  def nillableOption(e: indexed.Elem): Option[Boolean] = {
    try {
      (e \@ EName("nillable")) map (_.toBoolean)
    } catch {
      case e: Exception => None
    }
  }

  /**
   * Returns the value of the 'ref' attribute as expanded name, if any, wrapped in an Option.
   */
  def refOption(e: indexed.Elem): Option[EName] = {
    val refOption = e \@ EName("ref")
    refOption map { ref =>
      e.elem.scope.resolveQNameOption(QName(ref)).getOrElse(
        sys.error("Could not resolve ref '%s' as expanded name".format(ref)))
    }
  }

  /**
   * Returns the value of the 'minOccurs' attribute, if any, wrapped in an Option.
   */
  def minOccursAttrOption(e: indexed.Elem): Option[String] = (e \@ EName("minOccurs"))

  /**
   * Returns the value of the 'maxOccurs' attribute, if any, wrapped in an Option.
   */
  def maxOccursAttrOption(e: indexed.Elem): Option[String] = (e \@ EName("maxOccurs"))
}
