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
  def checkSchema(e: indexed.Elem): Unit = {
    require(e.resolvedName == EName(ns, "schema"))
    require(e.elemPath.isRoot)
  }

  /**
   * Checks the XML element as XML Schema element declaration, throwing an exception if invalid.
   */
  def checkElementDeclaration(e: indexed.Elem): Unit = {
    def isTopLevel: Boolean = e.elemPath.entries.size == 1

    def isReference: Boolean = refOption(e).isDefined

    def isAbstract: Boolean = abstractOption(e) == Some(true)

    require(e.resolvedName == EName(ns, "element"))

    if (isTopLevel) {
      require(minOccursAttrOption(e).isEmpty, "Top level element declarations are not particles, so have no 'minOccurs'")
      require(maxOccursAttrOption(e).isEmpty, "Top level element declarations are not particles, so have no 'maxOccurs'")

      require(!isReference, "Top level element declarations can not be references")
      require(e.attributeOption(EName("form")).isEmpty, "Top level element declarations can not have a 'form' attribute")

      require(enameOption(e).isDefined, "Top level element declarations must have a name attribute")
    } else {
      require(refOption(e).isDefined || enameOption(e).isDefined, "One of 'ref' or 'name' must be present")
      require(refOption(e).isEmpty || enameOption(e).isEmpty, "One of 'ref' or 'name' must be absent")

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
  }

  /**
   * Checks the XML element as XML Schema particle, throwing an exception if invalid.
   */
  def checkParticle(e: indexed.Elem): Unit = {
    require(minOccursAttrOption(e).getOrElse("") forall (_.isDigit))
    require((maxOccursAttrOption(e).getOrElse("").toLowerCase(java.util.Locale.ENGLISH) == "unbounded") ||
      (maxOccursAttrOption(e).getOrElse("") forall (_.isDigit)))
  }

  /**
   * Checks the XML element as XML Schema simple type definition, throwing an exception if invalid.
   */
  def checkSimpleTypeDefinition(e: indexed.Elem): Unit = {
    require(e.resolvedName == EName(ns, "simpleType"))
  }

  /**
   * Checks the XML element as XML Schema complex type definition, throwing an exception if invalid.
   */
  def checkComplexTypeDefinition(e: indexed.Elem): Unit = {
    require(e.resolvedName == EName(ns, "complexType"))
  }

  /**
   * Checks the XML element as XML Schema annotation, throwing an exception if invalid.
   */
  def checkAnnotation(e: indexed.Elem): Unit = {
    require(e.resolvedName == EName(ns, "annotation"),
      "Expected <xs:annotation> but got %s instead".format(e.resolvedName))

    val expectedChildNames = Set(EName(ns, "appinfo"), EName(ns, "documentation"))
    val unexpectedChildNames = (e.allChildElems map (_.resolvedName)).toSet diff expectedChildNames

    require(e.allChildElems forall (e =>
      expectedChildNames.contains(e.resolvedName)),
      "Unexpected child elements %s of <xs:annotation>".format(unexpectedChildNames))
  }

  /**
   * Returns the `EName` by combining the (root) target namespace and the value of the "name" attribute,
   * if any, wrapped in an Option.
   */
  def enameOption(e: indexed.Elem): Option[EName] = {
    val tnsOption = e.rootElem \@ EName("targetNamespace")
    val localNameOption = e \@ EName("name")
    localNameOption map { nm => EName(tnsOption, nm) }
  }

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
