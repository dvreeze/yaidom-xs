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
import eu.cdevreeze.yaidom._
import SchemaObject._

/**
 * Immutable XML Schema component. Inspired both by the Apache Common XML Schema API 2.0 and XSOM.
 * See http://ws.apache.org/commons/xmlschema20/xmlschema-core/apidocs/overview-summary.html and
 * http://xsom.java.net/nonav/javadoc/index.html, respectively, for both APIs.
 *
 * This represents only schema file content, without resolving imports and includes, and without
 * resolving types, substitution groups, etc. The latter requires an appropriate collection of schema documents
 * (closed under imports and includes).
 *
 * TODO Use yaidom trait HasParent.
 *
 * @author Chris de Vreeze
 */
sealed class SchemaObject(
  val wrappedElem: indexed.Elem) extends ElemLike[SchemaObject] with SchemaObject.HasParent[SchemaObject] with HasText with Immutable {

  final override def allChildElems: immutable.IndexedSeq[SchemaObject] = {
    wrappedElem.allChildElems map { e => SchemaObject(e) }
  }

  final override def resolvedName: EName = wrappedElem.resolvedName

  final override def resolvedAttributes: immutable.IndexedSeq[(EName, String)] = wrappedElem.resolvedAttributes

  final override def equals(obj: Any): Boolean = obj match {
    case other: SchemaObject => (other.wrappedElem == this.wrappedElem)
    case _ => false
  }

  final override def hashCode: Int = wrappedElem.hashCode

  final override def text: String = wrappedElem.text

  final override def parentOption: Option[SchemaObject] =
    wrappedElem.parentOption map { e => SchemaObject(e) }

  final override def toString: String = wrappedElem.elem.toString
}

/**
 * XML Schema (from one document). That is, the "schema" XML element.
 */
final class Schema(override val wrappedElem: indexed.Elem) extends SchemaObject(wrappedElem) {
  require(wrappedElem.resolvedName == EName(ns, "schema"))
  require(wrappedElem.elemPath.isRoot)

  final def targetNamespaceOption: Option[String] = wrappedElem \@ EName("targetNamespace")

  final def elementDeclarations: immutable.IndexedSeq[ElementDeclaration] =
    this filterElemsOrSelf { e => e.resolvedName == EName(ns, "element") } collect { case e: ElementDeclaration => e }

  final def globalElementDeclarations: immutable.IndexedSeq[ElementDeclaration] =
    elementDeclarations filter { e => e.wrappedElem.elemPath.entries.size == 1 }
}

/**
 * Particle, having a min and max occurs (possibly default).
 */
abstract class Particle(override val wrappedElem: indexed.Elem) extends SchemaObject(wrappedElem) {

  final def minOccurs: String = (wrappedElem \@ EName("minOccurs")).getOrElse(1.toString)

  final def maxOccursOption: String = (wrappedElem \@ EName("maxOccurs")).getOrElse(1.toString)
}

/**
 * Element declaration. That is, the "element" XML element.
 */
final class ElementDeclaration(override val wrappedElem: indexed.Elem) extends Particle(wrappedElem) {
  require(wrappedElem.resolvedName == EName(ns, "element"))

  final def isTopLevel: Boolean = wrappedElem.elemPath.entries.size == 1

  final def enameOption: Option[EName] = {
    val tnsOption = wrappedElem.rootElem \@ EName("targetNamespace")
    val localNameOption = wrappedElem \@ EName("name")
    localNameOption map { nm => EName(tnsOption, nm) }
  }

  final def idOption: Option[String] = wrappedElem \@ EName("id")

  final def typeAttributeOption: Option[EName] = {
    val typeAttrStringOption = wrappedElem \@ EName("type")
    typeAttrStringOption map { tpe =>
      wrappedElem.elem.scope.resolveQNameOption(QName(tpe)).getOrElse(
        sys.error("Could not resolve type '%s' as expanded name".format(tpe)))
    }
  }

  final def substitutionGroupOption: Option[EName] = {
    val substGroupStringOption = wrappedElem \@ EName("substitutionGroup")
    substGroupStringOption map { substGroup =>
      wrappedElem.elem.scope.resolveQNameOption(QName(substGroup)).getOrElse(
        sys.error("Could not resolve substitution group '%s' as expanded name".format(substGroup)))
    }
  }

  final def abstractOption: Option[Boolean] = (wrappedElem \@ EName("abstract")) map (_.toBoolean)

  final def nillableOption: Option[Boolean] = (wrappedElem \@ EName("nillable")) map (_.toBoolean)

  final def refOption: Option[EName] = {
    val refOption = wrappedElem \@ EName("ref")
    refOption map { ref =>
      wrappedElem.elem.scope.resolveQNameOption(QName(ref)).getOrElse(
        sys.error("Could not resolve ref '%s' as expanded name".format(ref)))
    }
  }
}

/**
 * Schema type, which is either a simple type or a complex type.
 */
abstract class SchemaType(override val wrappedElem: indexed.Elem) extends SchemaObject(wrappedElem) {
}

/**
 * Simple type. That is, the "simpleType" XML element.
 */
final class SimpleType(override val wrappedElem: indexed.Elem) extends SchemaType(wrappedElem) {
  require(wrappedElem.resolvedName == EName(ns, "simpleType"))
}

/**
 * Complex type. That is, the "complexType" XML element.
 */
final class ComplexType(override val wrappedElem: indexed.Elem) extends SchemaType(wrappedElem) {
  require(wrappedElem.resolvedName == EName(ns, "complexType"))
}

object SchemaObject {

  trait HasParent[E <: HasParent[E]] { self: E =>

    /**
     * Returns the parent element, if any, wrapped in an Option
     */
    def parentOption: Option[E]

    /**
     * Returns the equivalent `parentOption.get`, throwing an exception if this is the root element
     */
    final def parent: E = parentOption.getOrElse(sys.error("There is no parent element"))

    /**
     * Returns all ancestor elements or self
     */
    final def ancestorsOrSelf: immutable.IndexedSeq[E] =
      self +: (parentOption.toIndexedSeq flatMap ((e: E) => e.ancestorsOrSelf))

    /**
     * Returns `ancestorsOrSelf.drop(1)`
     */
    final def ancestors: immutable.IndexedSeq[E] = ancestorsOrSelf.drop(1)

    /**
     * Returns the first found ancestor-or-self element obeying the given predicate, if any, wrapped in an Option
     */
    final def findAncestorOrSelf(p: E => Boolean): Option[E] = ancestorsOrSelf find p

    /**
     * Returns the first found ancestor element obeying the given predicate, if any, wrapped in an Option
     */
    final def findAncestor(p: E => Boolean): Option[E] = ancestors find p
  }

  def apply(wrappedElem: indexed.Elem): SchemaObject = wrappedElem match {
    // TODO
    case e if e.resolvedName == EName(ns, "schema") => Schema(wrappedElem)
    case e if e.resolvedName == EName(ns, "element") => ElementDeclaration(wrappedElem)
    case e if e.resolvedName == EName(ns, "simpleType") => SimpleType(wrappedElem)
    case e if e.resolvedName == EName(ns, "complexType") => ComplexType(wrappedElem)
    case _ => new SchemaObject(wrappedElem)
  }
}

object Schema {

  def apply(wrappedElem: indexed.Elem): Schema = {
    require(wrappedElem.resolvedName == EName(ns, "schema"))
    new Schema(wrappedElem)
  }
}

object ElementDeclaration {

  def apply(wrappedElem: indexed.Elem): ElementDeclaration = {
    require(wrappedElem.resolvedName == EName(ns, "element"))
    new ElementDeclaration(wrappedElem)
  }
}

object SimpleType {

  def apply(wrappedElem: indexed.Elem): SimpleType = {
    require(wrappedElem.resolvedName == EName(ns, "simpleType"))
    new SimpleType(wrappedElem)
  }
}

object ComplexType {

  def apply(wrappedElem: indexed.Elem): ComplexType = {
    require(wrappedElem.resolvedName == EName(ns, "complexType"))
    new ComplexType(wrappedElem)
  }
}
