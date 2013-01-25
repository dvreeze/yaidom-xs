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
 * Immutable XML Schema or a part thereof.
 *
 * Terminology is taken as much as possible from the XML Schema specification Part 1 (especially section 2.2).
 *
 * Still, the API has been inspired both by the Apache Common XML Schema API 2.0 and XSOM.
 * See http://ws.apache.org/commons/xmlschema20/xmlschema-core/apidocs/overview-summary.html and
 * http://xsom.java.net/nonav/javadoc/index.html, respectively.
 *
 * This represents only schema file content, without resolving imports and includes, and without
 * resolving types, substitution groups, etc. The latter requires an appropriate collection of schema documents
 * (closed under imports and includes).
 *
 * TODO Use yaidom trait HasParent.
 *
 * @author Chris de Vreeze
 */
sealed abstract class SchemaObject(
  val wrappedElem: indexed.Elem) extends ElemLike[SchemaObject] with SchemaObject.HasParent[SchemaObject] with HasText with Immutable {

  require(wrappedElem ne null)
  require(wrappedElem.rootElem.resolvedName == EName(ns, "schema"))
  require((wrappedElem.resolvedName == EName(ns, "schema")) || (!wrappedElem.elemPath.isRoot))

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
 *
 * This is what the XML Schema specification calls a schema document, or the document element thereof.
 * In the abstract schema model of the specification, a schema is represented by one or more of these "schema documents".
 */
final class Schema(override val wrappedElem: indexed.Elem) extends SchemaObject(wrappedElem) {
  require(wrappedElem.resolvedName == EName(ns, "schema"))
  require(wrappedElem.elemPath.isRoot)

  // The following val variable contains and therefore validates the top level child elements
  final val childSchemaObjects: immutable.IndexedSeq[SchemaObject] = allChildElems

  final def targetNamespaceOption: Option[String] = wrappedElem \@ EName("targetNamespace")

  final def elementDeclarations: immutable.IndexedSeq[ElementDeclaration] =
    this filterElemsOrSelf { e => e.resolvedName == EName(ns, "element") } collect { case e: ElementDeclaration => e }

  final def globalElementDeclarations: immutable.IndexedSeq[ElementDeclaration] =
    elementDeclarations filter { e => e.isTopLevel }
}

// Schema Components

/**
 * Schema component, as specified in the XML Schema specification, part 1, section 2.2.
 *
 * There is an important difference, however. For example, in the abstract schema model, a particle contains a term, such
 * as an element declaration. In the concrete XML representation of the schema, the particle and the term that it contains,
 * such as an element declaration, are part of the same XML element. To keep close to the XML representation of XML Schema,
 * a term like an element declaration IS a particle, so a particle does NOT HAVE a term. More generally, it is the schema
 * XML document that is leading, and not the abstract schema model that has no knowledge about its XML representation.
 */
abstract class SchemaComponent(override val wrappedElem: indexed.Elem) extends SchemaObject(wrappedElem)

/**
 * Particle, having a min and max occurs (possibly default).
 */
abstract class Particle(override val wrappedElem: indexed.Elem) extends SchemaComponent(wrappedElem) {
  require(minOccursStringOption.getOrElse("") forall (_.isDigit))
  require((maxOccursStringOption.getOrElse("").toLowerCase(java.util.Locale.ENGLISH) == "unbounded") ||
    (maxOccursStringOption.getOrElse("") forall (_.isDigit)))

  final def minOccurs: Int = minOccursStringOption map (_.toInt) getOrElse 1

  final def maxOccurs: Int = {
    maxOccursStringOption map { v =>
      if (v.toLowerCase(java.util.Locale.ENGLISH) == "unbounded") Particle.Unbounded else 1
    } getOrElse 1
  }

  private final def minOccursStringOption: Option[String] = (wrappedElem \@ EName("minOccurs"))

  private final def maxOccursStringOption: Option[String] = (wrappedElem \@ EName("maxOccurs"))
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
 * Schema type definition, which is either a simple type or a complex type.
 */
abstract class TypeDefinition(override val wrappedElem: indexed.Elem) extends SchemaComponent(wrappedElem)

/**
 * Simple type definition. That is, the "simpleType" XML element.
 */
final class SimpleTypeDefinition(override val wrappedElem: indexed.Elem) extends TypeDefinition(wrappedElem) {
  require(wrappedElem.resolvedName == EName(ns, "simpleType"))
}

/**
 * Complex type definition. That is, the "complexType" XML element.
 */
final class ComplexTypeDefinition(override val wrappedElem: indexed.Elem) extends TypeDefinition(wrappedElem) {
  require(wrappedElem.resolvedName == EName(ns, "complexType"))
}

/**
 * Annotation schema component.
 */
final class Annotation(override val wrappedElem: indexed.Elem) extends SchemaComponent(wrappedElem) {
  require(wrappedElem.resolvedName == EName(ns, "annotation"),
    "Expected <xs:annotation> but got %s instead".format(wrappedElem.resolvedName))

  val expectedChildNames = Set(EName(ns, "appinfo"), EName(ns, "documentation"))
  val unexpectedChildNames = (wrappedElem.allChildElems map (_.resolvedName)).toSet diff expectedChildNames

  require(wrappedElem.allChildElems forall (e =>
    expectedChildNames.contains(e.resolvedName)),
    "Unexpected child elements %s of <xs:annotation>".format(unexpectedChildNames))
}

object SchemaComponent {

  def apply(elem: indexed.Elem): SchemaComponent = {
    wrapOption(elem).getOrElse(sys.error("%s is not a schema component".format(elem.resolvedName)))
  }

  def wrapOption(elem: indexed.Elem): Option[SchemaComponent] = elem match {
    // TODO
    case e if e.resolvedName == EName(ns, "element") => Some(new ElementDeclaration(e))
    case e if e.resolvedName == EName(ns, "simpleType") => Some(new SimpleTypeDefinition(e))
    case e if e.resolvedName == EName(ns, "complexType") => Some(new ComplexTypeDefinition(e))
    case e => None
  }
}

// Other schema parts, that are not Schema Components themselves

/**
 * Simple type content, which can be a restriction, list or union
 */
abstract class SimpleTypeContent(override val wrappedElem: indexed.Elem) extends SchemaObject(wrappedElem)

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
    case e if e.resolvedName == EName(ns, "schema") => new Schema(wrappedElem)
    case e if Set(
      EName(ns, "element"),
      EName(ns, "simpleType"),
      EName(ns, "complexType")).contains(e.resolvedName) => SchemaComponent(wrappedElem)
    case _ => new SchemaObject(wrappedElem) {}
  }
}

object Particle {

  val Unbounded = -1
}
