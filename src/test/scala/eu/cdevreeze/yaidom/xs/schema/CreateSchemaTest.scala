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

import java.{ util => jutil, io => jio }
import javax.xml.parsers.SAXParserFactory
import scala.collection.immutable
import org.xml.sax.{ EntityResolver, InputSource }
import org.junit.{ Test, Before, Ignore }
import org.junit.runner.RunWith
import org.scalatest.{ Suite, BeforeAndAfterAll }
import org.scalatest.junit.JUnitRunner

/**
 * XML Schema creation test case.
 *
 * @author Chris de Vreeze
 */
@RunWith(classOf[JUnitRunner])
class CreateSchemaTest extends Suite {

  @Test def testCreateValidSchema() {
    val docParser = parse.DocumentParserUsingSax.newInstance
    val doc = docParser.parse(classOf[CreateSchemaTest].getResourceAsStream("shiporder.xsd"))

    val docUri = classOf[CreateSchemaTest].getResource("shiporder.xsd").toURI

    val schemaDoc = new SchemaDocument(indexed.Document(doc).withUriOption(Some(docUri)))
    val schema = schemaDoc.schema

    val globalElemDecls = schema.findAllGlobalElementDeclarations
    val elemDecls = schema.findAllElems collect { case e: ElementDeclarationOrReference => e }

    val globalElemDecls2 = schema \\! EName(XsNamespace, "element")
    val elemDecls2 = schema \\ EName(XsNamespace, "element")

    val globalElemDecls3 = schema \\! EName(XsNamespace, "element")
    val elemDecls3 = schema \\ EName(XsNamespace, "element")

    val globalAttrDecls = schema.findAllGlobalAttributeDeclarations
    val attrDecls = schema.findAllElems collect { case e: AttributeDeclarationOrReference => e }

    val tns = "http://shiporder"

    expectResult(Seq(EName(tns, "shiporder"))) {
      globalElemDecls map { elemDecl => elemDecl.ename }
    }
    expectResult(Seq(EName(tns, "shiporder"))) {
      globalElemDecls2 collect { case elemDecl: ElementDeclaration => elemDecl } map { elemDecl => elemDecl.ename }
    }
    expectResult(Seq(EName(tns, "shiporder"))) {
      globalElemDecls3 map { e =>
        val tnsOption = e.indexedElem.rootElem \@ EName("targetNamespace")
        val name = (e \@ EName("name")).get
        EName(tnsOption, name)
      }
    }

    val expectedElemNames = Seq(
      EName(tns, "shiporder"),
      EName(tns, "orderperson"),
      EName(tns, "shipto"),
      EName(tns, "name"),
      EName(tns, "address"),
      EName(tns, "city"),
      EName(tns, "country"),
      EName(tns, "item"),
      EName(tns, "title"),
      EName(tns, "note"),
      EName(tns, "quantity"),
      EName(tns, "price"))

    expectResult(expectedElemNames) {
      elemDecls collect {
        case elemDecl: ElementDeclaration => elemDecl.ename
      }
    }
    expectResult(expectedElemNames) {
      elemDecls2 collect { case elemDecl: ElementDeclaration => elemDecl.ename }
    }
    expectResult(expectedElemNames) {
      elemDecls3 map { e =>
        val tnsOption = e.indexedElem.rootElem \@ EName("targetNamespace")
        val name = (e \@ EName("name")).get
        EName(tnsOption, name)
      }
    }

    expectResult(Seq()) {
      globalAttrDecls flatMap { attrDecl => attrDecl.attributeOption(EName("name")) }
    }
    expectResult(Seq("orderid")) {
      attrDecls flatMap { attrDecl => attrDecl.attributeOption(EName("name")) }
    }

    expectResult(Set(docUri)) {
      schema.findAllElemsOrSelf.map(_.docUri).toSet
    }
  }

  @Test def testCreateValidSchemaOfXmlSchema() {
    val spf = SAXParserFactory.newInstance
    spf.setFeature("http://xml.org/sax/features/namespaces", true)
    spf.setFeature("http://xml.org/sax/features/namespace-prefixes", true)

    val docParser = parse.DocumentParserUsingSax.newInstance(
      spf,
      () => new parse.DefaultElemProducingSaxHandler with MyEntityResolver)

    val doc = docParser.parse(classOf[CreateSchemaTest].getResourceAsStream("XMLSchema.xsd"))

    val docUri = classOf[CreateSchemaTest].getResource("XMLSchema.xsd").toURI

    val schemaDoc = new SchemaDocument(indexed.Document(doc).withUriOption(Some(docUri)))
    val schema = schemaDoc.schema

    val globalElemDecls = schema.findAllGlobalElementDeclarations
    val elemDecls = schema.findAllElems collect { case e: ElementDeclarationOrReference => e }

    assert(globalElemDecls.size >= 40)
    assert(globalElemDecls.size <= 50)
    assert(elemDecls.size > globalElemDecls.size)

    val occursAttrGroupOption =
      schema.findAllElemsOrSelf collect { case e: AttributeGroupDefinition => e } find { e => (e \@ EName("name")) == Some("occurs") }

    assert(occursAttrGroupOption.isDefined)

    val minOccursAttrOption =
      occursAttrGroupOption.get findElem { e =>
        e match {
          case e: AttributeDeclarationOrReference if (e \@ EName("name")) == Some("minOccurs") => true
          case _ => false
        }
      }

    assert(minOccursAttrOption.isDefined)

    val openAttrsComplexTypeOption =
      schema findElem { e =>
        e match {
          case e: ComplexTypeDefinition if (e \@ EName("name")) == Some("openAttrs") => true
          case _ => false
        }
      }

    assert(openAttrsComplexTypeOption.isDefined)
    expectResult(2) {
      openAttrsComplexTypeOption.get.findAllChildElems.size
    }

    val secondChildElem = openAttrsComplexTypeOption.get.findAllChildElems(1)

    expectResult("complexContent") {
      secondChildElem.localName
    }

    import NodeBuilder._

    val expectedElemBuilder =
      elem(
        qname = QName("xs:complexContent"),
        namespaces = Declarations.from("xs" -> XsNamespace),
        children =
          Vector(
            elem(
              qname = QName("xs:restriction"),
              attributes = Vector(QName("base") -> "xs:anyType"),
              children = Vector(
                elem(
                  qname = QName("xs:anyAttribute"),
                  attributes = Vector(QName("namespace") -> "##other", QName("processContents") -> "lax"))))))
    val expectedElem = expectedElemBuilder.build(openAttrsComplexTypeOption.get.elem.scope)

    expectResult(resolved.Elem(expectedElem)) {
      resolved.Elem(secondChildElem.elem).removeAllInterElementWhitespace
    }

    expectResult(1) {
      schema.findAllImports.size
    }
    expectResult(0) {
      schema.findAllIncludes.size
    }
    expectResult(0) {
      schema.findAllRedefines.size
    }

    expectResult(Set(docUri)) {
      schema.findAllElemsOrSelf.map(_.docUri).toSet
    }
  }

  @Test def testCreateValidLargeSchema() {
    val spf = SAXParserFactory.newInstance
    spf.setFeature("http://xml.org/sax/features/namespaces", true)
    spf.setFeature("http://xml.org/sax/features/namespace-prefixes", true)

    val docParser = parse.DocumentParserUsingSax.newInstance(
      spf,
      () => new parse.DefaultElemProducingSaxHandler with MyEntityResolver)

    val doc = docParser.parse(classOf[CreateSchemaTest].getResourceAsStream("ifrs-gp-2006-08-15.xsd"))

    val docUri = classOf[CreateSchemaTest].getResource("ifrs-gp-2006-08-15.xsd").toURI

    val schemaDoc = new SchemaDocument(indexed.Document(doc).withUriOption(Some(docUri)))
    val schema = schemaDoc.schema

    val globalElemDecls = schema.findAllGlobalElementDeclarations
    val elemDecls = schema.findAllElems collect { case e: ElementDeclarationOrReference => e }

    assert(globalElemDecls.size >= 4000)
    assert(globalElemDecls.size <= 5000)
    assert(elemDecls.size > globalElemDecls.size)

    expectResult(22) {
      val filteredElemDecls = globalElemDecls filter { elemDecl =>
        elemDecl.ename.localPart.startsWith("Accumulated")
      }
      filteredElemDecls.size
    }

    val nsXbrli = "http://www.xbrl.org/2003/instance"

    expectResult(Set(EName(nsXbrli, "item"), EName(nsXbrli, "tuple"))) {
      val result = globalElemDecls flatMap { elemDecl => elemDecl.substitutionGroupOption }
      result.toSet
    }
    expectResult(Set("instant", "duration")) {
      val result = globalElemDecls flatMap { elemDecl => elemDecl.attributeOption(EName(nsXbrli, "periodType")) }
      result.toSet
    }

    val topmostElemDecls = schema.findTopmostElems(e => e.resolvedName == XsElementEName) collect { case e: ElementDeclarationOrReference => e }

    expectResult(globalElemDecls) {
      topmostElemDecls
    }
    expectResult(elemDecls) {
      topmostElemDecls flatMap { e => e +: (e.filterElems(_.resolvedName == XsElementEName) collect { case e: ElementDeclarationOrReference => e }) }
    }

    expectResult(4) {
      schema.findAllImports.size
    }
    expectResult(0) {
      schema.findAllIncludes.size
    }
    expectResult(0) {
      schema.findAllRedefines.size
    }
  }

  @Test def testTargetNamespace() {
    val docParser = parse.DocumentParserUsingSax.newInstance
    val doc = docParser.parse(classOf[CreateSchemaTest].getResourceAsStream("shiporder.xsd"))

    val docUri = classOf[CreateSchemaTest].getResource("shiporder.xsd").toURI

    val schemaDoc = new SchemaDocument(indexed.Document(doc).withUriOption(Some(docUri)))
    val schema = schemaDoc.schema

    val expectedTns = "http://shiporder"

    expectResult(Some(expectedTns)) {
      schema.targetNamespaceOption
    }

    val shipOrderElemDeclOption = schema.findAllGlobalElementDeclarations find { e => e.nameAttribute == "shiporder" }

    assert(shipOrderElemDeclOption.isDefined)
    assert(shipOrderElemDeclOption.get.isInstanceOf[GlobalElementDeclaration])

    expectResult(Some(expectedTns)) {
      shipOrderElemDeclOption.get.indexedElem.rootElem \@ TargetNamespaceEName
    }
    expectResult(EName(expectedTns, "shiporder")) {
      shipOrderElemDeclOption.get.ename
    }
    expectResult(None) {
      shipOrderElemDeclOption.get.indexedElem.elemPath findAncestorPath {
        e => e.elementNameOption == Some(XsComplexTypeEName)
      }
    }

    val nameElemDeclOption = schema.filterElems(_.resolvedName == XsElementEName) collect { case e: ElementDeclaration => e } find { e =>
      e.nameAttribute == "name"
    }

    assert(nameElemDeclOption.isDefined)
    assert(!nameElemDeclOption.get.isInstanceOf[GlobalElementDeclaration])

    expectResult(Some(expectedTns)) {
      nameElemDeclOption.get.indexedElem.rootElem \@ TargetNamespaceEName
    }
    expectResult(EName(expectedTns, "name")) {
      nameElemDeclOption.get.ename
    }
    expectResult(Some(5)) {
      nameElemDeclOption.get.indexedElem.elemPath findAncestorPath {
        e => e.elementNameOption == Some(XsComplexTypeEName)
      } map { _.entries.size }
    }

    val orderidAttrDeclOption =
      (schema.findTopmostElems(_.resolvedName == XsAttributeEName) collect { case e: AttributeDeclaration => e } find { e =>
        e.nameAttribute == "orderid"
      }).headOption

    assert(orderidAttrDeclOption.isDefined)
    assert(!orderidAttrDeclOption.get.isInstanceOf[GlobalAttributeDeclaration])

    expectResult(Some(2)) {
      orderidAttrDeclOption.get.indexedElem.elemPath findAncestorPath {
        e => e.elementNameOption == Some(XsComplexTypeEName)
      } map { _.entries.size }
    }
  }

  trait MyEntityResolver extends EntityResolver {
    override def resolveEntity(publicId: String, systemId: String): InputSource = {
      new InputSource(new java.io.StringReader(""))
    }
  }
}
