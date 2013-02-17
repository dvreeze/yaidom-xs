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

    val schemaDoc = new SchemaDocument(indexed.Document(doc))
    val schema = schemaDoc.schema

    val globalElemDecls = schema.topLevelElementDeclarations
    val elemDecls = schema.elementDeclarations

    val globalElemDecls2 = schema \\! EName(ns, "element")
    val elemDecls2 = schema \\ EName(ns, "element")

    val globalElemDecls3 = schema.wrappedElem \\! EName(ns, "element")
    val elemDecls3 = schema.wrappedElem \\ EName(ns, "element")

    val globalAttrDecls = schema.topLevelAttributeDeclarations
    val attrDecls = schema.attributeDeclarations

    val tns = "http://shiporder"

    expect(Seq(EName(tns, "shiporder"))) {
      globalElemDecls flatMap { elemDecl => elemDecl.enameOption }
    }
    expect(Seq(EName(tns, "shiporder"))) {
      globalElemDecls2 collect { case elemDecl: ElementDeclaration => elemDecl } flatMap { elemDecl => elemDecl.enameOption }
    }
    expect(Seq(EName(tns, "shiporder"))) {
      globalElemDecls3 map { e =>
        val tnsOption = e.rootElem \@ "targetNamespace"
        val name = (e \@ "name").get
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

    expect(expectedElemNames) {
      elemDecls flatMap { elemDecl => elemDecl.enameOption }
    }
    expect(expectedElemNames) {
      elemDecls2 collect { case elemDecl: ElementDeclaration => elemDecl } flatMap { elemDecl => elemDecl.enameOption }
    }
    expect(expectedElemNames) {
      elemDecls3 map { e =>
        val tnsOption = e.rootElem \@ "targetNamespace"
        val name = (e \@ "name").get
        EName(tnsOption, name)
      }
    }

    expect(Seq()) {
      globalAttrDecls flatMap { attrDecl => attrDecl.attributeOption(EName("name")) }
    }
    expect(Seq("orderid")) {
      attrDecls flatMap { attrDecl => attrDecl.attributeOption(EName("name")) }
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

    val schemaDoc = new SchemaDocument(indexed.Document(doc))
    val schema = schemaDoc.schema

    val globalElemDecls = schema.topLevelElementDeclarations
    val elemDecls = schema.elementDeclarations

    assert(globalElemDecls.size >= 40)
    assert(globalElemDecls.size <= 50)
    assert(elemDecls.size > globalElemDecls.size)

    val occursAttrGroupOption =
      schema collectFromElemsOrSelf { case e: AttributeGroupDefinition => e } find { e => (e \@ "name") == Some("occurs") }

    assert(occursAttrGroupOption.isDefined)

    val minOccursAttrOption =
      occursAttrGroupOption.get findElem { e =>
        e match {
          case e: AttributeDeclaration if (e \@ "name") == Some("minOccurs") => true
          case _ => false
        }
      }

    assert(minOccursAttrOption.isDefined)

    val openAttrsComplexTypeOption =
      schema findElem { e =>
        e match {
          case e: ComplexTypeDefinition if (e \@ "name") == Some("openAttrs") => true
          case _ => false
        }
      }

    assert(openAttrsComplexTypeOption.isDefined)
    expect(2) {
      openAttrsComplexTypeOption.get.allChildElems.size
    }

    val secondChildElem = openAttrsComplexTypeOption.get.allChildElems(1)

    expect("complexContent") {
      secondChildElem.localName
    }

    import NodeBuilder._

    val expectedElemBuilder =
      elem(
        qname = QName("xs:complexContent"),
        namespaces = Declarations.from("xs" -> ns),
        children =
          Vector(
            elem(
              qname = QName("xs:restriction"),
              attributes = Vector(QName("base") -> "xs:anyType"),
              children = Vector(
                elem(
                  qname = QName("xs:anyAttribute"),
                  attributes = Vector(QName("namespace") -> "##other", QName("processContents") -> "lax"))))))
    val expectedElem = expectedElemBuilder.build(openAttrsComplexTypeOption.get.wrappedElem.elem.scope)

    expect(resolved.Elem(expectedElem)) {
      resolved.Elem(secondChildElem.wrappedElem.elem).removeAllInterElementWhitespace
    }

    expect(1) {
      schema.imports.size
    }
    expect(0) {
      schema.includes.size
    }
    expect(0) {
      schema.redefines.size
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

    val schemaDoc = new SchemaDocument(indexed.Document(doc))
    val schema = schemaDoc.schema

    val globalElemDecls = schema.topLevelElementDeclarations
    val elemDecls = schema.elementDeclarations

    assert(globalElemDecls.size >= 4000)
    assert(globalElemDecls.size <= 5000)
    assert(elemDecls.size > globalElemDecls.size)

    expect(22) {
      val filteredElemDecls = globalElemDecls filter { elemDecl =>
        elemDecl.enameOption.map(_.localPart).getOrElse("").startsWith("Accumulated")
      }
      filteredElemDecls.size
    }

    val nsXbrli = "http://www.xbrl.org/2003/instance"

    expect(Set(EName(nsXbrli, "item"), EName(nsXbrli, "tuple"))) {
      val result = globalElemDecls flatMap { elemDecl => elemDecl.substitutionGroupOption }
      result.toSet
    }
    expect(Set("instant", "duration")) {
      val result = globalElemDecls flatMap { elemDecl => elemDecl.attributeOption(EName(nsXbrli, "periodType")) }
      result.toSet
    }

    val topmostElemDecls = schema.topmostElementDeclarations

    expect(globalElemDecls) {
      topmostElemDecls
    }
    expect(elemDecls) {
      topmostElemDecls flatMap { e => e +: e.elementDeclarations }
    }

    expect(4) {
      schema.imports.size
    }
    expect(0) {
      schema.includes.size
    }
    expect(0) {
      schema.redefines.size
    }
  }

  @Test def testCreateInvalidSchema() {
    val docParser = parse.DocumentParserUsingSax.newInstance
    val doc = docParser.parse(classOf[CreateSchemaTest].getResourceAsStream("shiporder.xsd"))

    import NodeBuilder._
    val invalidChild =
      textElem(QName("xyz"), "invalid").build(doc.documentElement.scope.withoutDefaultNamespace)

    val invalidDoc = Document(doc.documentElement.plusChild(invalidChild))

    intercept[Exception] {
      new SchemaDocument(indexed.Document(invalidDoc))
    }
  }

  @Test def testCreateAnotherInvalidSchema() {
    val docParser = parse.DocumentParserUsingSax.newInstance
    val doc = docParser.parse(classOf[CreateSchemaTest].getResourceAsStream("shiporder.xsd"))

    import NodeBuilder._
    val invalidChild =
      textElem(QName("xs:complexContent"), "invalid").build(doc.documentElement.scope ++ Scope.from("xs" -> ns))

    val invalidDoc = Document(doc.documentElement.plusChild(invalidChild))

    intercept[Exception] {
      new SchemaDocument(indexed.Document(invalidDoc))
    }
  }

  @Test def testTargetNamespace() {
    val docParser = parse.DocumentParserUsingSax.newInstance
    val doc = docParser.parse(classOf[CreateSchemaTest].getResourceAsStream("shiporder.xsd"))

    val schemaDoc = new SchemaDocument(indexed.Document(doc))
    val schema = schemaDoc.schema

    val expectedTns = "http://shiporder"

    expect(Some(expectedTns)) {
      schema.targetNamespaceOption
    }

    val shipOrderElemDeclOption = schema.topLevelElementDeclarations find { e => e.nameAttributeOption == Some("shiporder") }

    assert(shipOrderElemDeclOption.isDefined)
    assert(shipOrderElemDeclOption.get.isTopLevel)

    expect(Some(expectedTns)) {
      shipOrderElemDeclOption.get.targetNamespaceOption
    }
    expect(Some(EName(expectedTns, "shiporder"))) {
      shipOrderElemDeclOption.get.enameOption
    }
    expect(None) {
      shipOrderElemDeclOption.get.scopeOption
    }

    val nameElemDeclOption = schema.elementDeclarations find { e => e.nameAttributeOption == Some("name") }

    assert(nameElemDeclOption.isDefined)
    assert(!nameElemDeclOption.get.isTopLevel)

    expect(Some(expectedTns)) {
      nameElemDeclOption.get.targetNamespaceOption
    }
    expect(Some(EName(expectedTns, "name"))) {
      nameElemDeclOption.get.enameOption
    }
    expect(Some(5)) {
      nameElemDeclOption.get.scopeOption map { complexTypeDef => complexTypeDef.elemPath.entries.size }
    }

    val orderidAttrDeclOption = schema.topmostAttributeDeclarations find { e => e.nameAttributeOption == Some("orderid") }

    assert(orderidAttrDeclOption.isDefined)
    assert(!orderidAttrDeclOption.get.isTopLevel)

    expect(None) {
      orderidAttrDeclOption.get.targetNamespaceOption
    }
    expect(Some(EName(None, "orderid"))) {
      orderidAttrDeclOption.get.enameOption
    }
    expect(Some(2)) {
      orderidAttrDeclOption.get.scopeOption map { complexTypeDef => complexTypeDef.elemPath.entries.size }
    }
  }

  trait MyEntityResolver extends EntityResolver {
    override def resolveEntity(publicId: String, systemId: String): InputSource = {
      new InputSource(new java.io.StringReader(""))
    }
  }
}
