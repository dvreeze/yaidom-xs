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
import scala.reflect.classTag
import org.xml.sax.{ EntityResolver, InputSource }
import org.junit.{ Test, Before, Ignore }
import org.junit.runner.RunWith
import org.scalatest.{ Suite, BeforeAndAfterAll }
import org.scalatest.junit.JUnitRunner
import eu.cdevreeze.yaidom.subtypeaware.SubtypeAwareParentElemApi.anyElem

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

    val schemaDoc = new SchemaDocument(docaware.Document(docUri, doc))
    val schema = schemaDoc.schema

    val globalElemDecls = schema.findAllGlobalElementDeclarations
    val elemDecls = schema.findAllElemsOfType(classTag[ElementDeclarationOrReference])

    val globalElemDecls2 = schema \\! EName(XsNamespace, "element")
    val elemDecls2 = schema \\ EName(XsNamespace, "element")

    val globalElemDecls3 = schema \\! EName(XsNamespace, "element")
    val elemDecls3 = schema \\ EName(XsNamespace, "element")

    val globalAttrDecls = schema.findAllGlobalAttributeDeclarations
    val attrDecls = schema.findAllElemsOfType(classTag[AttributeDeclarationOrReference])

    val tns = "http://shiporder"

    assertResult(Seq(EName(tns, "shiporder"))) {
      globalElemDecls map { elemDecl => elemDecl.targetEName }
    }
    assertResult(Seq(EName(tns, "shiporder"))) {
      globalElemDecls2 collect { case elemDecl: ElementDeclaration => elemDecl } map { elemDecl => elemDecl.targetEName }
    }
    assertResult(Seq(EName(tns, "shiporder"))) {
      globalElemDecls3 map { e =>
        val tnsOption = e.docawareElem.rootElem \@ EName("targetNamespace")
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

    assertResult(expectedElemNames) {
      elemDecls collect {
        case elemDecl: ElementDeclaration => elemDecl.targetEName
      }
    }
    assertResult(expectedElemNames) {
      elemDecls2 collect { case elemDecl: ElementDeclaration => elemDecl.targetEName }
    }
    assertResult(expectedElemNames) {
      elemDecls3 map { e =>
        val tnsOption = e.docawareElem.rootElem \@ EName("targetNamespace")
        val name = (e \@ EName("name")).get
        EName(tnsOption, name)
      }
    }

    assertResult(Seq()) {
      globalAttrDecls flatMap { attrDecl => attrDecl.attributeOption(EName("name")) }
    }
    assertResult(Seq("orderid")) {
      attrDecls flatMap { attrDecl => attrDecl.attributeOption(EName("name")) }
    }

    assertResult(Set(docUri)) {
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

    val schemaDoc = new SchemaDocument(docaware.Document(docUri, doc))
    val schema = schemaDoc.schema

    val globalElemDecls = schema.findAllGlobalElementDeclarations
    val elemDecls = schema.findAllElemsOfType(classTag[ElementDeclarationOrReference])

    assert(globalElemDecls.size >= 40)
    assert(globalElemDecls.size <= 50)
    assert(elemDecls.size > globalElemDecls.size)

    val occursAttrGroupOption =
      schema.findAllElemsOrSelfOfType(classTag[AttributeGroupDefinition]) find { e => (e \@ EName("name")) == Some("occurs") }

    assert(occursAttrGroupOption.isDefined)

    val minOccursAttrOption =
      occursAttrGroupOption.get findElem { e: XsdElem =>
        e match {
          case e: AttributeDeclarationOrReference if (e \@ EName("name")) == Some("minOccurs") => true
          case _ => false
        }
      }

    assert(minOccursAttrOption.isDefined)

    val openAttrsComplexTypeOption =
      schema findElem { e: XsdElem =>
        e match {
          case e: ComplexTypeDefinition if (e \@ EName("name")) == Some("openAttrs") => true
          case _ => false
        }
      }

    assert(openAttrsComplexTypeOption.isDefined)
    assertResult(2) {
      openAttrsComplexTypeOption.get.findAllChildElems.size
    }

    val secondChildElem = openAttrsComplexTypeOption.get.findAllChildElems(1)

    assertResult("complexContent") {
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

    assertResult(resolved.Elem(expectedElem)) {
      resolved.Elem(secondChildElem.elem).removeAllInterElementWhitespace
    }

    assertResult(1) {
      schema.findAllImports.size
    }
    assertResult(0) {
      schema.findAllIncludes.size
    }
    assertResult(0) {
      schema.findAllRedefines.size
    }

    assertResult(Set(docUri)) {
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

    val schemaDoc = new SchemaDocument(docaware.Document(docUri, doc))
    val schema = schemaDoc.schema

    val globalElemDecls = schema.findAllGlobalElementDeclarations
    val elemDecls = schema.findAllElemsOfType(classTag[ElementDeclarationOrReference])

    assert(globalElemDecls.size >= 4000)
    assert(globalElemDecls.size <= 5000)
    assert(elemDecls.size > globalElemDecls.size)

    assertResult(22) {
      val filteredElemDecls = globalElemDecls filter { elemDecl =>
        elemDecl.targetEName.localPart.startsWith("Accumulated")
      }
      filteredElemDecls.size
    }

    val nsXbrli = "http://www.xbrl.org/2003/instance"

    assertResult(Set(EName(nsXbrli, "item"), EName(nsXbrli, "tuple"))) {
      val result = globalElemDecls flatMap { elemDecl => elemDecl.substitutionGroupOption }
      result.toSet
    }
    assertResult(Set("instant", "duration")) {
      val result = globalElemDecls flatMap { elemDecl => elemDecl.attributeOption(EName(nsXbrli, "periodType")) }
      result.toSet
    }

    val topmostElemDecls = schema.findTopmostElemsOfType(classTag[ElementDeclarationOrReference])(_ => true)

    assertResult(globalElemDecls) {
      topmostElemDecls
    }
    assertResult(elemDecls) {
      topmostElemDecls flatMap { e => e +: (e.findAllElemsOfType(classTag[ElementDeclarationOrReference])) }
    }

    assertResult(4) {
      schema.findAllImports.size
    }
    assertResult(0) {
      schema.findAllIncludes.size
    }
    assertResult(0) {
      schema.findAllRedefines.size
    }
  }

  @Test def testTargetNamespace() {
    val docParser = parse.DocumentParserUsingSax.newInstance
    val doc = docParser.parse(classOf[CreateSchemaTest].getResourceAsStream("shiporder.xsd"))

    val docUri = classOf[CreateSchemaTest].getResource("shiporder.xsd").toURI

    val schemaDoc = new SchemaDocument(docaware.Document(docUri, doc))
    val schema = schemaDoc.schema

    val expectedTns = "http://shiporder"

    assertResult(Some(expectedTns)) {
      schema.targetNamespaceOption
    }

    val shipOrderElemDeclOption = schema.findAllGlobalElementDeclarations find { e => e.nameAttribute == "shiporder" }

    assert(shipOrderElemDeclOption.isDefined)
    assert(shipOrderElemDeclOption.get.isInstanceOf[GlobalElementDeclaration])

    assertResult(Some(expectedTns)) {
      shipOrderElemDeclOption.get.docawareElem.rootElem \@ TargetNamespaceEName
    }
    assertResult(EName(expectedTns, "shiporder")) {
      shipOrderElemDeclOption.get.targetEName
    }
    assertResult(None) {
      shipOrderElemDeclOption.get.docawareElem.path findAncestorPath {
        e => e.elementNameOption == Some(XsComplexTypeEName)
      }
    }

    val nameElemDeclOption = schema.findAllElemsOfType(classTag[ElementDeclaration]) find { e =>
      e.nameAttribute == "name"
    }

    assert(nameElemDeclOption.isDefined)
    assert(!nameElemDeclOption.get.isInstanceOf[GlobalElementDeclaration])

    assertResult(Some(expectedTns)) {
      nameElemDeclOption.get.docawareElem.rootElem \@ TargetNamespaceEName
    }
    assertResult(EName(expectedTns, "name")) {
      nameElemDeclOption.get.targetEName
    }
    assertResult(Some(5)) {
      nameElemDeclOption.get.docawareElem.path findAncestorPath {
        e => e.elementNameOption == Some(XsComplexTypeEName)
      } map { _.entries.size }
    }

    val orderidAttrDeclOption =
      (schema.findTopmostElemsOfType(classTag[AttributeDeclaration])(_ => true) find { e =>
        e.nameAttribute == "orderid"
      }).headOption

    assert(orderidAttrDeclOption.isDefined)
    assert(!orderidAttrDeclOption.get.isInstanceOf[GlobalAttributeDeclaration])

    assertResult(Some(2)) {
      orderidAttrDeclOption.get.docawareElem.path findAncestorPath {
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
