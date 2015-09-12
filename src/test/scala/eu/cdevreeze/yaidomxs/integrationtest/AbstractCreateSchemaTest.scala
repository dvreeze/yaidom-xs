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

package eu.cdevreeze.yaidomxs.integrationtest

import scala.Vector
import scala.reflect.classTag

import org.junit.Test
import org.scalatest.Suite

import eu.cdevreeze.yaidom.core.Declarations
import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.core.QName
import eu.cdevreeze.yaidom.queryapi.ElemApi.anyElem
import eu.cdevreeze.yaidom.queryapi.HasENameApi.ToHasElemApi
import eu.cdevreeze.yaidom.resolved
import eu.cdevreeze.yaidom.simple.NodeBuilder
import eu.cdevreeze.yaidomxs.model
import eu.cdevreeze.yaidomxs.model.bridged.XsdDocument
import eu.cdevreeze.yaidomxs.model.bridged.ElementDeclaration
import eu.cdevreeze.yaidomxs.model.bridged.ElementDeclarationOrReference
import eu.cdevreeze.yaidomxs.model.bridged.AttributeDeclaration
import eu.cdevreeze.yaidomxs.model.bridged.AttributeDeclarationOrReference
import eu.cdevreeze.yaidomxs.model.bridged.AttributeGroupDefinition
import eu.cdevreeze.yaidomxs.model.bridged.ComplexTypeDefinition
import eu.cdevreeze.yaidomxs.model.bridged.GlobalElementDeclaration
import eu.cdevreeze.yaidomxs.model.bridged.GlobalAttributeDeclaration

/**
 * XML Schema creation test case.
 *
 * @author Chris de Vreeze
 */
abstract class AbstractCreateSchemaTest extends Suite {

  protected def getSchemaDocument(fileName: String): XsdDocument

  @Test def testCreateValidSchema(): Unit = {
    val schemaDoc = getSchemaDocument("shiporder.xsd")
    val schema = schemaDoc.schemaRootElem

    val globalElemDecls = schema.findAllGlobalElementDeclarations
    val elemDecls = schema.findAllElemsOfType(classTag[ElementDeclarationOrReference])

    val globalElemDecls2 = schema \\! EName(model.XsNamespace, "element")
    val elemDecls2 = schema \\ EName(model.XsNamespace, "element")

    val globalElemDecls3 = schema \\! EName(model.XsNamespace, "element")
    val elemDecls3 = schema \\ EName(model.XsNamespace, "element")

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
        val tnsOption = e.bridgeElem.rootElem \@ EName("targetNamespace")
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
        val tnsOption = e.bridgeElem.rootElem \@ EName("targetNamespace")
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

    assertResult(Set(schemaDoc.uri)) {
      schema.findAllElemsOrSelf.map(_.baseUri).toSet
    }
  }

  @Test def testCreateValidSchemaOfXmlSchema(): Unit = {
    val schemaDoc = getSchemaDocument("XMLSchema.xsd")
    val schema = schemaDoc.schemaRootElem

    val globalElemDecls = schema.findAllGlobalElementDeclarations
    val elemDecls = schema.findAllElemsOfType(classTag[ElementDeclarationOrReference])

    assert(globalElemDecls.size >= 40)
    assert(globalElemDecls.size <= 50)
    assert(elemDecls.size > globalElemDecls.size)

    val occursAttrGroupOption =
      schema.findAllElemsOrSelfOfType(classTag[AttributeGroupDefinition]) find { e => (e \@ EName("name")) == Some("occurs") }

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
        namespaces = Declarations.from("xs" -> model.XsNamespace),
        children =
          Vector(
            elem(
              qname = QName("xs:restriction"),
              attributes = Vector(QName("base") -> "xs:anyType"),
              children = Vector(
                emptyElem(
                  qname = QName("xs:anyAttribute"),
                  attributes = Vector(QName("namespace") -> "##other", QName("processContents") -> "lax"))))))
    val expectedElem = expectedElemBuilder.build(openAttrsComplexTypeOption.get.scope)

    assertResult(resolved.Elem(expectedElem)) {
      resolved.Elem(secondChildElem.bridgeElem.toElem).removeAllInterElementWhitespace
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

    assertResult(Set(schemaDoc.uri)) {
      schema.findAllElemsOrSelf.map(_.baseUri).toSet
    }
  }

  @Test def testCreateValidLargeSchema(): Unit = {
    val schemaDoc = getSchemaDocument("ifrs-gp-2006-08-15.xsd")
    val schema = schemaDoc.schemaRootElem

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

    val topmostElemDecls = schema.findTopmostElemsOfType(classTag[ElementDeclarationOrReference])(anyElem)

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

  @Test def testTargetNamespace(): Unit = {
    val schemaDoc = getSchemaDocument("shiporder.xsd")
    val schema = schemaDoc.schemaRootElem

    val expectedTns = "http://shiporder"

    assertResult(Some(expectedTns)) {
      schema.targetNamespaceOption
    }

    val shipOrderElemDeclOption = schema.findAllGlobalElementDeclarations find { e => e.nameAttribute == "shiporder" }

    assert(shipOrderElemDeclOption.isDefined)
    assert(shipOrderElemDeclOption.get.isInstanceOf[GlobalElementDeclaration])

    assertResult(Some(expectedTns)) {
      shipOrderElemDeclOption.get.bridgeElem.rootElem \@ model.TargetNamespaceEName
    }
    assertResult(EName(expectedTns, "shiporder")) {
      shipOrderElemDeclOption.get.targetEName
    }
    assertResult(None) {
      shipOrderElemDeclOption.get.bridgeElem.path findAncestorPath {
        e => e.elementNameOption == Some(model.XsComplexTypeEName)
      }
    }

    val nameElemDeclOption = schema.findAllElemsOfType(classTag[ElementDeclaration]) find { e =>
      e.nameAttribute == "name"
    }

    assert(nameElemDeclOption.isDefined)
    assert(!nameElemDeclOption.get.isInstanceOf[GlobalElementDeclaration])

    assertResult(Some(expectedTns)) {
      nameElemDeclOption.get.bridgeElem.rootElem \@ model.TargetNamespaceEName
    }
    assertResult(EName(expectedTns, "name")) {
      nameElemDeclOption.get.targetEName
    }
    assertResult(Some(5)) {
      nameElemDeclOption.get.bridgeElem.path findAncestorPath {
        e => e.elementNameOption == Some(model.XsComplexTypeEName)
      } map { _.entries.size }
    }

    val orderidAttrDeclOption =
      (schema.findTopmostElemsOfType(classTag[AttributeDeclaration])(anyElem) find { e =>
        e.nameAttribute == "orderid"
      }).headOption

    assert(orderidAttrDeclOption.isDefined)
    assert(!orderidAttrDeclOption.get.isInstanceOf[GlobalAttributeDeclaration])

    assertResult(Some(2)) {
      orderidAttrDeclOption.get.bridgeElem.path findAncestorPath {
        e => e.elementNameOption == Some(model.XsComplexTypeEName)
      } map { _.entries.size }
    }
  }
}
