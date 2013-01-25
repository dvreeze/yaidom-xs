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

    val globalElemDecls = schema.globalElementDeclarations
    val elemDecls = schema.elementDeclarations

    val globalElemDecls2 = schema \\! EName(ns, "element")
    val elemDecls2 = schema \\ EName(ns, "element")

    val globalElemDecls3 = schema.wrappedElem \\! EName(ns, "element")
    val elemDecls3 = schema.wrappedElem \\ EName(ns, "element")

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

    val globalElemDecls = schema.globalElementDeclarations
    val elemDecls = schema.elementDeclarations

    assert(globalElemDecls.size >= 40)
    assert(globalElemDecls.size <= 50)
    assert(elemDecls.size > globalElemDecls.size)
  }

  @Test def testCreateValidLargeSchema() {
    val docParser = parse.DocumentParserUsingSax.newInstance
    val doc = docParser.parse(classOf[CreateSchemaTest].getResourceAsStream("ifrs-gp-2006-08-15.xsd"))

    val schemaDoc = new SchemaDocument(indexed.Document(doc))
    val schema = schemaDoc.schema

    val globalElemDecls = schema.globalElementDeclarations
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
  }

  trait MyEntityResolver extends EntityResolver {
    override def resolveEntity(publicId: String, systemId: String): InputSource = {
      new InputSource(new java.io.StringReader(""))
    }
  }
}
