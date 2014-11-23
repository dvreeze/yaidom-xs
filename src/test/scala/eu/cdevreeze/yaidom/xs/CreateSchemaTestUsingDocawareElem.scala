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

package eu.cdevreeze.yaidom.xs

import java.io.File
import org.junit.runner.RunWith
import org.xml.sax.EntityResolver
import org.xml.sax.InputSource
import org.scalatest.junit.JUnitRunner
import eu.cdevreeze.yaidom.docaware
import eu.cdevreeze.yaidom.parse.DefaultElemProducingSaxHandler
import eu.cdevreeze.yaidom.parse.DocumentParserUsingSax
import eu.cdevreeze.yaidom.simple.Document
import javax.xml.parsers.SAXParserFactory
import eu.cdevreeze.yaidom.bridge.DefaultDocawareBridgeElem

/**
 * XML Schema creation test case using docaware elements.
 *
 * @author Chris de Vreeze
 */
@RunWith(classOf[JUnitRunner])
class CreateSchemaTestUsingDocawareElem extends AbstractCreateSchemaTest {

  protected def getSchemaDocument(fileName: String): SchemaDocument = {
    val spf = SAXParserFactory.newInstance
    spf.setFeature("http://xml.org/sax/features/namespaces", true)
    spf.setFeature("http://xml.org/sax/features/namespace-prefixes", true)

    val docParser = DocumentParserUsingSax.newInstance(
      spf,
      () => new DefaultElemProducingSaxHandler with MyEntityResolver)

    val uri = classOf[CreateSchemaTestUsingDocawareElem].getResource(fileName).toURI
    val f = new File(uri)

    val doc: Document = docParser.parse(f)

    val schemaDoc: SchemaDocument =
      new SchemaDocument(
        DefaultDocawareBridgeElem.wrap(docaware.Elem(uri, doc.documentElement)))

    schemaDoc
  }

  trait MyEntityResolver extends EntityResolver {
    override def resolveEntity(publicId: String, systemId: String): InputSource = {
      new InputSource(new java.io.StringReader(""))
    }
  }
}
