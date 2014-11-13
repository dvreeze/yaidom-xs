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
import org.scalatest.junit.JUnitRunner

import net.sf.saxon.om.DocumentInfo
import net.sf.saxon.s9api.Processor

/**
 * XML Schema creation test case using Saxon.
 *
 * @author Chris de Vreeze
 */
@RunWith(classOf[JUnitRunner])
class CreateSchemaTestUsingSaxonElem extends AbstractCreateSchemaTest {

  private val processor = new Processor(false)

  protected def getSchemaDocument(fileName: String): SchemaDocument = {
    val docBuilder = processor.newDocumentBuilder()

    val uri = classOf[CreateSchemaTestUsingSaxonElem].getResource(fileName).toURI
    val f = new File(uri)

    val node = docBuilder.build(f)
    node.getUnderlyingNode.setSystemId(uri.toString)

    val schemaDoc: SchemaDocument =
      new SchemaDocument(
        BridgeElemTakingSaxonElem.wrap(new saxon.DomDocument(node.getUnderlyingNode.asInstanceOf[DocumentInfo]).documentElement))

    schemaDoc
  }
}
