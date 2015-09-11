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

package eu.cdevreeze.yaidomxs.model.bridged

import java.net.URI

import scala.Vector
import scala.collection.immutable

import eu.cdevreeze.yaidom.bridge.IndexedBridgeElem
import eu.cdevreeze.yaidom.queryapi.DocumentApi
import eu.cdevreeze.yaidom.queryapi.Nodes

/**
 * Immutable XML Schema Document, containing an xs:schema root element.
 *
 * @author Chris de Vreeze
 */
final class XsdDocument(
  val children: immutable.IndexedSeq[Nodes.CanBeDocumentChild]) extends DocumentApi[XsdElem] {

  require(children.collect({ case e: Nodes.Elem => e }).size == 1,
    s"Expected precisely one root element")
  require(children.collectFirst({ case e: Nodes.Elem => e }).get.isInstanceOf[XsdElem],
    s"Expected XsdElem root element")
  require(children.collectFirst({ case e: Nodes.Elem => e }).get.isInstanceOf[SchemaRootElem],
    s"Expected SchemaRootElem root element")

  final val schemaRootElem: SchemaRootElem =
    children.collectFirst({ case e: Nodes.Elem => e }).get.asInstanceOf[SchemaRootElem]

  final def documentElement: XsdElem = schemaRootElem

  final def bridgeElem: IndexedBridgeElem = schemaRootElem.bridgeElem

  final def uriOption: Option[URI] = Some(bridgeElem.baseUri)

  final def uri: URI = bridgeElem.baseUri

  final override def toString: String = bridgeElem.toString
}

object XsdDocument {

  def apply(children: immutable.IndexedSeq[Nodes.CanBeDocumentChild]): XsdDocument = new XsdDocument(children)

  def apply(docElem: SchemaRootElem): XsdDocument = new XsdDocument(Vector(docElem))
}
