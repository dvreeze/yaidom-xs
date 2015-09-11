/*
 * Copyright 2014 Chris de Vreeze
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

package eu.cdevreeze.yaidomxs.model.bridged.saxon

import java.net.URI

import scala.collection.immutable

import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.core.Path
import eu.cdevreeze.yaidom.core.QName
import eu.cdevreeze.yaidom.core.Scope
import eu.cdevreeze.yaidom.bridge.IndexedBridgeElem

/**
 * Overridable bridge element taking a `SaxonElem`.
 *
 * @author Chris de Vreeze
 */
class SaxonBridgeElem(val backingElem: SaxonElem) extends AnyVal with IndexedBridgeElem {

  final type BackingElem = SaxonElem

  final type SelfType = SaxonBridgeElem

  final type UnwrappedBackingElem = SaxonElem

  final def findAllChildElems: immutable.IndexedSeq[SelfType] =
    backingElem.findAllChildElems.map(e => new SaxonBridgeElem(e))

  final def resolvedName: EName = backingElem.resolvedName

  final def resolvedAttributes: immutable.Iterable[(EName, String)] = backingElem.resolvedAttributes

  final def qname: QName = backingElem.qname

  final def attributes: immutable.Iterable[(QName, String)] = backingElem.attributes

  final def scope: Scope = backingElem.scope

  final def text: String = backingElem.text

  final def findChildElemByPathEntry(entry: Path.Entry): Option[SelfType] =
    backingElem.findChildElemByPathEntry(entry).map(e => new SaxonBridgeElem(e))

  final def toElem: eu.cdevreeze.yaidom.simple.Elem = backingElem.toElem

  final def rootElem: UnwrappedBackingElem = backingElem.ancestorsOrSelf.last

  final def path: Path = backingElem.path

  final def unwrappedBackingElem: UnwrappedBackingElem = backingElem

  final def docUri: URI = {
    // Assuming that the document URI is the root element base URI, which is not necessarily true
    Option(rootElem.wrappedNode.getBaseURI).map(s => new URI(s)).getOrElse(sys.error(s"Missing document URI"))
  }

  final def baseUri: URI =
    Option(backingElem.wrappedNode.getBaseURI).map(s => new URI(s)).getOrElse(sys.error(s"Missing base URI"))
}

object SaxonBridgeElem {

  def wrap(elem: SaxonElem): SaxonBridgeElem = new SaxonBridgeElem(elem)
}
