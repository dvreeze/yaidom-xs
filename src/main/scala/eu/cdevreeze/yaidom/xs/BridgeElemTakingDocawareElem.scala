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

package eu.cdevreeze.yaidom.xs

import java.net.URI

import scala.collection.immutable

import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.core.Path
import eu.cdevreeze.yaidom.core.QName
import eu.cdevreeze.yaidom.core.Scope
import eu.cdevreeze.yaidom.docaware

/**
 * Overridable bridge element taking an `docaware.Elem`. This is a value class instance, to prevent object creation.
 *
 * @author Chris de Vreeze
 */
class BridgeElemTakingDocawareElem(val backingElem: eu.cdevreeze.yaidom.docaware.Elem) extends AnyVal with BridgeElem {

  final type BackingElem = eu.cdevreeze.yaidom.docaware.Elem

  final type SelfType = BridgeElemTakingDocawareElem

  final type UnwrappedBackingElem = eu.cdevreeze.yaidom.simple.Elem

  final def findAllChildElems: immutable.IndexedSeq[SelfType] =
    backingElem.findAllChildElems.map(e => new BridgeElemTakingDocawareElem(e))

  final def resolvedName: EName = backingElem.resolvedName

  final def resolvedAttributes: immutable.Iterable[(EName, String)] = backingElem.resolvedAttributes

  final def qname: QName = backingElem.qname

  final def attributes: immutable.Iterable[(QName, String)] = backingElem.attributes

  final def scope: Scope = backingElem.scope

  final def text: String = backingElem.text

  final def findChildElemByPathEntry(entry: Path.Entry): Option[SelfType] =
    backingElem.findChildElemByPathEntry(entry).map(e => new BridgeElemTakingDocawareElem(e))

  final def toElem: eu.cdevreeze.yaidom.simple.Elem = backingElem.elem

  final def rootElem: UnwrappedBackingElem = backingElem.rootElem

  final def path: Path = backingElem.path

  final def unwrappedBackingElem: UnwrappedBackingElem = backingElem.elem

  final def baseUri: URI = getBaseUri(path)

  private def getBaseUri(path: Path): URI = {
    // Once docaware.Elem has method baseUri, this code can disappear
    val xmlBaseEName = EName("http://www.w3.org/XML/1998/namespace", "base")

    if (path.isRoot) {
      val explicitBaseUriOption = backingElem.rootElem.attributeOption(xmlBaseEName).map(s => new URI(s))
      explicitBaseUriOption.map(u => backingElem.docUri.resolve(u)).getOrElse(backingElem.docUri)
    } else {
      val parentPath = path.parentPath

      // Recursive call
      val parentBaseUri = getBaseUri(parentPath)

      val e = backingElem.rootElem.getElemOrSelfByPath(path)
      val explicitBaseUriOption = e.attributeOption(xmlBaseEName).map(s => new URI(s))
      explicitBaseUriOption.map(u => parentBaseUri.resolve(u)).getOrElse(parentBaseUri)
    }
  }
}

object BridgeElemTakingDocawareElem {

  def wrap(elem: docaware.Elem): BridgeElemTakingDocawareElem = new BridgeElemTakingDocawareElem(elem)
}
