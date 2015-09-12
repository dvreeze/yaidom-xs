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

package eu.cdevreeze.yaidomxs.nodeinfo

import java.net.URI

import scala.Vector
import scala.collection.immutable
import scala.collection.mutable

import eu.cdevreeze.yaidom.queryapi.DocumentApi
import net.sf.saxon.`type`.Type
import net.sf.saxon.om.AxisInfo
import net.sf.saxon.om.DocumentInfo
import net.sf.saxon.om.NodeInfo

/**
 * Saxon (aidom) wrapper document.
 *
 * @author Chris de Vreeze
 */
final class SaxonDocument(val wrappedNode: DocumentInfo) extends DocumentApi[SaxonElem] {
  require(wrappedNode ne null)
  require(wrappedNode.getNodeKind == Type.DOCUMENT)

  final def documentElement: SaxonElem =
    (children collectFirst { case e: SaxonElem => e }).getOrElse(sys.error(s"Missing document element"))

  final def children: immutable.IndexedSeq[SaxonNode] = {
    if (!wrappedNode.hasChildNodes) Vector()
    else {
      val it = wrappedNode.iterateAxis(AxisInfo.CHILD)

      val nodes = Stream.continually(it.next).takeWhile(_ ne null).toVector

      nodes.flatMap(nodeInfo => SaxonNode.wrapNodeOption(nodeInfo))
    }
  }

  final def uriOption: Option[URI] = {
    // There must be a better way
    None
  }
}

object SaxonDocument {

  def wrapDocument(doc: DocumentInfo): SaxonDocument = new SaxonDocument(doc)
}
