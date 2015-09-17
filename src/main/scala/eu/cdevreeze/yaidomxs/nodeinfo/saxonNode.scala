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

import eu.cdevreeze.yaidomxs.XmlStringUtils
import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.core.ENameProvider
import eu.cdevreeze.yaidom.core.QName
import eu.cdevreeze.yaidom.core.QNameProvider
import eu.cdevreeze.yaidom.core.Path
import eu.cdevreeze.yaidom.core.Scope
import eu.cdevreeze.yaidom.queryapi.DocumentApi
import eu.cdevreeze.yaidom.queryapi.HasParent
import eu.cdevreeze.yaidom.queryapi.IsNavigable
import eu.cdevreeze.yaidom.queryapi.Nodes
import eu.cdevreeze.yaidom.queryapi.ScopedElemLike
import eu.cdevreeze.yaidom.resolved.ResolvedNodes
import eu.cdevreeze.yaidom.simple
import net.sf.saxon.`type`.Type
import net.sf.saxon.om.AxisInfo
import net.sf.saxon.om.DocumentInfo
import net.sf.saxon.om.NodeInfo

/**
 * Saxon (yaidom) wrapper node hierarchy.
 *
 * @author Chris de Vreeze
 */
abstract class SaxonNode(val wrappedNode: NodeInfo) extends ResolvedNodes.Node {

  import ENameProvider.globalENameProvider._
  import QNameProvider.globalQNameProvider._

  final override def toString: String = wrappedNode.toString

  protected def nodeInfo2EName(nodeInfo: NodeInfo): EName = {
    val ns: String = nodeInfo.getURI
    val nsOption: Option[String] = if (ns == "") None else Some(ns)
    getEName(nsOption, nodeInfo.getLocalPart)
  }

  protected def nodeInfo2QName(nodeInfo: NodeInfo): QName = {
    val pref: String = nodeInfo.getPrefix
    val prefOption: Option[String] = if (pref == "") None else Some(pref)
    getQName(prefOption, nodeInfo.getLocalPart)
  }

  def toNodeOption: Option[simple.Node] = None

  override def equals(other: Any): Boolean = other match {
    case n: SaxonNode => wrappedNode == n.wrappedNode
    case _            => false
  }

  override def hashCode: Int = wrappedNode.hashCode
}

final class SaxonElem(
  override val wrappedNode: NodeInfo) extends SaxonNode(wrappedNode) with ResolvedNodes.Elem with ScopedElemLike[SaxonElem] with HasParent[SaxonElem] { self =>

  require(wrappedNode ne null)
  require(wrappedNode.getNodeKind == Type.ELEMENT)

  final def children: immutable.IndexedSeq[SaxonNode] = {
    if (!wrappedNode.hasChildNodes) Vector()
    else {
      val it = wrappedNode.iterateAxis(AxisInfo.CHILD)

      val nodes = Stream.continually(it.next).takeWhile(_ ne null).toVector

      nodes.flatMap(nodeInfo => SaxonNode.wrapNodeOption(nodeInfo))
    }
  }

  override def findAllChildElems: immutable.IndexedSeq[SaxonElem] = children collect { case e: SaxonElem => e }

  override def resolvedName: EName = nodeInfo2EName(wrappedNode)

  override def resolvedAttributes: immutable.IndexedSeq[(EName, String)] = {
    val it = wrappedNode.iterateAxis(AxisInfo.ATTRIBUTE)

    val nodes = Stream.continually(it.next).takeWhile(_ ne null).toVector

    nodes map { nodeInfo => nodeInfo2EName(nodeInfo) -> nodeInfo.getStringValue }
  }

  override def qname: QName = nodeInfo2QName(wrappedNode)

  override def attributes: immutable.IndexedSeq[(QName, String)] = {
    val it = wrappedNode.iterateAxis(AxisInfo.ATTRIBUTE)

    val nodes = Stream.continually(it.next).takeWhile(_ ne null).toVector

    nodes map { nodeInfo => nodeInfo2QName(nodeInfo) -> nodeInfo.getStringValue }
  }

  /** Returns the text children */
  def textChildren: immutable.IndexedSeq[SaxonText] = children collect { case t: SaxonText => t }

  /** Returns the comment children */
  def commentChildren: immutable.IndexedSeq[SaxonComment] = children collect { case c: SaxonComment => c }

  /**
   * Returns the concatenation of the texts of text children, including whitespace and CData. Non-text children are ignored.
   * If there are no text children, the empty string is returned.
   */
  override def text: String = {
    val textStrings = textChildren map { t => t.text }
    textStrings.mkString
  }

  override def parentOption: Option[SaxonElem] = {
    val parentNodeOption = Option(wrappedNode.getParent)
    val parentElemOption = parentNodeOption collect { case e: NodeInfo if e.getNodeKind == Type.ELEMENT => e }
    parentElemOption map { e => SaxonNode.wrapElement(e) }
  }

  override def scope: Scope = {
    val it = wrappedNode.iterateAxis(AxisInfo.NAMESPACE)

    val nodes = Stream.continually(it.next).takeWhile(_ ne null).toVector

    val resultMap = {
      val result =
        nodes map { nodeInfo =>
          // Not very transparent: prefix is "display name" and namespace URI is "string value"
          val prefix = nodeInfo.getDisplayName
          val nsUri = nodeInfo.getStringValue
          (prefix -> nsUri)
        }
      result.toMap
    }

    Scope.from(resultMap - "xml")
  }

  override def toNodeOption: Option[simple.Node] = Some(toElem)

  def toElem: simple.Elem = {
    import simple.Node._

    elem(qname, attributes, scope, children.flatMap(_.toNodeOption))
  }

  def path: Path = {
    if (parentOption.isEmpty) Path.Root
    else {
      val prevSiblingElems = previousSiblings collect { case e: SaxonElem => e }
      val cnt = prevSiblingElems.filter(e => e.resolvedName == this.resolvedName).size
      val entry = Path.Entry(this.resolvedName, cnt)

      // Recursive call
      parent.path.append(entry)
    }
  }

  private def previousSiblings: immutable.IndexedSeq[SaxonNode] = {
    val it = wrappedNode.iterateAxis(AxisInfo.PRECEDING_SIBLING)

    val nodes = Stream.continually(it.next).takeWhile(_ ne null).toVector

    nodes.flatMap(nodeInfo => SaxonNode.wrapNodeOption(nodeInfo))
  }
}

final class SaxonText(override val wrappedNode: NodeInfo) extends SaxonNode(wrappedNode) with ResolvedNodes.Text {
  require(wrappedNode ne null)
  require(wrappedNode.getNodeKind == Type.TEXT || wrappedNode.getNodeKind == Type.WHITESPACE_TEXT)

  def text: String = wrappedNode.getStringValue

  def trimmedText: String = text.trim

  def normalizedText: String = XmlStringUtils.normalizeString(text)

  override def toNodeOption: Option[simple.Node] = Some(simple.Text(text, false))
}

final class SaxonProcessingInstruction(override val wrappedNode: NodeInfo) extends SaxonNode(wrappedNode) with Nodes.ProcessingInstruction {
  require(wrappedNode ne null)
  require(wrappedNode.getNodeKind == Type.PROCESSING_INSTRUCTION)

  // TODO
  override def toNodeOption: Option[simple.Node] = None

  def target: String = wrappedNode.getDisplayName

  def data: String = wrappedNode.getStringValue
}

final class SaxonComment(override val wrappedNode: NodeInfo) extends SaxonNode(wrappedNode) with Nodes.Comment {
  require(wrappedNode ne null)
  require(wrappedNode.getNodeKind == Type.COMMENT)

  def text: String = wrappedNode.getStringValue

  override def toNodeOption: Option[simple.Node] = Some(simple.Comment(text))
}

object SaxonNode {

  def wrapNodeOption(node: NodeInfo): Option[SaxonNode] = {
    node.getNodeKind match {
      case Type.ELEMENT                => Some(new SaxonElem(node))
      case Type.TEXT                   => Some(new SaxonText(node))
      case Type.WHITESPACE_TEXT        => Some(new SaxonText(node))
      case Type.PROCESSING_INSTRUCTION => Some(new SaxonProcessingInstruction(node))
      case Type.COMMENT                => Some(new SaxonComment(node))
      case _                           => None
    }
  }

  def wrapElement(elm: NodeInfo): SaxonElem = new SaxonElem(elm)
}
