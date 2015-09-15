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

package eu.cdevreeze.yaidomxs.testprogram

import java.io.File
import java.util.concurrent.atomic.AtomicInteger

import scala.Vector
import scala.reflect.classTag
import scala.util.Try

import net.sf.saxon.om.DocumentInfo
import net.sf.saxon.s9api.Processor
import eu.cdevreeze.yaidomxs.nodeinfo.SaxonBridgeElem
import eu.cdevreeze.yaidomxs.nodeinfo.SaxonDocument
import eu.cdevreeze.yaidom.bridge.DefaultIndexedBridgeElem
import eu.cdevreeze.yaidom.indexed
import eu.cdevreeze.yaidomxs.model
import eu.cdevreeze.yaidomxs.model.bridged.NamedTypeDefinition
import eu.cdevreeze.yaidomxs.model.bridged.XsdElem
import eu.cdevreeze.yaidomxs.model.bridged.XsdDocument

/**
 * Tries to load many schema documents using Saxon, and query them.
 *
 * @author Chris de Vreeze
 */
object TryToQueryManyDocumentsUsingSaxon {

  private val processor = new Processor(false)

  def main(args: Array[String]): Unit = {
    require(args.size == 1, s"Usage: TryToQueryManyDocumentsUsingSaxon <root directory>")
    val rootDir = new File(args(0))

    require(rootDir.isDirectory)

    val files = findFiles(rootDir, acceptFile)

    println(s"Found ${files.size} files")
    println("Parsing all files ...")

    val idx = new AtomicInteger(0)

    val docBuilder = processor.newDocumentBuilder()

    val docs = (files.par flatMap { f =>
      val currIdx = idx.getAndIncrement()
      if (currIdx % 1000 == 0 && currIdx > 0) println(s"Parsed ${currIdx} documents")

      val docOption = Try(docBuilder.build(f)).toOption
      docOption.foreach(d => d.getUnderlyingNode.setSystemId(f.toURI.toString))
      docOption
    }).seq.toVector

    println(s"Parsed ${docs.size} XML files. Now instantiating schema documents from them ...")

    val schemaDocs = docs flatMap { doc =>
      val tryDoc =
        Try(
          XsdDocument(
            XsdElem(
              SaxonBridgeElem.wrap(new SaxonDocument(doc.getUnderlyingNode.asInstanceOf[DocumentInfo]).documentElement), None)))
      if (tryDoc.isFailure) println(s"Could not instantiate schema document ${doc.getUnderlyingNode.getSystemId}")
      tryDoc.toOption
    }

    println(s"Instantiated ${schemaDocs.size} schema documents.")

    println("Starting querying (schemas) ...")
    println()

    schemaDocs.foreach(performSchemaQueries)

    println("Querying again (schemas) ...")
    println()

    schemaDocs.foreach(performSchemaQueries)

    println("Ready!")
  }

  private def acceptFile(f: File): Boolean =
    f.isFile && Set(".xsd").exists(s => f.getName.endsWith(s))

  private def findFiles(dir: File, p: File => Boolean): Vector[File] = {
    require(dir.isDirectory)
    val files = Try(dir.listFiles.toVector).toOption.getOrElse(Vector())

    // Recursive calls
    files flatMap { f =>
      if (f.isFile) Vector(f).filter(p)
      else if (f.isDirectory) findFiles(f, p)
      else Vector()
    }
  }

  private def performSchemaQueries(doc: XsdDocument): Unit = {
    val globalElemDecls = doc.schemaRootElems.flatMap(_.findAllGlobalElementDeclarations)

    val namedTypeDefs = doc.schemaRootElems.flatMap(_.findAllChildElemsOfType(classTag[NamedTypeDefinition]))

    val enames = doc.schemaRootElems.flatMap(_.findAllElemsOrSelf.map(_.resolvedName)).toSet

    println(s"Schema document ${doc.uri} has ${globalElemDecls.size} global element declarations")
    println(s"Schema document ${doc.uri} has ${namedTypeDefs.size} named type definitions")
    println(s"Schema document ${doc.uri} has ${enames.size} different (qualified) XML element names")
    println()
  }
}
