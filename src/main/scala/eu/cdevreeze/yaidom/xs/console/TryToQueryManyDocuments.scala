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

package eu.cdevreeze.yaidom.xs.console

import java.io.File
import scala.Vector
import scala.util.Try
import scala.reflect.classTag
import eu.cdevreeze.yaidom.parse.DocumentParserUsingSax
import eu.cdevreeze.yaidom.simple.Document
import eu.cdevreeze.yaidom.docaware
import eu.cdevreeze.yaidom.resolved
import eu.cdevreeze.yaidom.core.QName
import eu.cdevreeze.yaidom.xs.XsSchemaEName
import eu.cdevreeze.yaidom.xs.SchemaDocument
import eu.cdevreeze.yaidom.bridge.DefaultDocawareBridgeElem
import eu.cdevreeze.yaidom.xs.NamedTypeDefinition

/**
 * Tries to load many schema documents, and query them.
 *
 * @author Chris de Vreeze
 */
object TryToQueryManyDocuments {

  def main(args: Array[String]): Unit = {
    require(args.size == 1, s"Usage: TryToQueryManyDocuments <root directory>")
    val rootDir = new File(args(0))

    require(rootDir.isDirectory)

    val docParser = DocumentParserUsingSax.newInstance()

    val files = findFiles(rootDir, acceptFile)

    println(s"Found ${files.size} files")
    println("Parsing all files ...")

    val docs = (files.par flatMap { f =>
      val docOption = Try(docParser.parse(f).withUriOption(Some(f.toURI))).toOption
      val schemaDocOption = docOption filter (doc => doc.documentElement.resolvedName == XsSchemaEName)
      schemaDocOption map { doc =>
        new SchemaDocument(DefaultDocawareBridgeElem.wrap(docaware.Elem(doc.uriOption.get, doc.documentElement)))
      }
    }).seq.toVector

    println(s"Parsed ${docs.size} schema files")
    println("Starting querying ...")

    docs.foreach(performQueries)

    println("Querying again ...")

    docs.foreach(performQueries)

    println("Ready!")
  }

  private def acceptFile(f: File): Boolean =
    f.isFile && Set(".xsd", ".xml").exists(s => f.getName.endsWith(s))

  private def findFiles(dir: File, p: File => Boolean): Vector[File] = {
    require(dir.isDirectory)
    val files = dir.listFiles.toVector

    // Recursive calls
    files.flatMap(f => if (f.isFile) Vector(f).filter(p) else findFiles(f, p))
  }

  private def performQueries(doc: SchemaDocument): Unit = {
    val globalElemDecls = doc.schema.findAllGlobalElementDeclarations

    val namedTypeDefs = doc.schema.findAllChildElemsOfType(classTag[NamedTypeDefinition])

    val enames = doc.schema.findAllElemsOrSelf.map(_.resolvedName).toSet

    println(s"Schema document ${doc.uri} has ${globalElemDecls.size} global element declarations")
    println(s"Schema document ${doc.uri} has ${namedTypeDefs.size} named type definitions")
    println(s"Schema document ${doc.uri} has ${enames.size} different (qualified) XML element names")
    println()
  }
}
