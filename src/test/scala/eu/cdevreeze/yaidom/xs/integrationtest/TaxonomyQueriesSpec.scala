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
package integrationtest

import java.io._
import java.net.{ URI, URL }
import javax.xml.parsers.SAXParserFactory
import org.xml.sax.{ EntityResolver, InputSource }
import scala.collection.immutable
import org.scalatest.{ FeatureSpec, GivenWhenThen }
import eu.cdevreeze.yaidom.xs.schema._

class TaxonomyQueriesSpec extends FeatureSpec with GivenWhenThen {

  val nsXLink = "http://www.w3.org/1999/xlink"
  val nsXL = "http://www.xbrl.org/2003/XLink"
  val nsLink = "http://www.xbrl.org/2003/linkbase"
  val nsXbrli = "http://www.xbrl.org/2003/instance"
  val nsXbrldt = "http://xbrl.org/2005/xbrldt"
  val nsVer = "http://xbrl.org/2010/versioning-base"
  val nsSbr = "http://www.nltaxonomie.nl/2011/xbrl/xbrl-syntax-extension"

  val docParser: parse.DocumentParser = {
    trait MyEntityResolver extends EntityResolver {
      override def resolveEntity(publicId: String, systemId: String): InputSource = {
        new InputSource(new StringReader(""))
      }
    }

    val spf = SAXParserFactory.newInstance
    spf.setFeature("http://xml.org/sax/features/namespaces", true)
    spf.setFeature("http://xml.org/sax/features/namespace-prefixes", true)

    val docParser = parse.DocumentParserUsingSax.newInstance(
      spf,
      () => new parse.DefaultElemProducingSaxHandler with MyEntityResolver)

    docParser
  }

  val roots: immutable.IndexedSeq[File] = {
    val uri = classOf[TaxonomyQueriesSpec].getResource("/nltax-7.0").toURI
    val root = new File(uri)
    assert(root.exists && root.isDirectory, "Root " + root.toString + " must be an existing directory")
    Vector(root)
  }

  val docs: immutable.IndexedSeq[indexed.Document] = {
    def findFiles(root: File): Vector[File] = {
      require(root.isDirectory)
      root.listFiles.toVector flatMap { f => if (f.isDirectory) findFiles(f) else if (f.isFile) Vector(f) else Vector() }
    }

    val files = roots flatMap { root => findFiles(root) } filter { f =>
      f.getName.endsWith(".xml") || f.getName.endsWith(".xsd")
    }

    val docs = files map { f =>
      val doc = docParser.parse(new FileInputStream(f))
      indexed.Document(doc.withBaseUriOption(Some(f.toURI)))
    }

    docs
  }

  val schemaDocs: Map[URI, SchemaDocument] = {
    val result =
      docs filter { doc =>
        doc.document.baseUriOption.getOrElse("").toString.endsWith(".xsd")
      } map { doc => (doc.baseUriOption.getOrElse(sys.error("Missing URI")) -> new SchemaDocument(doc)) }
    result.toMap
  }

  val linkbaseDocs: Map[URI, indexed.Document] = {
    val result =
      docs filter { doc =>
        doc.documentElement.resolvedName == EName(nsLink, "linkbase")
      } map { doc => (doc.baseUriOption.getOrElse(sys.error("Missing URI")) -> doc) }
    result.toMap
  }

  val schemaDocSet: SchemaDocumentSet = new SchemaDocumentSet(schemaDocs.values.toVector)

  feature("The API user can query for the total counts of parsed schemas and linkbases") {

    scenario("All parsed schemas are found") {

      Given("the parsed schemas and linkbases")
      When("asking for the number of parsed schemas")
      val schemaCount = schemaDocs.size

      Then("more than 100 schemas are found")
      expectResult(100) {
        100.min(schemaCount)
      }

      info("In fact, found %d schemas".format(schemaDocs.size))
      info("Combined these schemas contain %d global element declarations".format(schemaDocSet.findAllGlobalElementDeclarations.size))
    }

    scenario("All parsed linkbases are found") {

      Given("the parsed schemas and linkbases")
      When("asking for the number of parsed linkbases")
      val linkbaseCount = linkbaseDocs.size

      Then("more than 100 linkbases are found")
      expectResult(100) {
        100.min(linkbaseCount)
      }

      info("In fact, found %d linkbases".format(linkbaseDocs.size))
    }
  }

  feature("The API user can query for substitution groups of element declarations") {

    scenario("Presentation tuples in kvk-tuples.xsd are found") {

      Given("the first global element declaration in kvk-tuples.xsd")
      val schemaDoc = schemaDocs.values.find(doc =>
        doc.wrappedDocument.baseUriOption.map(_.toString).getOrElse("").endsWith("kvk-tuples.xsd")).get

      val elemDecl = schemaDoc.schema.findAllGlobalElementDeclarations.find(e =>
        (e \@ "id") == Some("kvk-t_ContactForDocumentPresentation")).get

      When("asking for its substitution group")
      val substGroupOption = elemDecl.substitutionGroupOption

      Then("it is found, and equals sbr:presentationTuple")
      expectResult(Some(EName(nsSbr, "presentationTuple"))) {
        substGroupOption
      }
    }

    scenario("Only presentation tuples and specification tuples are found at top level in kvk-tuples.xsd") {

      Given("all global element declarations in kvk-tuples.xsd")
      val schemaDoc = schemaDocs.values.find(doc =>
        doc.wrappedDocument.baseUriOption.map(_.toString).getOrElse("").endsWith("kvk-tuples.xsd")).get

      val elemDecls = schemaDoc.schema.findAllGlobalElementDeclarations

      When("asking for their substitution groups")
      val substGroups = elemDecls flatMap { _.substitutionGroupOption }

      Then("only sbr:presentationTuple and sbr:specificationTuple are found")
      expectResult(Set(EName(nsSbr, "presentationTuple"), EName(nsSbr, "specificationTuple"))) {
        substGroups.toSet
      }
    }

    scenario("Presentation tuples and specification tuples are tuples themselves") {

      Given("substitution groups sbr:presentationTuple and sbr:specificationTuple")
      val substGroups = Set(EName(nsSbr, "presentationTuple"), EName(nsSbr, "specificationTuple"))

      When("asking for their substitution group heads")
      val substGroupAncestries = substGroups map { substGroup => schemaDocSet.findSubstitutionGroupAncestry(substGroup) }

      Then("these element declarations themselves have substitution group xbrli:tuple (xbrli:tuple is the subst. group head)")
      expectResult(Set(EName(nsXbrli, "tuple"))) {
        val result = substGroupAncestries map { _.last }
        result.toSet
      }
    }

    scenario("Only global element declarations can have substitution groups") {

      Given("all local element declarations")
      val elemDecls = schemaDocSet filterElementDeclarations { e => !e.isGlobal }

      When("asking for their substitution groups")
      val substGroups = elemDecls.flatMap(_.substitutionGroupOption).toSet

      Then("no substitution groups are found")
      expectResult(Set()) {
        substGroups
      }
    }

    scenario("File xbrl-syntax-extension.xsd introduces some concept substitution groups") {

      Given("the xbrl-syntax-extension.xsd schema")
      val schemaDoc = schemaDocs.values.find(doc =>
        doc.wrappedDocument.baseUriOption.map(_.toString).getOrElse("").endsWith("xbrl-syntax-extension.xsd")).get

      When("asking for its abstract global element declarations")
      val elemDecls = schemaDoc.schema filterGlobalElementDeclarations { e => e.isAbstract }

      Then("all these element declarations are in the xbrli:item or xbrli:tuple substitution group")
      val expectedSubstGroupOptions = Set(Some(EName(nsXbrli, "item")), Some(EName(nsXbrli, "tuple")))

      expectResult(expectedSubstGroupOptions) {
        elemDecls.map(e => e.substitutionGroupOption).toSet
      }

      And("they are all in the same 'sbr' (http://www.nltaxonomie.nl/2011/xbrl/xbrl-syntax-extension) target namespace")
      expectResult(List(Some(nsSbr))) {
        elemDecls.map(_.targetNamespaceOption).distinct
      }

      And("indeed all these element declarations are used as substitution groups")
      val allGlobalElemDecls = schemaDocSet.findAllGlobalElementDeclarations

      assert(elemDecls forall (elem => allGlobalElemDecls.find(e => e.substitutionGroupOption == elem.enameOption).isDefined))

      val substGroupQNames = elemDecls map { _.ename } map { ename => ename.toQName(Some("sbr")) }
      info("In fact, the substitution groups introduced in xbrl-syntax-extension.xsd are: " + substGroupQNames.mkString(", "))
    }

    scenario("When searching for all substitution groups (only) in www.nltaxonomie.nl, some expected ones should be present") {

      Given("all global element declarations in www.nltaxonomie.nl")
      val nltaxSchemaDocs = schemaDocSet.schemaDocuments filter (doc =>
        doc.wrappedDocument.baseUriOption.map(_.toString).getOrElse("").contains("/www.nltaxonomie.nl/"))
      val nltaxSchemaDocSet = new SchemaDocumentSet(nltaxSchemaDocs)
      val elemDecls = nltaxSchemaDocSet.findAllGlobalElementDeclarations

      When("asking for their substitution groups")
      val substGroups = elemDecls.flatMap(_.substitutionGroupOption).toSet

      Then("some expected substitution groups are found")
      assert(Set(
        EName(nsXbrli, "item"),
        EName(nsXbrli, "tuple"),
        EName(nsXbrldt, "hypercubeItem"),
        EName(nsXbrldt, "dimensionItem"),
        EName(nsVer, "event")).subsetOf(substGroups))

      info("All non-sbr substitution groups in www.nltaxonomie.nl: " + substGroups.filterNot(_.namespaceUriOption == Some(nsSbr)).mkString(", "))
    }

    scenario("Only tuple concepts are found at top level in kvk-tuples.xsd") {

      Given("schema kvk-tuples.xsd")
      val schemaDoc = schemaDocs.values.find(doc =>
        doc.wrappedDocument.baseUriOption.map(_.toString).getOrElse("").endsWith("kvk-tuples.xsd")).get

      When("finding global concepts (with some known substitution groups for tuples)")
      val conceptSubstGroups =
        Set(EName(nsXbrli, "tuple"), EName(nsSbr, "presentationTuple"), EName(nsSbr, "specificationTuple"))

      val topLevelConcepts = schemaDoc.schema.findAllDirectSubstitutables(conceptSubstGroups)

      Then("all global element declarations are found")
      expectResult(schemaDoc.schema.findAllGlobalElementDeclarations) {
        topLevelConcepts
      }
    }
  }
}
