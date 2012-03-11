package eu.cdevreeze.yaidom
package xs
package integrationtest

import java.{ util => jutil, io => jio }
import org.w3c.dom.{ Element }
import org.w3c.dom.bootstrap.DOMImplementationRegistry
import org.w3c.dom.ls.{ LSResourceResolver, LSInput, DOMImplementationLS }
import org.xml.sax.{ EntityResolver, InputSource, ErrorHandler, SAXParseException }
import javax.xml.XMLConstants
import javax.xml.transform.stream.StreamSource
import javax.xml.parsers.{ DocumentBuilderFactory, DocumentBuilder }
import javax.xml.validation.{ SchemaFactory, Schema }
import scala.collection.{ immutable, mutable }
import scala.collection.JavaConverters._
import org.apache.xerces
import org.junit.{ Test, Before, Ignore }
import org.junit.runner.RunWith
import org.scalatest.{ Suite, BeforeAndAfterAll }
import org.scalatest.junit.JUnitRunner
import xercesinterop.XercesConversions._

@RunWith(classOf[JUnitRunner])
class LoadModelTest extends Suite {

  private val logger: jutil.logging.Logger = jutil.logging.Logger.getLogger("eu.cdevreeze.yaidom.xs")

  @Test def testShowElementDeclarationsFromPsvi() {
    // See http://xerces.apache.org/xerces2-j/faq-dom.html
    // For the specific code, see http://www.sts.tu-harburg.de/~se.bossung/xerces-example/index.html

    // For the Xerces XS API, see http://xerces.apache.org/xerces2-j/javadocs/xs/index.html.

    val documentClassName = "org.apache.xerces.dom.PSVIDocumentImpl"
    require(Class.forName(documentClassName) ne null, "Expected JDK class '%s' to be found".format(documentClassName))

    val dbf = DocumentBuilderFactory.newInstance("org.apache.xerces.jaxp.DocumentBuilderFactoryImpl", classOf[LoadModelTest].getClassLoader)
    dbf.setNamespaceAware(true)
    dbf.setValidating(true)
    dbf.setAttribute("http://java.sun.com/xml/jaxp/properties/schemaLanguage", "http://www.w3.org/2001/XMLSchema")
    dbf.setAttribute("http://apache.org/xml/features/validation/schema", java.lang.Boolean.TRUE)
    dbf.setAttribute("http://apache.org/xml/properties/dom/document-class-name", documentClassName)

    val inputSources = Array(new InputSource(classOf[LoadModelTest].getResourceAsStream("shiporder.xsd")))

    dbf.setAttribute("http://java.sun.com/xml/jaxp/properties/schemaSource", inputSources)

    def createDocumentBuilder(documentBuilderFactory: DocumentBuilderFactory): DocumentBuilder = {
      val db = documentBuilderFactory.newDocumentBuilder()
      db.setErrorHandler(new ErrorHandler {
        override def error(exc: SAXParseException) {
          logger.warning("Error: %s".format(exc))
        }

        override def fatalError(exc: SAXParseException) {
          logger.severe("Fatal error: %s".format(exc))
        }

        override def warning(exc: SAXParseException) {
          logger.warning("Warning: %s".format(exc))
        }
      })
      db
    }

    val is = classOf[LoadModelTest].getResourceAsStream("shiporder.xml")

    // The (converted) ElementPSVI and AttributePSVI instances in the PSVI-enriched DOM trees should point to the (converted) XSModel.
    // Should they become hard pointers? Or do we need to (partly?) support SCD (schema component designators), and store
    // these somehow in or with the yaidom node trees?

    try {
      val db: DocumentBuilder = createDocumentBuilder(dbf)
      val domDoc: org.w3c.dom.Document = db.parse(is)

      val domRootElm = domDoc.getDocumentElement.asInstanceOf[xerces.xs.ElementPSVI]
      val psviModel = domRootElm.getSchemaInformation()
      require(psviModel ne null)

      val xercesElmDeclNamedMap = psviModel.getComponents(xerces.xs.XSConstants.ELEMENT_DECLARATION)
      val xercesElmDecls = xercesElementDeclarationNamedMapToSeq(xercesElmDeclNamedMap)

      expect(1) {
        xercesElmDecls.size
      }

      val convertedElmDecls: immutable.IndexedSeq[XSElementDeclaration] =
        xercesElmDecls map { xercesElmDecl => convertXercesElementDeclaration(xercesElmDecl) }

      expect(xercesElmDecls.size) {
        convertedElmDecls.size
      }

      val xercesElementNames = xercesElmDecls map { decl => Option(decl.getNamespace) map { _.ns.ename(decl.getName) } getOrElse (decl.getName.ename) }
      expect(xercesElementNames) {
        convertedElmDecls flatMap { decl => decl.toInstanceExpandedNameOption }
      }
    } finally {
      if (is ne null) is.close()
    }
  }

  @Test def testShowElementDeclarationsFromLoadedSchema() {
    System.setProperty(DOMImplementationRegistry.PROPERTY, "org.apache.xerces.dom.DOMXSImplementationSourceImpl")
    val registry = DOMImplementationRegistry.newInstance()

    val xsImpl = registry.getDOMImplementation("XS-Loader").asInstanceOf[xerces.xs.XSImplementation]

    val schemaLoader = xsImpl.createXSLoader(null)

    val schemaUri = classOf[LoadModelTest].getResource("shiporder.xsd").toURI
    val psviModel = schemaLoader.loadURI(schemaUri.toString)
    require(psviModel ne null)

    val xercesElmDeclNamedMap = psviModel.getComponents(xerces.xs.XSConstants.ELEMENT_DECLARATION)
    val xercesElmDecls = xercesElementDeclarationNamedMapToSeq(xercesElmDeclNamedMap)

    expect(1) {
      xercesElmDecls.size
    }

    val convertedElmDecls: immutable.IndexedSeq[XSElementDeclaration] =
      xercesElmDecls map { xercesElmDecl => convertXercesElementDeclaration(xercesElmDecl) }

    expect(xercesElmDecls.size) {
      convertedElmDecls.size
    }

    val xercesElementNames = xercesElmDecls map { decl => Option(decl.getNamespace) map { _.ns.ename(decl.getName) } getOrElse (decl.getName.ename) }
    expect(xercesElementNames) {
      convertedElmDecls flatMap { decl => decl.toInstanceExpandedNameOption }
    }
  }

  @Test def testShowElementDeclarationsFromLoadedLargeSchema() {
    System.setProperty(DOMImplementationRegistry.PROPERTY, "org.apache.xerces.dom.DOMXSImplementationSourceImpl")
    val registry = DOMImplementationRegistry.newInstance()

    val xsImpl = registry.getDOMImplementation("XS-Loader").asInstanceOf[xerces.xs.XSImplementation]

    val schemaLoader = xsImpl.createXSLoader(null)
    schemaLoader.getConfig.setParameter("resource-resolver", new ResourceResolver(xsImpl.asInstanceOf[DOMImplementationLS]))

    val schemaUri = classOf[LoadModelTest].getResource("ifrs-gp-2006-08-15.xsd").toURI
    val psviModel = schemaLoader.loadURI(schemaUri.toString)
    require(psviModel ne null)

    val xercesElmDeclNamedMap = psviModel.getComponents(xerces.xs.XSConstants.ELEMENT_DECLARATION)
    val xercesElmDecls = xercesElementDeclarationNamedMapToSeq(xercesElmDeclNamedMap)

    require(xercesElmDecls.size >= 4000)
    expect(4143) {
      xercesElmDecls.size
    }

    val convertedElmDecls: immutable.IndexedSeq[XSElementDeclaration] =
      xercesElmDecls map { xercesElmDecl => convertXercesElementDeclaration(xercesElmDecl) }

    expect(xercesElmDecls.size) {
      convertedElmDecls.size
    }

    val xercesElementNames = xercesElmDecls map { decl => Option(decl.getNamespace) map { _.ns.ename(decl.getName) } getOrElse (decl.getName.ename) }
    expect(xercesElementNames) {
      convertedElmDecls flatMap { decl => decl.toInstanceExpandedNameOption }
    }
  }

  private def xercesElementDeclarationNamedMapToSeq(m: xerces.xs.XSNamedMap): immutable.IndexedSeq[xerces.xs.XSElementDeclaration] = {
    (0 until m.getLength).toIndexedSeq map { (i: Int) => m.item(i).asInstanceOf[xerces.xs.XSElementDeclaration] }
  }

  final class ResourceResolver(val domImpl: DOMImplementationLS) extends LSResourceResolver {

    def resolveResource(tpe: String, namespaceURI: String, publicId: String, systemId: String, baseURI: String): LSInput = {
      logger.info("Public ID: %s. System ID: %s".format(publicId, systemId))

      def resolveLocally(fileNameInSystemId: String): Boolean = {
        (systemId == fileNameInSystemId) || (systemId.endsWith("/" + fileNameInSystemId)) || (systemId.endsWith("\\" + fileNameInSystemId))
      }

      def createLocalLSInput(fileNameInSystemId: String): LSInput = {
        val lsInput = domImpl.createLSInput()
        lsInput.setBaseURI(baseURI)
        lsInput.setByteStream(classOf[LoadModelTest].getResourceAsStream(fileNameInSystemId))
        lsInput.setPublicId(publicId)
        lsInput.setSystemId(systemId)

        logger.info("Locally resolving '%s'".format(fileNameInSystemId))
        lsInput
      }

      if (resolveLocally("xbrl-instance-2003-12-31.xsd")) {
        createLocalLSInput("xbrl-instance-2003-12-31.xsd")
      } else if (resolveLocally("xbrl-linkbase-2003-12-31.xsd")) {
        createLocalLSInput("xbrl-linkbase-2003-12-31.xsd")
      } else if (resolveLocally("xl-2003-12-31.xsd")) {
        createLocalLSInput("xl-2003-12-31.xsd")
      } else if (systemId.endsWith("xlink-2003-12-31.xsd")) {
        createLocalLSInput("xlink-2003-12-31.xsd")
      } else if (systemId.endsWith("ifrs-gp-typ-2006-08-15.xsd")) {
        createLocalLSInput("ifrs-gp-typ-2006-08-15.xsd")
      } else if (systemId.endsWith("restatedLabel.xsd")) {
        createLocalLSInput("restatedLabel.xsd")
      } else null
    }
  }
}
