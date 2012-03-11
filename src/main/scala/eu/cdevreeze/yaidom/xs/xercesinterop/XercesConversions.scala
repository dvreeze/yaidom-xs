package eu.cdevreeze.yaidom
package xs
package xercesinterop

import scala.collection.{ immutable, mutable }
import org.apache.xerces.{ xs => xxs }
import Compositor._
import ConstraintType._
import DerivationMethod._
import Facet._
import NSConstraintType._
import Ordered._
import ProcessContents._
import DeclarationScope._
import Variety._

object XercesConversions {

  def convertXercesElementDeclaration(xercesObject: xxs.XSElementDeclaration): XSElementDeclaration =
    ConvertXercesElementDeclaration(xercesObject, Map[xxs.XSObject, XSObject]()).resultValue

  private type Cache = Map[xxs.XSObject, XSObject]

  private final class ConversionResult[+B <: XSObject](
    val resultValue: B,
    val cache: Cache) extends Immutable {

    require(resultValue ne null)
    require(cache ne null)
  }

  private type Converter[A <: xxs.XSObject, B <: XSObject] = (A, Cache) => ConversionResult[B]

  private object ConvertXercesXSObject extends Converter[xxs.XSObject, XSObject] {

    def apply(xercesObject: xxs.XSObject, cache: Cache): ConversionResult[XSObject] = xercesObject match {
      case elementDecl: xxs.XSElementDeclaration => ConvertXercesElementDeclaration(elementDecl, cache)
      // TODO ...
    }
  }

  private object ConvertXercesElementDeclaration extends Converter[xxs.XSElementDeclaration, XSElementDeclaration] {

    def apply(xercesObject: xxs.XSElementDeclaration, cache: Cache): ConversionResult[XSElementDeclaration] = {
      import XSElementDeclaration._

      cache.get(xercesObject) collect { case obj: XSElementDeclaration => new ConversionResult(obj, cache) } getOrElse {
        var tempCache = cache

        val obj = new XSElementDeclaration(
          nameOption = Option(xercesObject.getName),
          targetNamespaceOption = Option(xercesObject.getNamespace),
          typeDefinition = {
            val result = ConvertXercesTypeDefinition(xercesObject.getTypeDefinition, tempCache)
            tempCache = result.cache
            result.resultValue
          },
          scope = convertXercesScope(xercesObject.getScope),
          constraintInfo = new ConstraintInfo(
            constraintType = convertXercesConstraintInfo(xercesObject.getConstraintType),
            constraintValueOption = Option(xercesObject.getActualVC) map { v => new XSValue(v) }),
          isNillable = xercesObject.getNillable,
          substitutionInfo = new SubstitutionInfo(
            substitutionGroupAffiliationOption = {
              val xercesResultOption = Option(xercesObject.getSubstitutionGroupAffiliation)

              if (xercesResultOption.isEmpty) None
              else if (xercesResultOption.get == xercesObject) None
              else {
                val result = ConvertXercesElementDeclaration(xercesResultOption.get, tempCache)
                tempCache = result.cache
                Option(result.resultValue)
              }
            },
            disallowedSubstitutions = {
              val xercesResult: Short = xercesObject.getDisallowedSubstitutions

              val derivationByExtensionOption: Option[DerivationMethod] =
                if ((xercesResult.toInt & 0x01) == 0) None else Some(DerivationByExtension)
              val derivationByRestrictionOption: Option[DerivationMethod] =
                if ((xercesResult.toInt & 0x02) == 0) None else Some(DerivationByRestriction)
              val derivationBySubstitutionOption: Option[DerivationMethod] =
                if ((xercesResult.toInt & 0x04) == 0) None else Some(DerivationBySubstitution)

              Set(derivationByExtensionOption, derivationByRestrictionOption, derivationBySubstitutionOption).flatten
            },
            substitutionGroupExclusions = {
              val xercesResult: Short = xercesObject.getSubstitutionGroupExclusions

              val derivationByExtensionOption: Option[DerivationMethod] =
                if ((xercesResult.toInt & 0x01) == 0) None else Some(DerivationByExtension)
              val derivationByRestrictionOption: Option[DerivationMethod] =
                if ((xercesResult.toInt & 0x02) == 0) None else Some(DerivationByRestriction)

              Set(derivationByExtensionOption, derivationByRestrictionOption).flatten
            }),
          isAbstract = xercesObject.getAbstract,
          annotations = immutable.IndexedSeq()) // TODO

        new ConversionResult(obj, tempCache)
      }
    }
  }

  private object ConvertXercesTypeDefinition extends Converter[xxs.XSTypeDefinition, XSTypeDefinition] {

    def apply(xercesObject: xxs.XSTypeDefinition, cache: Cache): ConversionResult[XSTypeDefinition] = xercesObject match {
      case typeDef: xxs.XSSimpleTypeDefinition => ConvertXercesSimpleTypeDefinition(typeDef, cache)
      case typeDef: xxs.XSComplexTypeDefinition => ConvertXercesComplexTypeDefinition(typeDef, cache)
    }
  }

  private object ConvertXercesSimpleTypeDefinition extends Converter[xxs.XSSimpleTypeDefinition, XSSimpleTypeDefinition] {

    def apply(xercesObject: xxs.XSSimpleTypeDefinition, cache: Cache): ConversionResult[XSSimpleTypeDefinition] = {
      cache.get(xercesObject) collect { case obj: XSSimpleTypeDefinition => new ConversionResult(obj, cache) } getOrElse {
        var tempCache = cache

        val obj = new XSSimpleTypeDefinition(
          nameOption = Option(xercesObject.getName),
          targetNamespaceOption = Option(xercesObject.getNamespace),
          baseTypeOption = {
            val xercesResultOption = Option(xercesObject.getBaseType)

            if (xercesResultOption.isEmpty) None
            else if (xercesResultOption.get == xercesObject) None
            else {
              val result = ConvertXercesTypeDefinition(xercesResultOption.get, tempCache)
              tempCache = result.cache
              Option(result.resultValue)
            }
          },
          finalFor = {
            val xercesResult: Short = xercesObject.getFinal

            val derivationByExtensionOption: Option[DerivationMethod] =
              if ((xercesResult.toInt & 0x01) == 0) None else Some(DerivationByExtension)
            val derivationByRestrictionOption: Option[DerivationMethod] =
              if ((xercesResult.toInt & 0x02) == 0) None else Some(DerivationByRestriction)
            val derivationUnionOption: Option[DerivationMethod] =
              if ((xercesResult.toInt & 0x08) == 0) None else Some(DerivationUnion)
            val derivationListOption: Option[DerivationMethod] =
              if ((xercesResult.toInt & 0x10) == 0) None else Some(DerivationList)

            Set(derivationByExtensionOption, derivationByRestrictionOption, derivationUnionOption, derivationListOption).flatten
          },
          varietyOption = convertXercesVariety(xercesObject.getVariety),
          primitiveTypeOption = {
            val xercesResultOption = Option(xercesObject.getPrimitiveType)

            if (xercesResultOption.isEmpty) None
            else if (xercesResultOption.get == xercesObject) None
            else {
              val result = ConvertXercesSimpleTypeDefinition(xercesResultOption.get, tempCache)
              tempCache = result.cache
              Option(result.resultValue)
            }
          },
          builtInKind = TypeCategory.fromShortValue(xercesObject.getBuiltInKind),
          itemTypeOption = {
            val xercesResultOption = Option(xercesObject.getItemType)

            if (xercesResultOption.isEmpty) None
            else if (xercesResultOption.get == xercesObject) None
            else {
              val result = ConvertXercesSimpleTypeDefinition(xercesResultOption.get, tempCache)
              tempCache = result.cache
              Option(result.resultValue)
            }
          },
          memberTypes = {
            val xercesResults = xercesObjectListToSeq(xercesObject.getMemberTypes)(classOf[xxs.XSSimpleTypeDefinition])

            xercesResults map { xercesResult =>
              val result = ConvertXercesSimpleTypeDefinition(xercesResult, tempCache)
              tempCache = result.cache
              result.resultValue
            }
          },
          definedFacets = convertXercesFacets(xercesObject.getDefinedFacets),
          fixedFacets = convertXercesFacets(xercesObject.getFixedFacets),
          lexicalEnumeration = xercesStringListToSeq(xercesObject.getLexicalEnumeration),
          lexicalPattern = xercesStringListToSeq(xercesObject.getLexicalPattern),
          ordered = convertXercesOrdered(xercesObject.getOrdered),
          isFinite = xercesObject.getFinite,
          isBounded = xercesObject.getBounded,
          isNumeric = xercesObject.getNumeric,
          facets = immutable.IndexedSeq(), // TODO
          multiValueFacets = immutable.IndexedSeq(), // TODO
          annotations = immutable.IndexedSeq()) // TODO

        new ConversionResult(obj, tempCache)
      }
    }
  }

  private object ConvertXercesComplexTypeDefinition extends Converter[xxs.XSComplexTypeDefinition, XSComplexTypeDefinition] {

    def apply(xercesObject: xxs.XSComplexTypeDefinition, cache: Cache): ConversionResult[XSComplexTypeDefinition] = {
      cache.get(xercesObject) collect { case obj: XSComplexTypeDefinition => new ConversionResult(obj, cache) } getOrElse {
        var tempCache = cache

        val obj = new XSComplexTypeDefinition(
          nameOption = Option(xercesObject.getName),
          targetNamespaceOption = Option(xercesObject.getNamespace),
          baseTypeOption = {
            val xercesResultOption = Option(xercesObject.getBaseType)

            if (xercesResultOption.isEmpty) None
            else if (xercesResultOption.get == xercesObject) None
            else {
              val result = ConvertXercesTypeDefinition(xercesResultOption.get, tempCache)
              tempCache = result.cache
              Option(result.resultValue)
            }
          },
          finalFor = {
            val xercesResult: Short = xercesObject.getFinal

            val derivationByExtensionOption: Option[DerivationMethod] =
              if ((xercesResult.toInt & 0x01) == 0) None else Some(DerivationByExtension)
            val derivationByRestrictionOption: Option[DerivationMethod] =
              if ((xercesResult.toInt & 0x02) == 0) None else Some(DerivationByRestriction)

            Set(derivationByExtensionOption, derivationByRestrictionOption).flatten
          },
          derivationMethodOption = xercesObject.getDerivationMethod match {
            case xxs.XSConstants.DERIVATION_EXTENSION => Some(DerivationByExtension)
            case xxs.XSConstants.DERIVATION_RESTRICTION => Some(DerivationByRestriction)
            case xxs.XSConstants.DERIVATION_NONE => None
          },
          isAbstract = xercesObject.getAbstract,
          attributeUses = {
            val xercesResults = xercesObjectListToSeq(xercesObject.getAttributeUses)(classOf[xxs.XSAttributeUse])

            xercesResults map { xercesResult =>
              val result = ConvertXercesAttributeUse(xercesResult, tempCache)
              tempCache = result.cache
              result.resultValue
            }
          },
          attributeWildcardOption = {
            val xercesResultOption = Option(xercesObject.getAttributeWildcard)

            if (xercesResultOption.isEmpty) None
            else {
              val result = ConvertXercesWildcard(xercesResultOption.get, tempCache)
              tempCache = result.cache
              Option(result.resultValue)
            }
          },
          simpleTypeOption = {
            val xercesResultOption = Option(xercesObject.getSimpleType)

            if (xercesResultOption.isEmpty) None
            else {
              val result = ConvertXercesSimpleTypeDefinition(xercesResultOption.get, tempCache)
              tempCache = result.cache
              Option(result.resultValue)
            }
          },
          particleOption = {
            val xercesResultOption = Option(xercesObject.getParticle)

            if (xercesResultOption.isEmpty) None
            else {
              val result = ConvertXercesParticle(xercesResultOption.get, tempCache)
              tempCache = result.cache
              Option(result.resultValue)
            }
          },
          prohibitedSubstitutions = {
            val xercesResult: Short = xercesObject.getProhibitedSubstitutions

            val derivationByExtensionOption: Option[DerivationMethod] =
              if ((xercesResult.toInt & 0x01) == 0) None else Some(DerivationByExtension)
            val derivationByRestrictionOption: Option[DerivationMethod] =
              if ((xercesResult.toInt & 0x02) == 0) None else Some(DerivationByRestriction)

            Set(derivationByExtensionOption, derivationByRestrictionOption).flatten
          },
          annotations = immutable.IndexedSeq()) // TODO

        new ConversionResult(obj, tempCache)
      }
    }
  }

  private object ConvertXercesTerm extends Converter[xxs.XSTerm, XSTerm] {

    def apply(xercesObject: xxs.XSTerm, cache: Cache): ConversionResult[XSTerm] = xercesObject match {
      case elementDecl: xxs.XSElementDeclaration => ConvertXercesElementDeclaration(elementDecl, cache)
      case modelGroup: xxs.XSModelGroup => ConvertXercesModelGroup(modelGroup, cache)
      case wildcard: xxs.XSWildcard => ConvertXercesWildcard(wildcard, cache)
    }
  }

  private object ConvertXercesParticle extends Converter[xxs.XSParticle, XSParticle] {

    def apply(xercesObject: xxs.XSParticle, cache: Cache): ConversionResult[XSParticle] = {
      cache.get(xercesObject) collect { case obj: XSParticle => new ConversionResult(obj, cache) } getOrElse {
        var tempCache = cache

        val obj = new XSParticle(
          nameOption = Option(xercesObject.getName),
          targetNamespaceOption = Option(xercesObject.getNamespace),
          minOccursOption = Some(xercesObject.getMinOccurs),
          maxOccursOption = if (xercesObject.getMaxOccursUnbounded) None else Some(xercesObject.getMaxOccurs),
          maxOccursUnbounded = xercesObject.getMaxOccursUnbounded,
          term = {
            val xercesResult = xercesObject.getTerm

            val result = ConvertXercesTerm(xercesResult, tempCache)
            tempCache = result.cache
            result.resultValue
          })

        new ConversionResult(obj, tempCache)
      }
    }
  }

  private object ConvertXercesAttributeUse extends Converter[xxs.XSAttributeUse, XSAttributeUse] {

    def apply(xercesObject: xxs.XSAttributeUse, cache: Cache): ConversionResult[XSAttributeUse] = {
      cache.get(xercesObject) collect { case obj: XSAttributeUse => new ConversionResult(obj, cache) } getOrElse {
        import XSAttributeUse._

        var tempCache = cache

        val obj = new XSAttributeUse(
          nameOption = Option(xercesObject.getName),
          targetNamespaceOption = Option(xercesObject.getNamespace),
          isRequired = xercesObject.getRequired,
          attributeDeclaration = {
            val xercesResult = xercesObject.getAttrDeclaration

            val result = ConvertXercesAttributeDeclaration(xercesResult, tempCache)
            tempCache = result.cache
            result.resultValue
          },
          constraintInfo = new ConstraintInfo(
            constraintType = convertXercesConstraintInfo(xercesObject.getConstraintType),
            constraintValueOption = Option(xercesObject.getActualVC) map { v => new XSValue(v) }),
          annotations = immutable.IndexedSeq()) // TODO

        new ConversionResult(obj, tempCache)
      }
    }
  }

  private object ConvertXercesAttributeDeclaration extends Converter[xxs.XSAttributeDeclaration, XSAttributeDeclaration] {

    def apply(xercesObject: xxs.XSAttributeDeclaration, cache: Cache): ConversionResult[XSAttributeDeclaration] = {
      cache.get(xercesObject) collect { case obj: XSAttributeDeclaration => new ConversionResult(obj, cache) } getOrElse {
        import XSAttributeDeclaration._

        var tempCache = cache

        val obj = new XSAttributeDeclaration(
          nameOption = Option(xercesObject.getName),
          targetNamespaceOption = Option(xercesObject.getNamespace),
          typeDefinition = {
            val result = ConvertXercesSimpleTypeDefinition(xercesObject.getTypeDefinition, tempCache)
            tempCache = result.cache
            result.resultValue
          },
          scope = convertXercesScope(xercesObject.getScope),
          constraintInfo = new ConstraintInfo(
            constraintType = convertXercesConstraintInfo(xercesObject.getConstraintType),
            constraintValueOption = Option(xercesObject.getActualVC) map { v => new XSValue(v) }),
          annotations = immutable.IndexedSeq()) // TODO

        new ConversionResult(obj, tempCache)
      }
    }
  }

  private object ConvertXercesModelGroup extends Converter[xxs.XSModelGroup, XSModelGroup] {

    def apply(xercesObject: xxs.XSModelGroup, cache: Cache): ConversionResult[XSModelGroup] = {
      cache.get(xercesObject) collect { case obj: XSModelGroup => new ConversionResult(obj, cache) } getOrElse {
        var tempCache = cache

        val obj = new XSModelGroup(
          nameOption = Option(xercesObject.getName),
          targetNamespaceOption = Option(xercesObject.getNamespace),
          compositor = xercesObject.getCompositor match {
            case xxs.XSModelGroup.COMPOSITOR_ALL => CompositorAll
            case xxs.XSModelGroup.COMPOSITOR_CHOICE => CompositorChoice
            case xxs.XSModelGroup.COMPOSITOR_SEQUENCE => CompositorSequence
          },
          particles = {
            val xercesResults = xercesObjectListToSeq(xercesObject.getParticles)(classOf[xxs.XSParticle])

            xercesResults map { xercesResult =>
              val result = ConvertXercesParticle(xercesResult, tempCache)
              tempCache = result.cache
              result.resultValue
            }
          },
          annotations = immutable.IndexedSeq()) // TODO

        new ConversionResult(obj, tempCache)
      }
    }
  }

  private object ConvertXercesWildcard extends Converter[xxs.XSWildcard, XSWildcard] {

    def apply(xercesObject: xxs.XSWildcard, cache: Cache): ConversionResult[XSWildcard] = {
      cache.get(xercesObject) collect { case obj: XSWildcard => new ConversionResult(obj, cache) } getOrElse {
        var tempCache = cache

        val obj = new XSWildcard(
          nameOption = Option(xercesObject.getName),
          targetNamespaceOption = Option(xercesObject.getNamespace),
          constraintType = xercesObject.getConstraintType match {
            case xxs.XSWildcard.NSCONSTRAINT_ANY => NSConstraintAny
            case xxs.XSWildcard.NSCONSTRAINT_LIST => NSConstraintList
            case xxs.XSWildcard.NSCONSTRAINT_NOT => NSConstraintNot
          },
          nsConstraintList = xercesStringListToSeq(xercesObject.getNsConstraintList),
          processContents = xercesObject.getProcessContents match {
            case xxs.XSWildcard.PC_SKIP => ProcessContentsSkip
            case xxs.XSWildcard.PC_LAX => ProcessContentsLax
            case xxs.XSWildcard.PC_STRICT => ProcessContentsStrict
          },
          annotations = immutable.IndexedSeq()) // TODO

        new ConversionResult(obj, tempCache)
      }
    }
  }

  private object ConvertXercesAnnotation extends Converter[xxs.XSAnnotation, XSAnnotation] {

    def apply(xercesObject: xxs.XSAnnotation, cache: Cache): ConversionResult[XSAnnotation] = {
      null // TODO
    }
  }

  private def convertXercesScope(xercesScope: Short): DeclarationScope = xercesScope match {
    case xxs.XSConstants.SCOPE_ABSENT => ScopeAbsent
    case xxs.XSConstants.SCOPE_GLOBAL => ScopeGlobal
    case xxs.XSConstants.SCOPE_LOCAL => ScopeLocal
  }

  private def convertXercesConstraintInfo(xercesConstraintInfo: Short): ConstraintType = xercesConstraintInfo match {
    case xxs.XSConstants.VC_DEFAULT => ConstraintTypeDefault
    case xxs.XSConstants.VC_FIXED => ConstraintTypeDefault
    case xxs.XSConstants.VC_NONE => ConstraintTypeNone
  }

  private def convertXercesVariety(xercesVariety: Short): Option[Variety] = xercesVariety match {
    case xxs.XSSimpleTypeDefinition.VARIETY_ABSENT => None
    case xxs.XSSimpleTypeDefinition.VARIETY_ATOMIC => Some(VarietyAtomic)
    case xxs.XSSimpleTypeDefinition.VARIETY_LIST => Some(VarietyList)
    case xxs.XSSimpleTypeDefinition.VARIETY_UNION => Some(VarietyUnion)
  }

  private def convertXercesOrdered(xercesOrdered: Short): Ordered = xercesOrdered match {
    case xxs.XSSimpleTypeDefinition.ORDERED_FALSE => OrderedFalse
    case xxs.XSSimpleTypeDefinition.ORDERED_PARTIAL => OrderedPartial
    case xxs.XSSimpleTypeDefinition.ORDERED_TOTAL => OrderedTotal
  }

  private def convertXercesFacets(xercesFacets: Short): Set[Facet] = {
    Set[Option[Facet]](
      if ((xercesFacets.toInt & 0x01) == 0) None else Some(FacetLength),
      if ((xercesFacets.toInt & 0x02) == 0) None else Some(FacetMinLength),
      if ((xercesFacets.toInt & 0x04) == 0) None else Some(FacetMaxLength),
      if ((xercesFacets.toInt & 0x08) == 0) None else Some(FacetPattern),
      if ((xercesFacets.toInt & 0x10) == 0) None else Some(FacetWhitespace),
      if ((xercesFacets.toInt & 0x20) == 0) None else Some(FacetMaxInclusive),
      if ((xercesFacets.toInt & 0x40) == 0) None else Some(FacetMaxExclusive),
      if ((xercesFacets.toInt & 0x80) == 0) None else Some(FacetMinExclusive),
      if ((xercesFacets.toInt & 0x100) == 0) None else Some(FacetMinInclusive),
      if ((xercesFacets.toInt & 0x200) == 0) None else Some(FacetTotalDigits),
      if ((xercesFacets.toInt & 0x400) == 0) None else Some(FacetFractionDigits),
      if ((xercesFacets.toInt & 0x800) == 0) None else Some(FacetEnumeration)).flatten
  }

  private def xercesObjectListToSeq[A](xercesObjectList: xxs.XSObjectList)(cls: Class[A]): immutable.IndexedSeq[A] = {
    (0 until xercesObjectList.getLength).toIndexedSeq map { (i: Int) => xercesObjectList.item(i).asInstanceOf[A] }
  }

  private def xercesStringListToSeq(xercesStringList: xxs.StringList): immutable.IndexedSeq[String] = {
    (0 until xercesStringList.getLength).toIndexedSeq map { (i: Int) => xercesStringList.item(i) }
  }
}
