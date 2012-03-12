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

final class XercesObjectConverter {

  @volatile private var cache: Map[xxs.XSObject, XSObject] = Map[xxs.XSObject, XSObject]()

  def convertXercesObject(xercesObject: xxs.XSObject): XSObject = xercesObject match {
    case elementDecl: xxs.XSElementDeclaration => convertXercesElementDeclaration(elementDecl)
    // TODO ...
  }

  def convertXercesElementDeclaration(xercesObject: xxs.XSElementDeclaration): XSElementDeclaration = {
    import XSElementDeclaration._

    cache.get(xercesObject) collect { case obj: XSElementDeclaration => logCacheHit(xercesObject); obj } getOrElse {
      val obj = new XSElementDeclaration(
        nameOption = Option(xercesObject.getName),
        targetNamespaceOption = Option(xercesObject.getNamespace),
        typeDefinition = convertXercesTypeDefinition(xercesObject.getTypeDefinition),
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
              val result = convertXercesElementDeclaration(xercesResultOption.get)
              Option(result)
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

      cache += (xercesObject -> obj)
      obj
    }
  }

  def convertXercesTypeDefinition(xercesObject: xxs.XSTypeDefinition): XSTypeDefinition = xercesObject match {
    case typeDef: xxs.XSSimpleTypeDefinition => convertXercesSimpleTypeDefinition(typeDef)
    case typeDef: xxs.XSComplexTypeDefinition => convertXercesComplexTypeDefinition(typeDef)
  }

  def convertXercesSimpleTypeDefinition(xercesObject: xxs.XSSimpleTypeDefinition): XSSimpleTypeDefinition = {
    cache.get(xercesObject) collect { case obj: XSSimpleTypeDefinition => logCacheHit(xercesObject); obj } getOrElse {
      val obj = new XSSimpleTypeDefinition(
        nameOption = Option(xercesObject.getName),
        targetNamespaceOption = Option(xercesObject.getNamespace),
        baseTypeOption = {
          val xercesResultOption = Option(xercesObject.getBaseType)

          if (xercesResultOption.isEmpty) None
          else if (xercesResultOption.get == xercesObject) None
          else {
            val result = convertXercesTypeDefinition(xercesResultOption.get)
            Option(result)
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
            val result = convertXercesSimpleTypeDefinition(xercesResultOption.get)
            Option(result)
          }
        },
        builtInKind = TypeCategory.fromShortValue(xercesObject.getBuiltInKind),
        itemTypeOption = {
          val xercesResultOption = Option(xercesObject.getItemType)

          if (xercesResultOption.isEmpty) None
          else if (xercesResultOption.get == xercesObject) None
          else {
            val result = convertXercesSimpleTypeDefinition(xercesResultOption.get)
            Option(result)
          }
        },
        memberTypes = {
          val xercesResults = xercesObjectListToSeq(xercesObject.getMemberTypes)(classOf[xxs.XSSimpleTypeDefinition])

          xercesResults map { xercesResult => convertXercesSimpleTypeDefinition(xercesResult) }
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

      cache += (xercesObject -> obj)
      obj
    }
  }

  def convertXercesComplexTypeDefinition(xercesObject: xxs.XSComplexTypeDefinition): XSComplexTypeDefinition = {
    cache.get(xercesObject) collect { case obj: XSComplexTypeDefinition => logCacheHit(xercesObject); obj } getOrElse {
      val obj = new XSComplexTypeDefinition(
        nameOption = Option(xercesObject.getName),
        targetNamespaceOption = Option(xercesObject.getNamespace),
        baseTypeOption = {
          val xercesResultOption = Option(xercesObject.getBaseType)

          if (xercesResultOption.isEmpty) None
          else if (xercesResultOption.get == xercesObject) None
          else {
            val result = convertXercesTypeDefinition(xercesResultOption.get)
            Option(result)
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

          xercesResults map { xercesResult => convertXercesAttributeUse(xercesResult) }
        },
        attributeWildcardOption = {
          val xercesResultOption = Option(xercesObject.getAttributeWildcard)

          if (xercesResultOption.isEmpty) None
          else {
            val result = convertXercesWildcard(xercesResultOption.get)
            Option(result)
          }
        },
        simpleTypeOption = {
          val xercesResultOption = Option(xercesObject.getSimpleType)

          if (xercesResultOption.isEmpty) None
          else {
            val result = convertXercesSimpleTypeDefinition(xercesResultOption.get)
            Option(result)
          }
        },
        particleOption = {
          val xercesResultOption = Option(xercesObject.getParticle)

          if (xercesResultOption.isEmpty) None
          else {
            val result = convertXercesParticle(xercesResultOption.get)
            Option(result)
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

      cache += (xercesObject -> obj)
      obj
    }
  }

  def convertXercesTerm(xercesObject: xxs.XSTerm): XSTerm = xercesObject match {
    case elementDecl: xxs.XSElementDeclaration => convertXercesElementDeclaration(elementDecl)
    case modelGroup: xxs.XSModelGroup => convertXercesModelGroup(modelGroup)
    case wildcard: xxs.XSWildcard => convertXercesWildcard(wildcard)
  }

  def convertXercesParticle(xercesObject: xxs.XSParticle): XSParticle = {
    cache.get(xercesObject) collect { case obj: XSParticle => logCacheHit(xercesObject); obj } getOrElse {
      val obj = new XSParticle(
        nameOption = Option(xercesObject.getName),
        targetNamespaceOption = Option(xercesObject.getNamespace),
        minOccursOption = Some(xercesObject.getMinOccurs),
        maxOccursOption = if (xercesObject.getMaxOccursUnbounded) None else Some(xercesObject.getMaxOccurs),
        maxOccursUnbounded = xercesObject.getMaxOccursUnbounded,
        term = {
          val xercesResult = xercesObject.getTerm

          convertXercesTerm(xercesResult)
        })

      cache += (xercesObject -> obj)
      obj
    }
  }

  def convertXercesAttributeUse(xercesObject: xxs.XSAttributeUse): XSAttributeUse = {
    cache.get(xercesObject) collect { case obj: XSAttributeUse => logCacheHit(xercesObject); obj } getOrElse {
      import XSAttributeUse._

      val obj = new XSAttributeUse(
        nameOption = Option(xercesObject.getName),
        targetNamespaceOption = Option(xercesObject.getNamespace),
        isRequired = xercesObject.getRequired,
        attributeDeclaration = {
          val xercesResult = xercesObject.getAttrDeclaration

          convertXercesAttributeDeclaration(xercesResult)
        },
        constraintInfo = new ConstraintInfo(
          constraintType = convertXercesConstraintInfo(xercesObject.getConstraintType),
          constraintValueOption = Option(xercesObject.getActualVC) map { v => new XSValue(v) }),
        annotations = immutable.IndexedSeq()) // TODO

      cache += (xercesObject -> obj)
      obj
    }
  }

  def convertXercesAttributeDeclaration(xercesObject: xxs.XSAttributeDeclaration): XSAttributeDeclaration = {
    cache.get(xercesObject) collect { case obj: XSAttributeDeclaration => logCacheHit(xercesObject); obj } getOrElse {
      import XSAttributeDeclaration._

      val obj = new XSAttributeDeclaration(
        nameOption = Option(xercesObject.getName),
        targetNamespaceOption = Option(xercesObject.getNamespace),
        typeDefinition = {
          convertXercesSimpleTypeDefinition(xercesObject.getTypeDefinition)
        },
        scope = convertXercesScope(xercesObject.getScope),
        constraintInfo = new ConstraintInfo(
          constraintType = convertXercesConstraintInfo(xercesObject.getConstraintType),
          constraintValueOption = Option(xercesObject.getActualVC) map { v => new XSValue(v) }),
        annotations = immutable.IndexedSeq()) // TODO

      cache += (xercesObject -> obj)
      obj
    }
  }

  def convertXercesModelGroup(xercesObject: xxs.XSModelGroup): XSModelGroup = {
    cache.get(xercesObject) collect { case obj: XSModelGroup => logCacheHit(xercesObject); obj } getOrElse {
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

          xercesResults map { xercesResult => convertXercesParticle(xercesResult) }
        },
        annotations = immutable.IndexedSeq()) // TODO

      cache += (xercesObject -> obj)
      obj
    }
  }

  def convertXercesWildcard(xercesObject: xxs.XSWildcard): XSWildcard = {
    cache.get(xercesObject) collect { case obj: XSWildcard => logCacheHit(xercesObject); obj } getOrElse {
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

      cache += (xercesObject -> obj)
      obj
    }
  }

  def convertXercesAnnotation(xercesObject: xxs.XSAnnotation): XSAnnotation = {
    null // TODO
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

  private def logCacheHit(xercesObject: xxs.XSObject): Unit = {
    println("Cache hit on %s (cache size: %d)".format(xercesObject, cache.size))
  }
}
