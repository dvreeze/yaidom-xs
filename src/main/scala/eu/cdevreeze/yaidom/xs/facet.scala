package eu.cdevreeze.yaidom
package xs

sealed trait Facet

object Facet {

  object FacetEnumeration extends Facet
  object FacetFractionDigits extends Facet
  object FacetLength extends Facet
  object FacetMaxInclusive extends Facet
  object FacetMaxExclusive extends Facet
  object FacetMaxLength extends Facet
  object FacetMinInclusive extends Facet
  object FacetMinExclusive extends Facet
  object FacetMinLength extends Facet
  object FacetNone extends Facet
  object FacetPattern extends Facet
  object FacetTotalDigits extends Facet
  object FacetWhitespace extends Facet
}
