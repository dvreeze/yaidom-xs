package eu.cdevreeze.yaidom
package xs

sealed trait DerivationMethod

object DerivationMethod {

  object DerivationByExtension extends DerivationMethod
  object DerivationByRestriction extends DerivationMethod
  object DerivationBySubstitution extends DerivationMethod
  object DerivationUnion extends DerivationMethod
  object DerivationList extends DerivationMethod
}
