package eu.cdevreeze.yaidom
package xs

sealed trait Variety

object Variety {

  object VarietyAtomic extends Variety
  object VarietyList extends Variety
  object VarietyUnion extends Variety
}
