package eu.cdevreeze.yaidom
package xs

sealed trait Ordered

object Ordered {

  object OrderedFalse extends Ordered
  object OrderedPartial extends Ordered
  object OrderedTotal extends Ordered
}
