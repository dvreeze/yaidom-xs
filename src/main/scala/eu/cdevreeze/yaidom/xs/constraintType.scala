package eu.cdevreeze.yaidom
package xs

sealed trait ConstraintType

object ConstraintType {

  object ConstraintTypeNone extends ConstraintType
  object ConstraintTypeDefault extends ConstraintType
  object ConstraintTypeFixed extends ConstraintType
}
