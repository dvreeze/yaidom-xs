package eu.cdevreeze.yaidom
package xs

sealed trait NSConstraintType

object NSConstraintType {

  object NSConstraintAny extends NSConstraintType
  object NSConstraintList extends NSConstraintType
  object NSConstraintNot extends NSConstraintType
}
