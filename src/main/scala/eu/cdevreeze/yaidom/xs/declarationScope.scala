package eu.cdevreeze.yaidom
package xs

sealed trait DeclarationScope

object DeclarationScope {

  object ScopeGlobal extends DeclarationScope
  object ScopeLocal extends DeclarationScope
  object ScopeAbsent extends DeclarationScope
}
