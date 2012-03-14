package org.scalaide.api.model

trait Scopes { this: Symbols =>

  type Scope <: Iterable[Symbol]

  def newScope(): Scope
}
