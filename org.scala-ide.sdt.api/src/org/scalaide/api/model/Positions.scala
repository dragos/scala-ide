package org.scalaide.api.model

trait Positions {

  type Position = scala.tools.nsc.util.Position
  
  val NoPosition = scala.tools.nsc.util.NoPosition
}