package org.scalaide.api

trait Positions {

  type Position = scala.tools.nsc.util.RangePosition
  
  val NoPosition = scala.tools.nsc.util.NoPosition
}