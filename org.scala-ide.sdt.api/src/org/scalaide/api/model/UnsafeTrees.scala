package org.scalaide.api.model

import scala.tools.nsc.interactive.Global

trait UnsafeTrees {
  
  protected val compiler: Global
  
  type Tree = compiler.Tree
  
  val t: Tree
  
  t.symbol

}