package org.scalaide.api.model

import scala.tools.nsc.interactive.Compatibility

trait CompilerTypes extends Compatibility.Positions
  with Compatibility.PresentationSymbols
  with Compatibility.PresentationTypes
  with Compatibility.Names
  with Compatibility.StandardNames
  with Compatibility.Constants
  with Compatibility.Scopes // used by Type extractors
  with Compatibility.PresentationStandardDefinitions
  with Compatibility.TreePrinters
  with Compatibility.Trees
