package org.scalaide.api.model

import scala.reflect.api.PresentationSymbols
import scala.reflect.api.PresentationTypes
import scala.reflect.api.PresentationStandardDefinitions

trait CompilerTypes extends reflect.api.Positions
  with PresentationSymbols
  with PresentationTypes
  with reflect.api.Names 
  with reflect.api.StandardNames
  with reflect.api.Constants
  with reflect.api.Scopes // used by Type extractors
  with PresentationStandardDefinitions