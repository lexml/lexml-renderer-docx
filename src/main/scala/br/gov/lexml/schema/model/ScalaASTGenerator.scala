package br.gov.lexml.schema.model

import scala.meta._

class ScalaASTGenerator(schema : Schema) {
  
  def generate() : Tree = {
    q"""
      import br.gov.lexml.model.scala._
      
      
      """
  }
}