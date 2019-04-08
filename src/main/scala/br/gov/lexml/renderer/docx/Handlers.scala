package br.gov.lexml.renderer.docx

import br.gov.lexml.doc._
import scala.xml._
import com.typesafe.config._


class LexmlHandlerContext {
  def cocument() : LexmlDocument
  def projetoNorma() : Option[(ProjetoNorma]
  def hierarchicalStructure() : Option[HierarchicalStructure]
  
}

trait LexmlHandlerContextHolder {
  def context() : LexmlHandlerContext
}

class LexmlHandlers(contextHolder : LexmlHandlerContextHolder) {
  private object Handlers extends TemplateHandlersBuilder {
    handle("docElement"
  }
  
  
}