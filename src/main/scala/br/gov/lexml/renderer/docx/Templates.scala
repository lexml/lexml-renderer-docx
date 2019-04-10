package br.gov.lexml.renderer.docx

import scala.xml._

trait TemplateHandler {  
  val label : String
  def handle(attrs : Map[String,String]) : Seq[Node]  
}

final case class TemplateHandlers(prefix : String,handlers : TemplateHandler*) {  
  val handlerMap : Map[String,TemplateHandler] = 
    handlers.map(h => (h.label,h)).toMap
  def apply(t : Template) = process(t.ns)
  private def process(ns : Seq[Node]) : Seq[Node] = NodeSeq.fromSeq(ns.flatMap(processNode))
  private def processNode(n : Node) : Seq[Node] = n match {
    case e : Elem if e.prefix == prefix =>
      handlerMap.get(e.label) match {
        case None => sys.error(s"No handler found for ${e.label} at ${n.toString}")
        case Some(handler) => handler.handle(e.attributes.asAttrMap)
      }    
    case e : Elem => e.copy(child = process(e.child))    
    case x => x
  }     
}

trait TemplateHandlersBuilder {    
  private var _handlers = Seq.newBuilder[TemplateHandler]
  protected class handle(elemLabel : String) {
    def `with`(f : Map[String,String] => Seq[Node]) {
     // _handlers +=  new TemplateHandler {  val label = elemLabel ; override def handle = f }
    }
  }
  def handlersWithPrefix(prefix : String) = TemplateHandlers(prefix,_handlers.result() : _*)
}

final case class Template(ns : Seq[Node] = Seq())

object Template { 
  def apply(txt : String) : Template = 
    Template(
        XML.loadString("<template>" + txt + "</template>").child)
}
