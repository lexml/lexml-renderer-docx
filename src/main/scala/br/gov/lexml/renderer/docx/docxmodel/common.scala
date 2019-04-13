package br.gov.lexml.renderer.docx.docxmodel

import scala.xml._

trait XmlComponent {
  def asXML : Elem
  implicit class RichOption[T](o : Option[T]) {
    def elem(f : T => NodeSeq) : NodeSeq = {
      o.map(f).getOrElse(NodeSeq.Empty)
    }
    def onSome(f : T => NodeSeq) : NodeSeq = elem(f)
    def optAttr : String = 
       o.map(_.toString).getOrElse(null)
  }
  implicit class BooleanElem[T](v : Boolean) {
    def onTrue(x : NodeSeq) : NodeSeq =
      if(v) { x } else { NodeSeq.Empty }  
    def onTrueOrNull(x : String)
      = if (v) { x } else { null }
  }
  def cond(c : Boolean)(e : => NodeSeq) : NodeSeq = {
    if(c) { e } else { NodeSeq.Empty }    
  }
  implicit class RichSeq[T](o : Seq[T]) {
    def eachElem(f : T => Elem) : NodeSeq =
        NodeSeq.fromSeq(o.map(f))
  }
}