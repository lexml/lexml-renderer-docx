package br.gov.lexml.renderer.docx.docxmodel

import scala.xml._

trait XmlComponent:
  def asXML : Elem
  extension[T](o : Option[T])
    def elem(f : T => NodeSeq) : NodeSeq = o.map(f).getOrElse(NodeSeq.Empty)
    def onSome(f : T => NodeSeq) : NodeSeq = elem(f)
    def optAttr : String = o.map(_.toString).orNull

  extension[T](v : Boolean)
    def onTrue(x : NodeSeq) : NodeSeq = if v then x else NodeSeq.Empty
    def onTrueOrNull(x : String): String = if v then x else null

  def cond(c : Boolean)(e : => NodeSeq) : NodeSeq =
    if c then e else NodeSeq.Empty
  extension[T](o : Seq[T])
    def eachElem(f : T => Elem) : NodeSeq =
        NodeSeq.fromSeq(o.map(f))

