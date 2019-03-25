package br.gov.lexml.renderer.docx

import org.docx4j.openpackaging.packages.WordprocessingMLPackage
import org.docx4j.wml.ArrayListWml
import org.docx4j.wml.P
import org.docx4j.wml.R
import org.docx4j.wml.Text
import scala.util.matching.Regex


trait DocxRendererParams {
  def baseDocument : WordprocessingMLPackage
}

abstract sealed class DocxRendererError extends Product


object LexmlStyles {
  def ensureStylesExist(doc : WordprocessingMLPackage) : Unit =  { }
}

object DocxUtils {
  
  def extractTextToSeq(o : Object) : Seq[String] = {
    import scala.collection.JavaConverters._
    
    o match {
      case (p : P) => ??? // p.getContent.asScala.flatMap(extractTextToSeq _)
      case (r : R) => ??? // r.getContent.asScala.flatMap(extractTextToSeq _) 
      case (t : Text) => Seq(t.getValue())
      case _ => Seq() 
    }
    
  }
  def extractText(o : Object) : String =
    extractTextToSeq(o).mkString("","","")
}


final case class  DocxRenderErrorException(error : DocxRendererError) extends Exception

abstract sealed class DocxRendererState extends Product {
  def lexmlDocument : scala.xml.Elem
  def next(p : P) : (Seq[P],DocxRendererState)  
  val markPattern : Regex = "#LEXML#([^#]+)#(INICIO|FIM|AQUI)#".r
}

final case class InitialDocxRendererState(lexmlDocument : scala.xml.Elem) extends DocxRendererState { 
  def next(p : P) : (Seq[P],DocxRendererState) = {
    val text = DocxUtils.extractText(p)
    /*  text.toUpperCase() match {
      case markPattern("FORMULA_PROMULGACAO",pos) => renderedFormulaPromulgacao
      case markPattern("EPIGRAFE",pos) => renderedEpigrafe
      case markPattern("EMENTA",pos) => renderedEmenta
      case markPattern("PREAMBULO",pos) => renderedPreambulo
      case markPattern("ARTICULACAO",pos) => renderedArticulacao    
      case markPattern("LOCAL_DATA_FECHO",pos) => renderedLocalDataFecho
      case markPattern("ANEXOS",pos) => renderedAnexos
      case markPattern("JUSTIFICACAO",pos) => renderedJustificacao
      case markPattern("AUTOR_PROJETO",pos) => renderedAutorProjeto 
    } */
    ???
  }
}


class DocxRenderer(params : DocxRendererParams) {
  
  LexmlStyles.ensureStylesExist(params.baseDocument)
  
  def newDoc() = params.baseDocument.clone().asInstanceOf[WordprocessingMLPackage]
  
  def render(lexmlDocument : scala.xml.Elem) : Either[DocxRendererError,WordprocessingMLPackage] = {          
    try {
      val doc = newDoc()
      doRender(doc,lexmlDocument)
      Right(doc)
    } catch {
        case DocxRenderErrorException(err) => Left(err) 
    }    
  }
      
  def initialRendererState(lexmlDocument : scala.xml.Elem) : DocxRendererState = ???
  
  def doRender(doc : WordprocessingMLPackage,lexmlDocument : scala.xml.Elem) {
     val mainPart = doc.getMainDocumentPart
     mainPart.getJaxbElement.getBody.getContent
     val jaxbContents = mainPart.getContent
     import scala.collection.JavaConverters._
     val contents = IndexedSeq( jaxbContents.asScala : _*)
     jaxbContents.clear()
     var state : DocxRendererState = initialRendererState(lexmlDocument)     
     contents foreach { obj => 
       obj match {
         case par : P => 
           val (pl,nextState) = state.next(par)
           pl.foreach(jaxbContents.add(_))
           state = nextState
         case _ => jaxbContents.add(obj)
       } 
     }     
  }
  
  
}