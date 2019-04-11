package br.gov.lexml.renderer.docx.ver4

import cats._
import cats.implicits._
import cats.data._
import cats.instances.all._

import br.gov.lexml.renderer.docx.docxmodel.builders._
import br.gov.lexml.renderer.docx.docxmodel.builders.implicits._
import br.gov.lexml.doc._
import br.gov.lexml.renderer.docx.docxmodel._
import java.net.URI

trait RunRendererState {
  def rotuloStyle : Option[RPr]
}

trait ParRendererState {
  def getBase : Option[URI]
  def setBase(base : Option[URI]) : ParRendererState 
  def addRef(href : URI) : (ParRendererState,HrefData)
}

final case class HrefData(id : String, tooltip : Option[String] = None, anchor : Option[String] = None,
    rPr : Option[RPr] = None)

trait MainDocRendererState {  
  def omissisParStyle : Option[PPr]
  def addUnsupported(msg : String, elem : Any) : MainDocRendererState
  def nomeAgrupadorPredefStyle(tipo : TipoAgrupadorPredef) : Option[PPr]
  def rotuloAgrupadorPredefStyle(tipo : TipoAgrupadorPredef) : Option[PPr]  
}

trait Renderers extends RunBuilderOps[RunRendererState] with ParBuilderOps[ParRendererState] 
  with MainDocBuilderOps[MainDocRendererState] {
  
  type RunRenderer[A] = PB[A] 
  
  type ParRenderer[A] = MB[A]
  
  implicit val runRendererStateMergeable : Mergeable[RunRendererState] = ???
  
  implicit val parRunRendererStatesMergeable : Mergeable2[ParRendererState,RunRendererState] = ???
  
  implicit val parRendererStateMergeable : Mergeable[ParRendererState] = ???
  
  implicit val mainDocParRendererStatesMergeable : Mergeable2[MainDocRendererState,ParRendererState] = ???
      
       
  def rotulo(r : Rotulo) : RunRenderer[Unit] = 
    runM()(text(r.rotulo + " "))
    
  
  def omissis() : RunRenderer[Unit] = 
    runM()(ptab(alignment = PTA_Right,leader = Some(TL_Dot),
            relativeTo = PTB_Margin ))  
                
  def parOmissis(x : Omissis) : ParRenderer[Unit] = {
    for {
      style <- inspectMDState(_.omissisParStyle)
      _ <- parM(style)(omissis)
    } yield (())
  }            
  
  def agrupadorGenerico(ag : AgrupadorGenerico) : ParRenderer[Unit] = 
    modifyMDState(_.addUnsupported("render(AgrupadorGenerico): não suportado: ",ag))
    
  def agrupadorPredef(ag : AgrupadorPredef) : ParRenderer[Unit] = {      
      for {    
        nomeAgrupadorStyle <- inspectMDState(_.nomeAgrupadorPredefStyle(ag.tipoAgrupador))
        rotuloAgrupadorStyle <- inspectMDState(_.nomeAgrupadorPredefStyle(ag.tipoAgrupador))
        _ <- ag.nomeAgrupador.ifDef(x => parM(nomeAgrupadorStyle)(inlineSeq(x.inlineSeq)))
        _ <- ag.rotulo.ifDef(x => parM(rotuloAgrupadorStyle)(rotulo(x)))
        _ <- mapM_(ag.elems)(hierarchicalElement)
      } yield (())
    }
  
  def agrupador(ag : Agrupador) : ParRenderer[Unit] = ag match {
     case x : AgrupadorPredef => agrupadorPredef(x)
     case x : AgrupadorGenerico => agrupadorGenerico(x)
  }
  
  def hierarchicalElement(he : HierarchicalElement) : ParRenderer[Unit] = he match {
      case x : Agrupador => agrupador(x)
      case x : Artigo => artigo(x)
      case x : Omissis => parOmissis(x)    
    }
  
    
  def remissaoMultipla(r : RemissaoMultipla) : RunRenderer[Unit] = 
    for {
      oldBase <- inspectPState(_.getBase)
      val newBase = oldBase.map(_.resolve(r.base.uri)).orElse(Some(r.base.uri))
      _ <- modifyPState(_.setBase(newBase))
      _ <- inlineSeq(r.inlineSeq)
      _ <- modifyPState(_.setBase(oldBase))      
    } yield (())
    
  def remissao(r : Remissao) : RunRenderer[Unit] = for {
      base <- inspectPState(_.getBase) 
      val href = base.map(_.resolve(r.href.uri)).getOrElse(r.href.uri)
      hd <- modifyPStateV(_.addRef(href))
      _ <- hyperlink(id = Some(hd.id),anchor=hd.anchor,tooltip=hd.tooltip,rPr=hd.rPr)(inlineSeq(r.inlineSeq))
    } yield (())
  
  def artigo(x : Artigo) : ParRenderer[Unit] = ???
  
  def inlineSeq(il : InlineSeq) : RunRenderer[Unit] = ???
    
}