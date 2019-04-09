package br.gov.lexml.renderer.docx

import scala.util.matching.Regex
import br.gov.lexml.renderer._
import br.gov.lexml.schema.scala.{data => X}
import br.gov.lexml.{doc => M}
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import br.gov.lexml.doc.xml.XmlConverter
import java.io.ByteArrayInputStream
import scala.xml._
import java.util.zip.ZipInputStream
import java.util.zip.ZipEntry
import org.apache.commons.io.IOUtils
import java.util.zip.ZipOutputStream
import java.io.ByteArrayOutputStream
import java.io.StringWriter
import org.docx4j.openpackaging.packages.WordprocessingMLPackage
import br.gov.lexml.doc.GenHtmlInlineElement
import java.net.URI

trait REMOVEME

/*
class DocxBuilder(config : Config) {
  
  def id() = { val x = nextId ; nextId = nextId + 1 ; x }
  
  def par(styleNameOrNull : String = null)(f : => Any) {
  }
  
  def text(txt : String) {
  }
  
  private def withS(g : Style => Style,f : => Any) {
  }
  
  def style(name : String)(f : => Any) = withS(_.RunStyle(name),f)    
  
  def bold(f : => Any) = withS(_.Bold,f)
      
  def italics(f : => Any) = withS(_.Italics,f)
  
  def caps(f : => Any) = withS(_.Caps,f)
  
  def smallCaps(f : => Any) = withS(_.SmallCaps,f)

  def sub(f : => Any) = withS(_.Sub,f)
  
  def sup(f : => Any) = withS(_.Super,f)
  
  def build() = {
  }
  
  def hyperlink(href : String,linkStyleOrNull : String = null)(f : => Any) {

  }
  
  def indent(len : Long)(f : => Any) {

  }
}

object ConfigDispositivo {
  
}

class ConfigDispositivo(tipo : String,config : Config) {
  val template = XML.loadString(config.getString("template"))
             
}

class TemplateConfig(config : Config) {
  val template = XML.loadString(config.getString("template"))  
}

class DocxRenderingConfig(config : Config) {
  val indentAlteracao = config.getInt("indentacaoAlteracao")
  val templates = { 
    val temps = config.getConfig("templates")
    templateKeys.map(k => (k,new TemplateConfig(config.getConfig(k)))).toMap    
  }
  val templateKeys = Set(
      "formulaPromulgacao",
      "epigrafe",
      "ementa",
      "preambulo",      
      "livro",
      "parte",
      "capitulo",
      "secao",
      "subsecao",
      "caput",
      "artigo",
      "caput",
      "paragrafo",
      "inciso",
      "alinea",
      "item",
      "pena",
      "ateracao",
      "remissao")
  
  
  val STYLE_NORMAL = config.getString("")
  val STYLE_FORMULA_PROMULGACAO = "FormulaPromulgacaoP"
  val STYLE_EPIGRAFE = "EpigrafeP"
  val STYLE_EMENTA = "EmentaP"
  val STYLE_PREAMBULO = "PreambuloP"
  val STYLE_ARTIGO_ROTULO="ArtigoRotuloP"
  val STYLE_LIVRO_ROTULO = "STYLE_LIVRO_ROTULO"
  val STYLE_LIVRO_NOME = "STYLE_LIVRO_NOME"
  val STYLE_PARTE_ROTULO = "STYLE_PARTE_ROTULO"
  val STYLE_PARTE_NOME = "STYLE_PARTE_NOME"
  val STYLE_TITULO_ROTULO = "STYLE_TITULO_ROTULO"
  val STYLE_TITULO_NOME = "STYLE_TITULO_NOME"
  val STYLE_CAPITULO_ROTULO = "STYLE_CAPITULO_ROTULO"
  val STYLE_CAPITULO_NOME = "STYLE_CAPITULO_NOME            "
  val STYLE_SECAO_ROTULO = "STYLE_SECAO_ROTULO"
  val STYLE_SECAO_NOME = "STYLE_SECAO_NOME"
  val STYLE_SUBSECAO_ROTULO = "STYLE_SUBSECAO_ROTULO"
  val STYLE_SUBSECAO_NOME = "STYLE_SUBSECAO_NOME"
  val STYLE_CAPUT_PAR = "STYLE_CAPUT_PAR"
  val STYLE_PARAGRAFO_PAR = "STYLE_PARAGRAFO_PAR"
  val STYLE_INCISO_PAR = "STYLE_INCISO_PAR"
  val STYLE_ALINEA_PAR = "STYLE_ALINEA_PAR"
  val STYLE_ITEM_PAR = "STYLE_ITEM_PAR"
  val STYLE_PENA_PAR = "STYLE_PENA_PAR"
  val STYLE_PARAGRAFO_ROTULO = "STYLE_PARAGRAFO_ROTULO"
  val STYLE_INCISO_ROTULO = "STYLE_INCISO_ROTULO"
  val STYLE_ALINEA_ROTULO = "STYLE_ALINEA_ROTULO"
  val STYLE_ITEM_ROTULO = "STYLE_ITEM_ROTULO"
  val STYLE_PENA_ROTULO = "STYLE_PENA_ROTULO"
  val STYLE_OMISSIS = "STYLE_OMISSIS"
  val STYLE_REMISSAO_HYPERLINK = "STYLE_REMISSAO_HYPERLINK"
  val STYLE_ANCHOR_HYPERLINK = "STYLE_ANCHOR_HYPERLINK"
  val STYLE_SPAN_HYPERLINK = "STYLE_SPAN_HYPERLINK"
  val INDENT_ALTERACAO = 562L
}

object DocxRendering {
  val mimeType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  val STYLE_NORMAL = "Normal"
  val STYLE_FORMULA_PROMULGACAO = "FormulaPromulgacaoP"
  val STYLE_EPIGRAFE = "EpigrafeP"
  val STYLE_EMENTA = "EmentaP"
  val STYLE_PREAMBULO = "PreambuloP"
  val STYLE_ARTIGO_ROTULO="ArtigoRotuloP"
  val STYLE_LIVRO_ROTULO = "STYLE_LIVRO_ROTULO"
  val STYLE_LIVRO_NOME = "STYLE_LIVRO_NOME"
  val STYLE_PARTE_ROTULO = "STYLE_PARTE_ROTULO"
  val STYLE_PARTE_NOME = "STYLE_PARTE_NOME"
  val STYLE_TITULO_ROTULO = "STYLE_TITULO_ROTULO"
  val STYLE_TITULO_NOME = "STYLE_TITULO_NOME"
  val STYLE_CAPITULO_ROTULO = "STYLE_CAPITULO_ROTULO"
  val STYLE_CAPITULO_NOME = "STYLE_CAPITULO_NOME            "
  val STYLE_SECAO_ROTULO = "STYLE_SECAO_ROTULO"
  val STYLE_SECAO_NOME = "STYLE_SECAO_NOME"
  val STYLE_SUBSECAO_ROTULO = "STYLE_SUBSECAO_ROTULO"
  val STYLE_SUBSECAO_NOME = "STYLE_SUBSECAO_NOME"
  val STYLE_CAPUT_PAR = "STYLE_CAPUT_PAR"
  val STYLE_PARAGRAFO_PAR = "STYLE_PARAGRAFO_PAR"
  val STYLE_INCISO_PAR = "STYLE_INCISO_PAR"
  val STYLE_ALINEA_PAR = "STYLE_ALINEA_PAR"
  val STYLE_ITEM_PAR = "STYLE_ITEM_PAR"
  val STYLE_PENA_PAR = "STYLE_PENA_PAR"
  val STYLE_PARAGRAFO_ROTULO = "STYLE_PARAGRAFO_ROTULO"
  val STYLE_INCISO_ROTULO = "STYLE_INCISO_ROTULO"
  val STYLE_ALINEA_ROTULO = "STYLE_ALINEA_ROTULO"
  val STYLE_ITEM_ROTULO = "STYLE_ITEM_ROTULO"
  val STYLE_PENA_ROTULO = "STYLE_PENA_ROTULO"
  val STYLE_OMISSIS = "STYLE_OMISSIS"
  val STYLE_REMISSAO_HYPERLINK = "STYLE_REMISSAO_HYPERLINK"
  val STYLE_ANCHOR_HYPERLINK = "STYLE_ANCHOR_HYPERLINK"
  val STYLE_SPAN_HYPERLINK = "STYLE_SPAN_HYPERLINK"
  val INDENT_ALTERACAO = 562L      
}


class DocxRendering(doc : M.LexmlDocument, config : Config) {
  import DocxRendering._
  private val d = new DocxBuilder(config)  
  var remMultiplaHref : Option[URI] = None
  var emAlteracao : Boolean = false
  
  import d._ //.{par=> dPar,_}
  /*
  def par(styleOrNull : String = null)(f : => Any) {
    dPar(styleOrNull) { 
      if(emAlteracao) {
        indent(INDENT_ALTERACAO) {
          f
        }
      } else {
        f
      }
    }
  }*/
  
  def render() : RenderedDocument = {    
    doc.contents match {
      case pj : M.ProjetoNorma => projetoNorma(pj)
      case n : M. Norma => norma(n)
      case x => sys.error("FIXME: unsupported doc. content: " + x)
    }
    val data = d.build()
    ByteArrayDocument(data,DocxRendering.mimeType)
  }   
  
  private def projetoNorma(pj : M.ProjetoNorma) {
    norma(pj.norma)  
  }
  
  private def norma(n : M.Norma) {
    hierarchicalStructure(n.contents)
  }
  
  private def hierarchicalStructure(hs : M.HierarchicalStructure) {
    hs.formulaPromulgacao.foreach(formulaPromulgacao)
    hs.epigrafe.foreach(epigrafe)
    hs.ementa.foreach(ementa)
    hs.preambulo.foreach(preambulo)
    hierarchicalElements(hs.articulacao.elems)
  }   
       
  def inlineElement(ie : M.InlineElement) = ie match {
    case x : M.Alteracao => alteracao(x)
    case x : M.Anchor => anchor(x)
    case x : M.EmLinha => emLinha(x)
    case x : M.Formula => formula(x)
    case x : GenHtmlInlineElement => genHtmlInlineElement(x)
    case x : M.Img => img(x)
    case x : M.Marcador => marcador(x)
    case x : M.NotaReferenciada => notaReferenciada(x)
    case x : M.Remissao => remissao(x)
    case x : M.RemissaoMultipla => remissaoMultipla(x)
    case x : M.Span => span(x)
  }
  
  def inlineSeq(inl : M.InlineSeq) {    
      inl.mixedElems.elems.collect {
        case Right(txt) => text(txt) 
        case Left(v) => inlineElement(v) 
      }    
  }
  
  def formulaPromulgacao(f : M.FormulaPromulgacao) {
    par(STYLE_FORMULA_PROMULGACAO) {
      inlineSeq(f.inlineSeq)
    }
  }
  
  def epigrafe(e : M.Epigrafe) {
    par(STYLE_EPIGRAFE) {
      inlineSeq(e.inlineSeq)
    }
  }
  
  def ementa(e : M.Ementa) {
    par(STYLE_EMENTA) {
      inlineSeq(e.inlineSeq)
    }
  }
  
  def preambulo(p : M.Preambulo) {
    p.inlineSeqs.foreach{ x => 
      par(STYLE_PREAMBULO) {
        inlineSeq(x.inlineSeq)       
      }
    }
  }
  
  def hierarchicalElements(els : Seq[M.HierarchicalElement]) = els.foreach(hierarchicalElement)
  
  def hierarchicalElement(he : M.HierarchicalElement) = he match {
    case x : M.Agrupador => agrupador(x)
    case x : M.Artigo => artigo(x)
    case x : M.Omissis => omissis(x)    
  }
  
  def agrupador(ag : M.Agrupador) = ag match {
    case x : M.AgrupadorGenerico => agrupadorGenerico(x)
    case x : M.AgrupadorPredef => agrupadorPredef(x)    
  }
  
  def artigo(art : M.Artigo) {
    val rotulo = art.rotulo.get.rotulo
    val titulo = art.titulo
    val containers = art.containers
    par(STYLE_ARTIGO_ROTULO) { text(rotulo) }
    art.alteracao.foreach(alteracao)
    containers.foreach(lxContainer)    
  }    
  
  def dispositivo(x : M.Dispositivo) = x match {
    case y : M.Artigo => artigo(y)
    case y : M.DispositivoPredefNA => dispositivoPredefNA(y)
    case y : M.DispositivoGenerico => dispositivoGenerico(y)
  }
  
  def lxContainer(x : M.LXContainer) = x match {
    case y : M.Dispositivo => dispositivo(y)                
    case y : M.Omissis => omissis(y)    
  }
  
  def genHtmlInlineElement(x : GenHtmlInlineElement) {    
    x.tipoHtmlInlineElement match {
      case M.TGHIE_B => bold { inlineSeq(x.inlineSeq) }
      case M.TGHIE_I => italics { inlineSeq(x.inlineSeq) }
      case M.TGHIE_Sub => sub { inlineSeq(x.inlineSeq) }
      case M.TGHIE_Sup => sup { inlineSeq(x.inlineSeq) }
      case M.TGHIE_Ins => sys.error("Ins unsupported in DOCX renderer!")
      case M.TGHIE_Del => sys.error("Del unsupported in DOCX renderer!")
      case M.TGHIE_Dfn => sys.error("Dfn unsupported in DOCX renderer!")
    }    
  }
  
  val agrupadorRotuloParStyles = Map[M.TipoAgrupadorPredef,String](
      M.TAP_Livro -> STYLE_LIVRO_ROTULO,
      M.TAP_Parte -> STYLE_PARTE_ROTULO,
      M.TAP_Titulo -> STYLE_TITULO_ROTULO,
      M.TAP_Capitulo -> STYLE_CAPITULO_ROTULO,            
      M.TAP_Secao -> STYLE_SECAO_ROTULO,
      M.TAP_Subsecao -> STYLE_SUBSECAO_ROTULO
      )
      
  val agrupadorNomeParStyles = Map[M.TipoAgrupadorPredef,String](
      M.TAP_Livro -> STYLE_LIVRO_NOME,
      M.TAP_Parte -> STYLE_PARTE_NOME,
      M.TAP_Titulo -> STYLE_TITULO_NOME,
      M.TAP_Capitulo -> STYLE_CAPITULO_NOME,            
      M.TAP_Secao -> STYLE_SECAO_NOME,
      M.TAP_Subsecao -> STYLE_SUBSECAO_NOME
      )
  
  def agrupadorPredef(x : M.AgrupadorPredef) {
    val elems = x.elems
    val rotulo = x.rotulo.get.rotulo
    val nomeAgrupador = x.nomeAgrupador.map(_.inlineSeq)
    val rotStyle = agrupadorRotuloParStyles(x.tipoAgrupador)
    val nomeStyle = agrupadorNomeParStyles(x.tipoAgrupador)
    par(rotStyle) { text(rotulo) }
    nomeAgrupador.foreach{ x => par(nomeStyle) { inlineSeq(x) } }
    elems.foreach(hierarchicalElement)
  }
  
  val dispositivoPredefParStyles = Map[M.TipoDispositivoPredef,String](
      M.TDP_Caput -> STYLE_CAPUT_PAR,
      M.TDP_Paragrafo -> STYLE_PARAGRAFO_PAR,
      M.TDP_Inciso -> STYLE_INCISO_PAR,
      M.TDP_Alinea -> STYLE_ALINEA_PAR,
      M.TDP_Item -> STYLE_ITEM_PAR,
      M.TDP_Pena -> STYLE_PENA_ROTULO
      )
      
  val dispositivoPredefRotuloStyles = Map[M.TipoDispositivoPredef,String](      
      M.TDP_Paragrafo -> STYLE_PARAGRAFO_ROTULO,
      M.TDP_Inciso -> STYLE_INCISO_ROTULO,
      M.TDP_Alinea -> STYLE_ALINEA_ROTULO,
      M.TDP_Item -> STYLE_ITEM_ROTULO,
      M.TDP_Pena -> STYLE_PENA_ROTULO
      )
  
  def dispositivoPredefNA(x : M.DispositivoPredefNA) {
    val rotuloStyle = dispositivoPredefRotuloStyles.get(x.tipoDisp)
    val parStyle = dispositivoPredefParStyles(x.tipoDisp)    
    val conteudo = x.conteudo 
    val rotulo = x.rotulo.map(_.rotulo)        
    val titulo = x.titulo.map(_.inlineSeq)    
    val alt = x.alteracao     
    val containers = x.containers
    
    val (par1,parRest) =
      conteudo match {
        case None => (None,Seq())
        case Some(M.TextoDispositivo(pars)) => (pars.headOption.map(_.inlineSeq),pars.tail.map(_.inlineSeq))
        case Some(M.OmissisSimples) => (Some(M.OmissisSimples),Seq())        
      }
    par1.foreach { p =>      
      par(parStyle) {
        rotulo.foreach { r =>
          style(rotuloStyle.get) {
            text(r)
          }
        }
        p match {
          case x : M.InlineSeq => inlineSeq(x)
          case M.OmissisSimples => omissisSimples()
        }
      }                 
    }
    parRest.foreach { p =>      
      par(parStyle) { inlineSeq(p) }                 
    } 
    alt.foreach(alteracao)
    containers.foreach(lxContainer)
  }
      
  
  def omissisSimples() {
    text(".....")
  }
  
  def omissis(o : M.Omissis) {
    par(STYLE_OMISSIS) { text("....") }
  }
  
  def alteracao(x : M.Alteracao) {
    indent(INDENT_ALTERACAO) {    
      x.mixedElems.elems.collect {
        case Right(x) => par() { text(x) }
        case Left(x) => alteracaoElement(x)
      }
    }    
  }
  
  def alteracaoElement(x : M.AlteracaoElement) = x match {
    case y : M.BlockElement => blockElement(y)
    case y : M.Container => container(y)   
    case y : M.Dispositivo => dispositivo(y)
    case y : M.Agrupador => agrupador(y)
    case y : M.Ementa => ementa(y)
    case y : M.FormulaPromulgacao => formulaPromulgacao(y)
    case y : M.Epigrafe => epigrafe(y)
    case y : M.Preambulo => preambulo(y)
    case y : M.Omissis => omissis(y)
  }
  
  def blockElement(x : M.BlockElement) = x match {
    case y : M.ConteudoExterno => conteudoExterno(y)
    case y : M.Bloco => bloco(y)
    case y : M.HTMLBlock => htmlBlock(y)
  }
  
  def container(x : M.Container) = x match {
    case y : M.Div => div(y)
    case y : M.Agrupamento => agrupamento(y)
  }
  
  
  def htmlBlock(x : M.HTMLBlock) = x match {
    case y : M.Paragraph => paragraph(y)
    case y : M.HTMLList => htmlList(y)
    case y : M.Table => table(y)   
  }
      
  def anchor(x : M.Anchor) {
    hyperlink(x.href.uri.toString,STYLE_ANCHOR_HYPERLINK) {
      inlineSeq(x.inlineSeq)
    }
  }   
  
  def remissao(x : M.Remissao) {
    val href = remMultiplaHref.map(h => h.resolve(x.href.uri)).getOrElse(x.href.uri)
    hyperlink(href.toString,STYLE_REMISSAO_HYPERLINK) {
      inlineSeq(x.inlineSeq)
    }
  }
  
  def remissaoMultipla(x : M.RemissaoMultipla) {
    val old = remMultiplaHref
    remMultiplaHref.map(h => h.resolve(x.base.uri)).getOrElse(x.base.uri)
    inlineSeq(x.inlineSeq)
    remMultiplaHref = old
  }
  
  def span(x : M.Span) {
    val href = remMultiplaHref.map(h => h.resolve(x.href.uri)).getOrElse(x.href.uri)
    hyperlink(href.toString,STYLE_SPAN_HYPERLINK) {
      inlineSeq(x.inlineSeq)
    }
  }
    
  def unsupported(x : Any) = {
    sys.error(s"${x.getClass.getName} unsupported in DOCX renderer!")
  }
  
  def emLinha(x : M.EmLinha) = unsupported(x)    
  def formula(x : M.Formula) = unsupported(x)    
  def img(x : M.Img) = unsupported(x)    
  def marcador(x : M.Marcador) = unsupported(x)
  def notaReferenciada(x : M.NotaReferenciada) = unsupported(x)
  def dispositivoGenerico(x : M.DispositivoGenerico) = unsupported(x)
  def agrupadorGenerico(x : M.AgrupadorGenerico) = unsupported(x)
  def div(x : M.Div) = unsupported(x)  
  def agrupamento(x : M.Agrupamento) = unsupported(x)    
  def conteudoExterno(x : M.ConteudoExterno) = unsupported(x)  
  def bloco(x : M.Bloco) = unsupported(x)  
  def paragraph(x : M.Paragraph) = unsupported(x)  
  def htmlList(x : M.HTMLList) = unsupported(x)  
  def table(x : M.Table) = unsupported(x)
  
}

class DocxRenderer extends Renderer {
  override def render(doc1 : X.LexML,config : Config = ConfigFactory.empty()) : RenderedDocument = {
    println("Rendering: " + doc1)
    val doc = XmlConverter.scalaxbToModel(doc1)
    println("As model object: " + doc)
    new DocxRendering(doc,config).render()       
  }
}*/