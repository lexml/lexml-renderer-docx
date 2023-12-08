package br.gov.lexml.renderer.docx.renderers

import cats.implicits.*
import cats.data.*
import br.gov.lexml.renderer.docx.docxmodel.builders.*
import br.gov.lexml.renderer.docx.docxmodel.builders.implicits.*
import br.gov.lexml.doc.*
import br.gov.lexml.renderer.docx.docxmodel.{Table as DocxTable, *}

import java.net.URI
import org.apache.commons.io.IOUtils
import org.slf4j.*
import br.gov.lexml.doc.xml.XmlConverter.SomeLexmlDocument
import br.gov.lexml.renderer.docx.LexmlToDocxConfig

import scala.language.existentials
import scala.xml.Elem


trait ParRendererState[T <: ParRendererState[T]]:
  def getBase : Option[URI]
  def setBase(base : Option[URI]) : T 
  def addRef(href : URI) : (T,HrefData)
  def getHyperlinkRPr : Option[RPr]
  def addUnsupported(msg : String, elem : Any) : T
  def currentRPrStyle : Option[RPr]
  def setCurrentRPrStyle(rPr : Option[RPr]) : T
  def endnoteId(origId : String) : String
  def skipHyperlink : Boolean

trait RunRendererState[T <: RunRendererState[T]]:
  def currentRPrStyle : Option[RPr]

trait MainDocRendererState[T <: MainDocRendererState[T]]:
  def omissisParStyle : Option[PPr]
  def addUnsupported(msg : String, elem : Any) : T
  def nomeAgrupadorPredefPPrStyle(tipo : TAP) : Option[PPr]
  def rotuloAgrupadorPredefPPrStyle(tipo : TAP) : Option[PPr]
  def nomeAgrupadorPredefRPrStyle(tipo : TAP) : Option[RPr]
  def nomeAgrupadorPredefIniciaisMaiusc(tipo : TAP) : Boolean
  def rotuloAgrupadorPredefRPrStyle(tipo : TAP) : Option[RPr]
  def epigrafeParStyle : Option[PPr]
  def epigrafeCharStyle : Option[RPr]  
  def ementaParStyle : Option[PPr]
  def ementaCharStyle : Option[RPr]
  def preambuloParStyle : Option[PPr]
  def preambuloCharStyle : Option[RPr]
  def formulaPromulgacaoParStyle : Option[PPr]
  def formulaPromulgacaoCharStyle : Option[RPr]
  def textoAlteracaoRPrStyle : Option[RPr]
  def setBase(baseUri : Option[URI]) : T
  def indentAlteracao : Ind           
  def rotuloStyleRPrForDispositivo(t : TipoDispositivo) : Option[RPr]
  def contentStyleRPrForDispositivo(t : TipoDispositivo) : Option[RPr]
  def contentStylePPrForDispositivo(t : TipoDispositivo) : Option[PPr]
  def tituloStylePPrForDispositivo(t : TipoDispositivo) : Option[PPr]
  def tituloStyleRPrForDispositivo(t : TipoDispositivo) : Option[RPr]
  def localDataFechoStylePPr : Option[PPr]
  def localDataFechoStyleRPr : Option[RPr]
  def assinaturaTextoStylePPr : Option[PPr]
  def assinaturaTextoStyleRPr : Option[RPr]  
  def notaReferenciadaRPrStyle : Option[RPr]
end MainDocRendererState

type RenderElement = RE
enum RE(superClasses : RE*):
  def apply[T](m: Map[RenderElement, T]): Option[T] =
    get(m).headOption.map(_._2)

  final def get[T](m: Map[RenderElement, T]): IndexedSeq[(Int, T)] =
    m.get(this).map(x => (0, x)).to(IndexedSeq) ++
      superClasses.to(IndexedSeq).flatMap(_.get(m)).map(x => (x._1 + 1, x._2)).sortBy(_._1)

  case Any extends RE()

  case ParteInicial extends RE(Any)
  case NomesAgrupador extends RE(Any)
  case RotulosAgrupador extends RE(Any)
  case Remissao extends RE(Any)
  case Omissis extends RE(Any)
  case NomeAgrupador(t: TAP) extends RE(NomesAgrupador)
  case RotuloAgrupador(t: TAP) extends RE(RotulosAgrupador)
  case Epigrafe extends RE(ParteInicial)
  case Ementa extends RE(ParteInicial)
  case Preambulo extends RE(ParteInicial)
  case FormulaPromulgacao extends RE(RE.ParteInicial)
  case RotulosDispositivo extends RE(Any)
  case RotuloDispositivo(t: TipoDispositivo) extends RE(RotulosDispositivo)
  case ConteudosDispositivo extends RE(Any)
  case ConteudoDispositivo(t: TipoDispositivo) extends RE(ConteudosDispositivo)
  case TitulosDispositivo extends RE(Any)
  case TituloDispositivo(t: TipoDispositivo) extends RE(RE.TitulosDispositivo)
  case TextoAlteracao extends RE(Any)
  case AbrevArtigo extends RE(RotulosDispositivo)
  case LocalDataFecho extends RE(Any)
  case AssinaturaTexto extends RE(Any)
  case NotaReferenciada extends RE(Any)

final case class Constants(   
   hrefIdPrefix : String = "HrefId",
   pprStyles : Map[RenderElement,PPr] = Map(),
   rprStyles : Map[RenderElement,RPr] = Map(),   
   indentAlteracao : Ind = Ind(),
   spacingAlteracao : Spacing = Spacing(),
   iniciaisMaiusculas : Set[RenderElement] = Set(),
   expressoesEmBold : Set[String] = Set(),
   skipHyperlink : Boolean = false
)

object Constants:
  lazy val default: Constants = Constants(
    pprStyles = Map(
       RE.Ementa -> DefaultStyles.pprEmenta,
       RE.Epigrafe -> DefaultStyles.pprEpigrafe,
       RE.Preambulo -> DefaultStyles.pprPreambulo,
       RE.Omissis -> DefaultStyles.pprOmissis,
       RE.NomesAgrupador -> DefaultStyles.pprNomeAgrupador,
       RE.RotulosAgrupador -> DefaultStyles.pprRotuloAgrupador,
       RE.Any -> DefaultStyles.pprAny,
       RE.ConteudosDispositivo -> DefaultStyles.pprConteudoDispositivo,
       RE.TitulosDispositivo -> DefaultStyles.pprTituloDispositivo,
       RE.LocalDataFecho -> DefaultStyles.pprLocalDataFecho,
       RE.AssinaturaTexto -> DefaultStyles.pprAssinaturaTexto,
       RE.FormulaPromulgacao -> DefaultStyles.pprFormulaPromulgacao
   ),
   rprStyles = Map(
       //RE.TextoAlteracao -> RPr(),
       //RE.Remissao -> RPr()
       RE.NomesAgrupador -> DefaultStyles.rprNomeAgrupador,
       RE.NomeAgrupador(TAP.Secao) -> DefaultStyles.rprSecaoSubsecao,
       RE.NomeAgrupador(TAP.Subsecao) -> DefaultStyles.rprSecaoSubsecao,
       RE.RotulosAgrupador -> DefaultStyles.rprRotuloAgrupador,
       RE.RotuloAgrupador(TAP.Secao) -> DefaultStyles.rprRotuloSecaoSubsecao,
       RE.RotuloAgrupador(TAP.Subsecao) -> DefaultStyles.rprRotuloSecaoSubsecao,
       RE.Ementa -> DefaultStyles.rprEmenta,
       RE.Epigrafe -> DefaultStyles.rprEpigrafe,
       RE.Preambulo -> DefaultStyles.rprPreambulo,
       RE.Remissao -> DefaultStyles.rprLinkRemissao,
       RE.TituloDispositivo(TDP.Artigo) -> DefaultStyles.rprTituloArtigo,
       RE.LocalDataFecho -> DefaultStyles.rprLocalDataFecho,
       RE.AssinaturaTexto -> DefaultStyles.rprAssinaturaTexto,
       RE.FormulaPromulgacao -> DefaultStyles.rprFormulaPromulgacao,
       RE.NotaReferenciada -> DefaultStyles.rprNotaReferenciada
       ),                        
   indentAlteracao = DefaultStyles.indentAlteracao1,
   spacingAlteracao = DefaultStyles.spacingAlteracao1,
   iniciaisMaiusculas = Set(
       RE.NomeAgrupador(TAP.Secao),
       RE.NomeAgrupador(TAP.Subsecao)
       ),
   expressoesEmBold = Set[String](
      "(?i)caput",
      "(?i)in fine", "(?i)quorum",
      "(?i)libor", "(?i)price", "(?i)front-end fee", 
      "(?i)transaction fee", 
      "(?i)variable spread loan", 
      "(?i)spread"       
      ).map(x => "\\b" + x + "\\b") ++
      Set("^O PRESIDENTE DA REP[UÚ]BLICA",
          "^O PRESIDENTE DO CONGRESSO",
          "^O Presidente da República(?= decreta:)",
          "^DECRETA: *$",
          "^Pre[âa]mbulo$")         
  )
end Constants

final case class RendererState(
    base : Option[URI] = None,
    hrefNext : Int = 1,    
    hrefToHrefData : Map[URI,HrefData] = Map(),
    unsupportedCases : Seq[(String,Any)] = Seq(),
    currentRPrStyle : Option[RPr] = None,   
    endnoteRefs : Set[String] = Set(),
    constants : Constants = Constants(),
    endnoteIdMap : Map[String,String] = Map()
    ) extends ParRendererState[RendererState] 
        with MainDocRendererState[RendererState] 
        with RunRendererState[RendererState]:
  override def getBase: Option[URI] = base
  override def setBase(b : Option[URI]): RendererState = copy(base = b)
  override def addRef(href : URI): (RendererState, HrefData) =
    hrefToHrefData.get(href) match {
      case Some(data) => (this,data)
      case None =>
        val idNum = hrefNext
        val id = constants.hrefIdPrefix + idNum
        val data = HrefData(href=href,id=id)
        (copy(hrefToHrefData = hrefToHrefData + (href -> data), hrefNext = hrefNext + 1),data)                
    }

  def mapGetConsiderDefault[K,V](m : Map[K,V],k : K) : Option[V] =
    m.get(k).orElse {
      try { Some(m(k)) } catch { case _ : Exception => None }
    }
  
  override def getHyperlinkRPr : Option[RPr] = RE.Remissao(constants.rprStyles)
  override def addUnsupported(msg : String, elem : Any): RendererState =
    copy(unsupportedCases = unsupportedCases :+ ((msg,elem)))      
  override def setCurrentRPrStyle(rPr : Option[RPr]): RendererState =
    copy(currentRPrStyle = rPr)
  override def omissisParStyle: Option[PPr] = RE.Omissis(constants.pprStyles)
  override def nomeAgrupadorPredefPPrStyle(tipo : TAP): Option[PPr] =
    RE.NomeAgrupador(tipo)(constants.pprStyles)
  override def rotuloAgrupadorPredefPPrStyle(tipo : TAP): Option[PPr] =
    RE.RotuloAgrupador(tipo)(constants.pprStyles)
  override def nomeAgrupadorPredefRPrStyle(tipo : TAP): Option[RPr] =
    RE.NomeAgrupador(tipo)(constants.rprStyles)
  override def rotuloAgrupadorPredefRPrStyle(tipo : TAP): Option[RPr] =
    RE.RotuloAgrupador(tipo)(constants.rprStyles)
    
  override def epigrafeParStyle: Option[PPr] = constants.pprStyles.get(RE.Epigrafe)
  override def epigrafeCharStyle: Option[RPr] = constants.rprStyles.get(RE.Epigrafe)
  override def ementaParStyle: Option[PPr] = constants.pprStyles.get(RE.Ementa)
  override def ementaCharStyle: Option[RPr] = constants.rprStyles.get(RE.Ementa)
  override def preambuloParStyle: Option[PPr] = constants.pprStyles.get(RE.Preambulo)
  override def preambuloCharStyle: Option[RPr] = constants.rprStyles.get(RE.Preambulo)
  override def formulaPromulgacaoParStyle: Option[PPr] = constants.pprStyles.get(RE.FormulaPromulgacao)
  override def formulaPromulgacaoCharStyle: Option[RPr] = constants.rprStyles.get(RE.FormulaPromulgacao)
  override def textoAlteracaoRPrStyle: Option[RPr] = constants.rprStyles.get(RE.TextoAlteracao)
  override def indentAlteracao: Ind = constants.indentAlteracao
  override def rotuloStyleRPrForDispositivo(t : TipoDispositivo): Option[RPr] = RE.RotuloDispositivo(t)(constants.rprStyles)
  override def contentStyleRPrForDispositivo(t : TipoDispositivo): Option[RPr] = RE.ConteudoDispositivo(t)(constants.rprStyles)
  override def contentStylePPrForDispositivo(t : TipoDispositivo): Option[PPr] = RE.RotuloDispositivo(t)(constants.pprStyles)
  override def nomeAgrupadorPredefIniciaisMaiusc(tipo : TAP): Boolean =
    constants.iniciaisMaiusculas.contains(RE.NomeAgrupador(tipo))
  override def tituloStylePPrForDispositivo(t : TipoDispositivo): Option[PPr] = RE.TituloDispositivo(t)(constants.pprStyles)
  override def tituloStyleRPrForDispositivo(t : TipoDispositivo): Option[RPr] = RE.TituloDispositivo(t)(constants.rprStyles)
  override def localDataFechoStylePPr: Option[PPr] = constants.pprStyles.get(RE.LocalDataFecho)
  override def localDataFechoStyleRPr: Option[RPr] = constants.rprStyles.get(RE.LocalDataFecho)
  override def assinaturaTextoStylePPr: Option[PPr] = constants.pprStyles.get(RE.AssinaturaTexto)
  override def assinaturaTextoStyleRPr: Option[RPr] = constants.rprStyles.get(RE.AssinaturaTexto)
  override def notaReferenciadaRPrStyle: Option[RPr] = constants.rprStyles.get(RE.NotaReferenciada)
  
  override def endnoteId(origId : String) : String = 
    endnoteIdMap(origId)
    
  override def skipHyperlink: Boolean = constants.skipHyperlink
end RendererState

object RendererState:
  given mergeable: Mergeable2[RendererState, RendererState] = new Mergeable2[RendererState,RendererState]:
    override def merge(a : RendererState, b : RendererState): RendererState =
      RendererState(
          base = a.base,
          hrefNext = b.hrefNext,
          hrefToHrefData = b.hrefToHrefData,
          unsupportedCases = b.unsupportedCases,
          currentRPrStyle = a.currentRPrStyle,          
          constants = a.constants,
          endnoteRefs = a.endnoteRefs ++ b.endnoteRefs,
          endnoteIdMap = a.endnoteIdMap ++ b.endnoteIdMap)
    def extract(a : RendererState): RendererState = a

class Renderers(config : LexmlToDocxConfig) extends RunBuilderOps[RendererState] with ParBuilderOps[RendererState]
  with DocxCompSeqBuilderOps[RendererState] {
  val logger: Logger = LoggerFactory.getLogger("br.gov.lexml.renderer.docx.renderers")
  
  type RunRenderer[A] = PB[A] 
  
  type ParRenderer[A] = MB[A]
  
  import RendererState.mergeable

  def rotulo(r : Rotulo, rPr : Option[RPr] = None, extraSpace : Boolean = false) : RunRenderer[Unit] =
    val suffix = if extraSpace then " " else ""
    runM_(rPr)(text(r.rotulo + suffix))

  def omissis : RunRenderer[Unit] = runM_()(tab)
                
  def parOmissis(x : Omissis) : ParRenderer[Unit] = 
    aspasP(x.abreAspas,x.fechaAspas,x.notaAlteracao) {
      for {
        style <- inspectMDState(_.omissisParStyle)
        _ <- parM(style)(omissis)
      } yield ()
    }
  
  def agrupadorGenerico(ag : AgrupadorGenerico) : ParRenderer[Unit] = 
    modifyMDState(_.addUnsupported("agrupadorGenerico: não suportado: ",ag))
    
  def agrupadorPredef(ag : AgrupadorPredef) : ParRenderer[Unit] =
    import implicits.ifDef
    aspasP(ag.abreAspas,ag.fechaAspas,ag.notaAlteracao) {      
        for {    
          nomeAgrupadorPPrStyle <- inspectMDState(_.nomeAgrupadorPredefPPrStyle(ag.tipoAgrupador))        
          nomeAgrupadorRPrStyle <- inspectMDState(_.nomeAgrupadorPredefRPrStyle(ag.tipoAgrupador))
          rotuloAgrupadorPPrStyle <- inspectMDState(_.rotuloAgrupadorPredefPPrStyle(ag.tipoAgrupador))
          rotuloAgrupadorRPrStyle <- inspectMDState(_.rotuloAgrupadorPredefRPrStyle(ag.tipoAgrupador))   
          iniciaisMaiusc <- inspectMDState(_.nomeAgrupadorPredefIniciaisMaiusc(ag.tipoAgrupador))
          _ <- ag.rotulo.ifDef(x => parM(rotuloAgrupadorPPrStyle)(
              rotulo(x,rotuloAgrupadorRPrStyle)
              ))
          _ <- ag.nomeAgrupador.ifDef(x => parM(nomeAgrupadorPPrStyle)(
              withStyleRunRenderer(nomeAgrupadorRPrStyle)(
              inlineSeq(x.inlineSeq)
              )))                  
        } yield ()
      } .flatMap { _ =>  mapM_(ag.elems)(hierarchicalElement) }
  
  
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
      newBase = oldBase.map(_.resolve(r.base.uri)).orElse(Some(r.base.uri))
      _ <- modifyPState(_.setBase(newBase))
      _ <- inlineSeq(r.inlineSeq)
      _ <- modifyPState(_.setBase(oldBase))      
    } yield ()
  
  def remissao(r : Remissao) : RunRenderer[Unit] = /* for {
      base <- inspectPState(_.getBase) 
      href = base.map(_.resolve(r.href.uri)).getOrElse(r.href.uri)
      hd <- modifyPStateV(_.addRef(href))
      rPr <- inspectPState(_.getHyperlinkRPr)
      _ <- withStyleRunRenderer(rPr)(hyperlink(id = Some(hd.id),anchor=hd.anchor,tooltip=hd.tooltip)(inlineSeq(r.inlineSeq)))      
    } yield (()) */ for {
      sh <- inspectPState(_.skipHyperlink)
      _ <- 
        if (!sh) { for {
          base <- inspectPState(_.getBase) 
          href = base.map(_.resolve(r.href.uri)).getOrElse(r.href.uri)
          lexmlPortalLink = java.net.URL(config.getLinkTemplate.format(href)).toURI
          hd <- modifyPStateV(_.addRef(lexmlPortalLink))
          rPr <- inspectPState(_.getHyperlinkRPr)
          _ <- withStyleRunRenderer(rPr)(hyperlink(id = Some(hd.id),anchor=hd.anchor,tooltip=hd.tooltip)(inlineSeq(r.inlineSeq)))  
        } yield () } else { inlineSeq(r.inlineSeq) }
    } yield ()
  
  def articulacao(a : Articulacao) : ParRenderer[Unit] =
      mapM_(a.elems)(hierarchicalElement)
  
  def formulaPromulgacao(e : FormulaPromulgacao) : ParRenderer[Unit] =
    aspasP(e.abreAspas,e.fechaAspas,e.notaAlteracao)(
    for {
      pPr <- inspectMDState(_.formulaPromulgacaoParStyle)
      rPr <- inspectMDState(_.formulaPromulgacaoCharStyle)
      _ <- mapM_(e.inlineSeqs)(x => parM(pPr)(withStyleRunRenderer(rPr)(inlineSeq(x.inlineSeq))))
    } yield (()))
             
  def epigrafe(e : Epigrafe) : ParRenderer[Unit] =
    aspasP(e.abreAspas,e.fechaAspas,e.notaAlteracao)(
    for {
      pPr <- inspectMDState(_.epigrafeParStyle)
      rPr <- inspectMDState(_.epigrafeCharStyle)
      _ <- parM(pPr)(withStyleRunRenderer(rPr)(inlineSeq(e.inlineSeq)))
    } yield (()))
          
    
  def ementa(e : Ementa) : ParRenderer[Unit] =
    aspasP(e.abreAspas,e.fechaAspas,e.notaAlteracao)(
    for {
      pPr <- inspectMDState(_.ementaParStyle)
      rPr <- inspectMDState(_.ementaCharStyle)
      _ <- parM(pPr)(withStyleRunRenderer(rPr)(inlineSeq(e.inlineSeq)))
    } yield (())) 
    
  def preambulo(pb : Preambulo) : ParRenderer[Unit] =
    aspasP(pb.abreAspas,pb.fechaAspas,pb.notaAlteracao)(
      mapM_(pb.inlineSeqs)(preambuloLine))
          
  def preambuloLine(pl : PreambuloLine) : ParRenderer[Unit] =
  	for {
      pPr <- inspectMDState(_.preambuloParStyle)
      rPr <- inspectMDState(_.preambuloCharStyle)
      _ <- parM(pPr)(withStyleRunRenderer(rPr)(inlineSeq(pl.inlineSeq)))
    } yield (())
  
  def genHtmlInlineElement(ge : GenHtmlInlineElement) : RunRenderer[Unit] = 
    ge.tipoHtmlInlineElement match {
      case TGHIE.B => boldened(inlineSeq(ge.inlineSeq))
      case TGHIE.I => italicized(inlineSeq(ge.inlineSeq))
      case TGHIE.Sub => inSubscript(inlineSeq(ge.inlineSeq))
      case TGHIE.Sup => inSuperscript(inlineSeq(ge.inlineSeq))
      case TGHIE.Ins => insertion(ge)
      case TGHIE.Del => deletion(ge)
      case TGHIE.Dfn => definition(ge)
    }     
  
  def runWithRPrChange[A](f : RPr => RPr)(rr : RunRenderer[A]) : RunRenderer[A] = 
    for {
      rPr <- inspectPState(_.currentRPrStyle)
      rPr1 = f(rPr.getOrElse(RPr()))
      _ <- modifyPState(_.setCurrentRPrStyle(Some(rPr1)))
      res <- rr
      _ <- modifyPState(_.setCurrentRPrStyle(rPr))
    } yield (res)
    
  def boldened[T](rr : RunRenderer[T]) : RunRenderer[T] =
    runWithRPrChange(_.copy(bold = Some(true)))(rr)
      
  def italicized[T](rr : RunRenderer[T]) : RunRenderer[T] = 
    runWithRPrChange(_.copy(italics = Some(true)))(rr)
    
  def inSubscript[T](rr : RunRenderer[T]) : RunRenderer[T] = 
    runWithRPrChange(_.copy(vertAlign = Some(VA.Subscript)))(rr)
  
  def inSuperscript[T](rr : RunRenderer[T]) : RunRenderer[T] = 
    runWithRPrChange(_.copy(vertAlign = Some(VA.Superscript)))(rr)
      
  def insertion(x : GenHtmlInlineElement) : RunRenderer[Unit] =  
    modifyPState(_.addUnsupported("insertion(GenHtmlInlineElement)",x))
    
  def deletion(x : GenHtmlInlineElement) : RunRenderer[Unit] =  
    modifyPState(_.addUnsupported("deletion(GenHtmlInlineElement)",x))
    
  def definition(x : GenHtmlInlineElement) : RunRenderer[Unit] =  
    modifyPState(_.addUnsupported("definition(GenHtmlInlineElement)",x))
  
  def htmlInlineElement(hie : HTMLinlineElement) : RunRenderer[Unit] = hie match {
    case x : Anchor => anchor(x)
    case x : Span => span(x)
    case x : GenHtmlInlineElement => genHtmlInlineElement(x)    
  }
    
  def lxInlineElement(e : LXInlineElement) : RunRenderer[Unit] = e match {
    case x : Remissao => remissao(x)
    case x : RemissaoMultipla => remissaoMultipla(x)
    case x => modifyPState(_.addUnsupported("lxInlineElement: não suportado",x))
  }
  
  def inlineElement(ie : InlineElement) : RunRenderer[Unit] = ie match {
      case nr : NotaReferenciada => notaReferenciada(nr) 
      case ie : LXInlineElement => lxInlineElement(ie)
      case hie: HTMLinlineElement => htmlInlineElement(hie)      
      case x => modifyPState(_.addUnsupported("lxInlineElement: não suportado",x))
    }
  
  def notaReferenciada(nr : NotaReferenciada) : RunRenderer[Unit] = for {        
    //rPr <- inspectPState(_.notaReferenciadaRPrStyle)
    id <- inspectPState(_.endnoteId(nr.nota.id))    
    rPr <- inspectPState(_.currentRPrStyle)
    _ <- runM(rPr)(text("("))
    _ <- runM(rPr)(endnoteReference(id))
    _ <- runM(rPr)(text(")"))
  } yield ()
  
  def inlineSeq(il : InlineSeq) : RunRenderer[Unit] = {          
    val rr = mixed(il.mixedElems)
    il.lang.map(l => inLang(l.code)(rr)).getOrElse(rr)    
  }
    
  def mixed(m : Mixed[InlineElement]) : RunRenderer[Unit] = mixedWith[InlineElement](m,inlineElement)
    
  def mixedWith[A](m : Mixed[A], f : A => RunRenderer[Unit]) : RunRenderer[Unit] = for {
    rPr <- inspectPState(_.currentRPrStyle)
    _ <- mapM_(m.elems)(x => x match {
      case Left(data) => f(data)
      case Right(txt : String) => runM_(rPr)(text(txt))      
    })
  } yield (())
  
  def inLang[A](lang : String)(rr : RunRenderer[A]) : RunRenderer[A] = {
     modifyPState(_.addUnsupported("inLang: não suportado",lang)) .flatMap(_ => rr)       
  }
  
  def projetoNorma(pj : ProjetoNorma) : ParRenderer[Unit] = norma(pj.norma)
    
  def norma(norma : Norma) : ParRenderer[Unit] = hierarchicalStructure(norma.contents)
  
  def hierarchicalStructure(hs : HierarchicalStructure) : ParRenderer[Unit] = {
    for {
      _ <- hs.formulaPromulgacao.ifDef(formulaPromulgacao)
      _ <- hs.epigrafe.ifDef(epigrafe)
      _ <- hs.ementa.ifDef(ementa)
      _ <- hs.preambulo.ifDef(preambulo)
      _ <- articulacao(hs.articulacao)
      _ <- hs.localDataFecho.ifDef(localDataFecho)
      _ <- mapM_(hs.assinaturas)(assinatura)
    } yield (())
  }  
  
  def lexmlDocument(doc : SomeLexmlDocument) : ParRenderer[Unit] =
    documentContents(doc.value.contents)

  def lexmlDocument(doc: DocumentContents[_]): ParRenderer[Unit] =
    documentContents(doc)

    
  def documentContents(contents : DocumentContents[_]) : ParRenderer[Unit] = contents match {
    case n : Norma => norma(n)
    case pj : ProjetoNorma => projetoNorma(pj)
  }
  
  def parTexto(rPr : Option[RPr] = None)(txt : String) : ParRenderer[Unit] = 
    parM()(runM_(rPr)(text(txt)))
  
  def alteracao(a : Alteracao) : ParRenderer[Unit] = {
    val baseUri = a.base.map(_.uri) 
    for {           
      rPrTexto <- inspectMDState(_.textoAlteracaoRPrStyle)
      ind <- inspectMDState(_.indentAlteracao)
      _ <- modifyMDState(_.setBase(baseUri))
      inside = mapM_(a.mixedElems.elems)(_.fold(alteracaoElement,parTexto(rPrTexto))) : ParRenderer[Unit]
      _ <- indenting(ind)(inside)
      _ <- modifyMDState(_.setBase(None))            
    } yield (())
  }  
      
  def addIndent(indent : Ind, p : P) : P = {
    val pPr = p.pPr.orElse(Some(PPr())).map { ppr => ppr.copy(ind = Some(indent)) }      
    p.copy(pPr = pPr)
  }
  
  def addIndents(indent : Ind, elems : Seq[DocxTextComponent]) : Seq[DocxTextComponent] = 
    elems.collect {
      case p : P => addIndent(indent,p)
      case x => x 
    }
  
  
  def indenting[T](indent : Ind)(pr : ParRenderer[T]) : ParRenderer[T] = State { st =>
    val st1 = st.copy(contents = Seq())
    val (st2,res) = pr.run(st1).value
    val comps = st2.contents
    val comps1 : Seq[DocxTextComponent] = st.contents ++ addIndents(indent,comps)
    (st2.copy(contents = comps1),res)
  }
  
  
  def alteracaoElement(a : AlteracaoElement) : ParRenderer[Unit] =
      a match {
        case x : BlockElement => blockElement(x)
        case x : Container => container(x)
        case x : FormulaPromulgacao => formulaPromulgacao(x)
        case x : Epigrafe => epigrafe(x)
        case x : Ementa => ementa(x)
        case x : Preambulo => preambulo(x)
        case x : HierarchicalElement => hierarchicalElement(x)
        case x : LXContainer => lxContainer(x,false)
      }

  def lxContainer(lc : LXContainer,skipFirst : Boolean) : ParRenderer[Unit] = lc match {
      case d : Dispositivo => dispositivo(d,skipFirst)
      case o : Omissis => if (skipFirst) { State.pure(()) } else { parOmissis(o) }
    }
    
  def dispositivo(d : Dispositivo, skipFirst : Boolean) : ParRenderer[Unit] = d match {
    case dg : DispositivoGenerico => dispositivoGenerico(dg,skipFirst)
    case dn : DispositivoPredefNA => dispositivoPredefNA(dn,skipFirst)
    case a : Artigo => artigo(a)
  }

  def dispositivoGenerico(d : DispositivoGenerico, skipFirst : Boolean) : ParRenderer[Unit] =
    aspasP(d.abreAspas,d.fechaAspas,d.notaAlteracao) {
    for {
      rotuloRPr <- rPrRotuloForDispositivo(TDP.Pena,d.rotulo)
      contentRPr <- inspectMDState(_.contentStyleRPrForDispositivo(TDP.Pena))
      contentPPr <- inspectMDState(_.contentStylePPrForDispositivo(TDP.Pena))
      (firstInlineSeq ,restInlineSeqs)  = (d.conteudo match {
          case None => (State.pure(()),List())
          case Some(t : TextoDispositivo) => (
              t.inlineSeqs.map(_.inlineSeq).headOption.ifDef(x => withStyleRunRenderer(contentRPr)(inlineSeq(x))),
              t.inlineSeqs.tail.map(_.inlineSeq).to(List))
          case Some(OmissisSimples) => (
              omissis,
              List())      
        }) : (RunRenderer[Unit], Seq[InlineSeq])
      (head,tail) /* : (ParRenderer[Unit],Seq[InlineSeq]) */ = (if(!skipFirst) {        
        val r1 = d.rotulo.ifDef(x => rotulo(x,rotuloRPr,true))
        val c1 = firstInlineSeq
        val l1 = parM(contentPPr)(r1 >> c1)
        (l1,restInlineSeqs)
      } else {
        (State.pure(()),restInlineSeqs)
      }) : (ParRenderer[Unit],Seq[InlineSeq])
      rendPar = (p : InlineSeq) => 
        parM(contentPPr)(withStyleRunRenderer(contentRPr)(inlineSeq(p)))
      tail1 = mapM_(tail)(rendPar)
      firstPart = head.flatMap { _ => mapM_(tail)(rendPar) }          
      _ <- firstPart
      _ <- d.alteracao.ifDef(alteracao)
      _ <- mapM_(d.containers){x => lxContainer(x,false) }       
    } yield (())
  }
  //    modifyMDState(_.addUnsupported("dispositivoGenerico: não suportado",dg))
  
  def anchor(a : Anchor) :  RunRenderer[Unit] =
    modifyPState(_.addUnsupported("anchor: não suportado: ",a))
    
  def span(s : Span) : RunRenderer[Unit] = 
    remissao(Remissao(href=s.href,inlineSeq=s.inlineSeq))
  
  def withStyleRunRenderer[E](rPr : Option[RPr] = None)(rr : RunRenderer[E]) : RunRenderer[E] = 
    for {
      st <- inspectPState(_.currentRPrStyle)
      _ <- modifyPState(_.setCurrentRPrStyle(mergeRPr(st,rPr)))
      res <- rr
      _ <- modifyPState(_.setCurrentRPrStyle(st))
    } yield (res)
    
  def mergeRPr(r1 : Option[RPr],r2 : Option[RPr]) : Option[RPr] = {
      r1.flatMap(x => r2.flatMap(y => Some(x + y)).orElse(Some(x))).orElse(r2)
    } 
  
  def rPrRotuloForDispositivo(t : TDP,r : Option[Rotulo]) : ParRenderer[Option[RPr]] =
    inspectMDState(_.rotuloStyleRPrForDispositivo(t))
    /* for {
    rotuloRPr <- inspectMDState(_.rotuloStyleRPrForDispositivo(t))  
    rPr = t match {
      case TDP.Paragrafo if r.map(_.rotulo.trim.contains("nico")).getOrElse(false) =>
        Some(rotuloRPr.getOrElse(RPr()).copy(italics = Some(true), bold = Some(true)))
      case _ => rotuloRPr
    }    
  } yield (rPr) */
    
  def dispositivoPredefNA(d : DispositivoPredefNA,skipFirst : Boolean) : ParRenderer[Unit] = 
    aspasP(d.abreAspas,d.fechaAspas,d.notaAlteracao) {
    for {
      rotuloRPr <- rPrRotuloForDispositivo(d.tipoDispositivo,d.rotulo)
      contentRPr <- inspectMDState(_.contentStyleRPrForDispositivo(d.tipoDispositivo))
      contentPPr <- inspectMDState(_.contentStylePPrForDispositivo(d.tipoDispositivo))
      (firstInlineSeq ,restInlineSeqs)  = (d.conteudo match {
          case None => (State.pure(()),List())
          case Some(t : TextoDispositivo) => (
              t.inlineSeqs.map(_.inlineSeq).headOption.ifDef(x => withStyleRunRenderer(contentRPr)(inlineSeq(x))),
              t.inlineSeqs.tail.map(_.inlineSeq).to(List))
          case Some(OmissisSimples) => (
              omissis,
              List())      
        }) : (RunRenderer[Unit], Seq[InlineSeq])
      (head,tail) /* : (ParRenderer[Unit],Seq[InlineSeq]) */ = (if(!skipFirst) {        
        val r1 = d.rotulo.ifDef(x => rotulo(x,rotuloRPr,true))
        val c1 = firstInlineSeq
        val l1 = parM(contentPPr)(r1 >> c1)
        (l1,restInlineSeqs)
      } else {
        (State.pure(()),restInlineSeqs)
      }) : (ParRenderer[Unit],Seq[InlineSeq])
      rendPar = (p : InlineSeq) => 
        parM(contentPPr)(withStyleRunRenderer(contentRPr)(inlineSeq(p)))
      tail1 = mapM_(tail)(rendPar)
      firstPart = head.flatMap { _ => mapM_(tail)(rendPar) }
      tituloPPr <- inspectMDState(_.tituloStylePPrForDispositivo(TDP.Artigo/*d.tipoDispositivo*/))
      tituloRPr <- inspectMDState(_.tituloStyleRPrForDispositivo(TDP.Artigo/*d.tipoDispositivo*/))
      _ <- d.titulo.ifDef(t => parM(tituloPPr)(withStyleRunRenderer(tituloRPr)(inlineSeq(t.inlineSeq))))
      _ <- firstPart
      _ <- d.alteracao.ifDef(alteracao)
      _ <- mapM_(d.containers){x => lxContainer(x,false) }       
    } yield (())
  }
 
    
  def renderDispHead(
      pPr : Option[PPr] = None,
      rotuloRPr : Option[RPr] = None,      
      r : Option[Rotulo], 
      conteudo : Option[RunRenderer[Unit]]) : 
    ParRenderer[Unit] = {
    parM(pPr)( for {
        _ <- r.ifDef{rot => rotulo(rot,rotuloRPr,true)}
        _ <- conteudo.ifDef(x => x)
      } yield ()
    )
  }  
  
  private val rotuloArtigoPattern = """(^[aA]rt\.)\s+(\S+)""".r
  private val rotuloArtigoUnicoPattern = """^artigo\s\+[uú]nico""".r
  
  def rotuloArtigo(r : Rotulo) : RunRenderer[Unit] = {
    val rPrArtigoUnico = Some(RPr(bold = Some(true), italics = Some(true)))
    val rPrLabelRotuloArtigo = Some(RPr(bold = Some(true)))
    val rPrNumRotuloArtigo = None    
    r.rotulo.toLowerCase.trim match {
      case rotuloArtigoUnicoPattern() =>
        runM_(rPrArtigoUnico)(text(r.rotulo))
      case _ => r.rotulo.trim match {
        case rotuloArtigoPattern(label,num) =>
          runM_(rPrLabelRotuloArtigo)(text(label + " ")) .flatMap (_ =>
          runM_(rPrNumRotuloArtigo)(text(num + " ")))
        case _ =>
          modifyPState(_.addUnsupported("rótulo de artigo",r)).flatMap (_ =>
          runM_(rPrNumRotuloArtigo)(text(r.rotulo)))
      }
    }    
  }
  
  def artigo(a : Artigo) : ParRenderer[Unit] = {
    this.ensuring(a.tipoDispositivo != null)    
    val rotuloArt = a.rotulo.ifDef(rotuloArtigo)     
    val selectFirst = ({
      case t : TextoDispositivo => (inlineSeq(t.inlineSeqs.head.inlineSeq),true)  
      case OmissisSimples => (omissis,true)        
      } : PartialFunction[ConteudoDispositivo,(RunRenderer[Unit],Boolean)])
    val (conteudo,skipFirst,contFechaAspas,contNotaAlteracao) = (a.containers.head match {      
          case d : DispositivoPredefNA if d.tipoDispositivo == TDP.Caput =>
            val (ct,skip) = d.conteudo.collect(selectFirst).getOrElse((State.pure(()),false)) : (RunRenderer[Unit],Boolean)
            (ct,skip,d.fechaAspas,d.notaAlteracao)
          case _ => (State.pure(()),false,false,None)
      }) : (RunRenderer[Unit],Boolean,Boolean,Option[String])    
    aspasP(a.abreAspas,a.fechaAspas,a.notaAlteracao)(for {      
        rotuloRPr <- inspectMDState(_.rotuloStyleRPrForDispositivo(a.tipoDispositivo))
        contentRPr <- inspectMDState(_.contentStyleRPrForDispositivo(a.tipoDispositivo))
        contentPPr <- inspectMDState(_.contentStylePPrForDispositivo(a.tipoDispositivo))
        tituloArtigoPPr <- inspectMDState(_.tituloStylePPrForDispositivo(a.tipoDispositivo))
        tituloArtigoRPr <- inspectMDState(_.tituloStyleRPrForDispositivo(a.tipoDispositivo))
        _ <- a.titulo.ifDef(t => parM(tituloArtigoPPr)(withStyleRunRenderer(tituloArtigoRPr)(inlineSeq(t.inlineSeq))))
        _ <- aspasP(false,contFechaAspas,contNotaAlteracao) { parM(contentPPr) { rotuloArt.flatMap(_ => conteudo.flatMap(_ => State.pure(()))) } }
        _ <- a.containers.headOption.ifDef(x => lxContainer(x,skipFirst))
        _ <- mapM_(a.containers.tail){x => lxContainer(x,false) }
      } yield ()
    )
  }
  
  def localDataFecho(ldf : LocalDataFecho) : ParRenderer[Unit] = for {
    localDataFechoPPr <- inspectMDState(_.localDataFechoStylePPr)
    localDataFechoRPr <- inspectMDState(_.localDataFechoStyleRPr)
    _ <- mapM_(ldf.inlineSeqs)(x => parM(localDataFechoPPr)(withStyleRunRenderer(localDataFechoRPr)(inlineSeq(x.inlineSeq))))
  } yield (())
  
  def assinatura(assinatura : ParteFinalAssinatura) : ParRenderer[Unit] = assinatura match {
    case x: ElementoAssinaturaGrupo => elementoAssinaturaGrupo(x)
    case ag: AssinaturaGrupo => State.pure(())
  }

  def elementoAssinaturaGrupo(x : ElementoAssinaturaGrupo) : ParRenderer[Unit] = x match {
    case at : AssinaturaTexto => for {
      assinaturaTextoPPr <- inspectMDState(_.assinaturaTextoStylePPr)
      assinaturaTextoRPr <- inspectMDState(_.assinaturaTextoStyleRPr)
      _ <- mapM_(at.inlineSeqs)(x => parM(assinaturaTextoPPr)(withStyleRunRenderer(assinaturaTextoRPr)(inlineSeq(x.inlineSeq))))
    } yield ()
    case ax : Assinatura => State.pure(())
  }
  
  def blockElement(b : BlockElement) : ParRenderer[Unit] = b match {    
    case x : HTMLBlock => htmlBlock(x)
    case _ => modifyMDState(_.addUnsupported("blockElement: não suportado: ",b))
  }
  
  def htmlBlock(b : HTMLBlock) : ParRenderer[Unit] = b match {
    case x : Paragraph => paragraph(x)
    case x : HTMLList => htmlList(x)
    case x : Table => table(x)
  }
  
  def paragraph(p : Paragraph) : ParRenderer[Unit] = 
     aspasP(p.abreAspas,p.fechaAspas,p.notaAlteracao)(parM()(inlineSeq(p.inlineSeq)))
    
  def htmlList(hl : HTMLList) : ParRenderer[Unit] = 
    modifyMDState(_.addUnsupported("htmlList: não suportado: ",hl))
    
  def table(t : Table) : ParRenderer[Unit] = 
    modifyMDState(_.addUnsupported("table: não suportado: ",t))
    
  def container(e : Container) : ParRenderer[Unit] = 
    modifyMDState(_.addUnsupported("container: não suportado: ",e))
 
    
  def aspasP[A](abreAspas : Boolean, fechaAspas : Boolean, notaAlteracao : Option[String])(rr : ParRenderer[A]) : ParRenderer[A] = State { st =>
    val st0 = st.copy(contents = Seq())
    val (st1,res) = rr.run(st0).value
    val v2 = mergeable.merge(st.value,st1.value)
    val elems = addAspasP(st1.contents,abreAspas,fechaAspas,notaAlteracao)
    val st2 = st.copy(contents = st.contents ++ elems,value = v2)
    (st2,res)
  }  
    
  //TODO: Inserir nota de alteração
         
  def addAspasP(els : Seq[DocxTextComponent],abreAspas : Boolean, fechaAspas : Boolean,
      notaAlteracao : Option[String])
          : Seq[DocxTextComponent] = {
    if((abreAspas || fechaAspas || !notaAlteracao.isEmpty) && els.exists(_.isInstanceOf[P])) {
      val (head,tail1) = els.span(!_.isInstanceOf[P])
      val revTail1 = tail1.reverse
      val (revTail,revMiddle) = revTail1.span(!_.isInstanceOf[P])      
      val tail = revTail.reverse
      val middle = revMiddle.reverse.collect { case x : P => x }
      val middle1 = if(abreAspas) { insertAbreAspas(middle.head) +: middle.tail } else middle  
      val last1 = middle1.last
      val last2 = if (fechaAspas) { last1.insertLast(fechaAspasRun) } else { last1 }
      val last3 = notaAlteracao.map { x =>
        val r = R(contents = Seq(T(s" (${x.trim})",preserveSpace=true)))
        last2.insertLast(r)
      }.getOrElse(last2)
      val middle2 = if(fechaAspas) { middle1.init :+ last3 } else { middle1 }      
      val res = head ++ middle2 ++ tail      
      res
    } else { els }
  }
  
  val abreAspasRun : R = R(contents = Seq(T("“")))
  val fechaAspasRun : R = R(contents = Seq(T("”")))
  
  def insertAbreAspas(p : P) : P = p.insertFirst(abreAspasRun)
    
  def insertFechaAspas(p : P) : P = p.insertLast(fechaAspasRun)
    
 
  def runM_(rPr : Option[RPr] = None)(rr : RB[Unit]) : PB[Unit] = 
    for {      
      rPr1 <- inspectPState(_.currentRPrStyle)
      rPr2 = mergeRPr(rPr1, rPr)
      _ <- runM(rPr2)(rr)
    } yield (())
}

final case class MainDocRendererResult(
    docx : Docx,
    unsupportedCases : Seq[(String,Any)])

class WordMarker(regex : String, change : RPr => RPr) {    
  import br.gov.lexml.renderer.docx.docxmodel._  
  private val exprRe = regex.r
      
  def fRunContentContainer(x : RunContent) : Seq[RunContent] = x match {
    case x : Ins => Seq(x.flatMap(fParElementContainer))
    case x : Del => Seq(x.flatMap(fParElementContainer))
    case x => Seq(x)
  }
  
  def trimLeft(rc : RunContent) : RunContent = rc match {
    case t : T => t.copy(text = t.text.replaceAll("^\\p{Space}+",""))
    case x => x
  }

  def hasTrailingSpace(rc : RunContent) : Boolean = rc match {
    case t : T => t.text.matches("\\p{Space}$")
    case x => false
  }

  def fParElementContainer(before : Option[ParElement],x : ParElement, next: Option[ParElement]) : Seq[ParElement]  = x match {
    case x : R =>
      val l : Seq[Either[(T,Seq[(Int,Int)]),RunContent]] = x.contents.collect {
        case t : T =>
          val matches = exprRe.findAllMatchIn(t.text).to(Seq).map { m =>
              (m.start,m.end) }
          if matches.isEmpty then Right(t) else Left(t,matches)
        case t => Right(t)
      }
      var lastHadSpace : Boolean = false
      val pl : Seq[ParElement] = l.flatMap { 
        case Right(t) =>
          val t1 = if lastHadSpace then trimLeft(t) else t
          lastHadSpace = hasTrailingSpace(t1)
          Seq(x.copy(contents = Seq(t1)))
        case Left((t,matches)) =>
          //DEBUG: start
          val doDebug = false //t.text.startsWith("regimento interno")
          def debug(msg : => String) : Unit = if doDebug then println("fParElementContainer: $msg")
          //DEBUG: end

          debug(s"""t = $t, matches=$matches, next = $next""")
          val b = Seq.newBuilder[R]
          var last : Int = 0
          lazy val newRPr = Some(change(x.rPr.getOrElse(RPr())))
          def passthrough(start : Int) : Unit =
            debug(s"""passthrough: start = $start, next = $next""")
            val mayAddWSatEnd = start < t.text.length ||
                                next.isDefined
            if start > last then
              val t1 = t.text.substring(last,start)
              val tb = Seq.newBuilder[T]
              val t2 = t1.replaceAll("\\p{Space}+$"," ")
                         .replaceAll(" +"," ")
              debug(s"""t2 = "${t2}" """)
              val t3 = if(lastHadSpace) { t2.replaceAll("^\\p{Space}+","") } else { t2 }
              val t4 = if(mayAddWSatEnd) { t3 } else { t3.replaceAll("\\p{Space}$","") }
              tb += T(t4, preserveSpace=true)
              lastHadSpace = t4.matches("\\p{Space}$")
              debug(s"""t4="${t4}" """)
              b += x.copy(contents = tb.result())
          end passthrough
          for { 
            (start,end) <- matches
          } {
            passthrough(start)            
            val mid = t.text.substring(start,end)
            b += R(rPr = newRPr,contents = Seq(T(mid)))
            last = end
          }
          passthrough(t.text.length)
          b.result()
      }
      pl
    //case x : ParElementContainer[ParElement] => Seq(x.flatMap(fParElementContainer))
    //case x : RunContentContainer[ParElement] => Seq(x.flatMap(fRunContentContainer))
    case x => Seq(x)
  }
          
  def apply(x : DocxTextComponent) : Seq[DocxTextComponent] = x match {
    case p : P => Seq(p.flatMap(fParElementContainer))
    case x => Seq(x)
  }
  
  def apply(x : DocxMainDocument) : DocxMainDocument = 
    x.flatMap(apply)
}

object WordMarker:
  def makeOr(exprs : Seq[String]): String = exprs.map(x => "(?:" + x + ")").mkString("|")
    
  val AddItalics : RPr => RPr = _.copy(italics = Some(true))
  val AddBold : RPr => RPr = _.copy(bold = Some(true))

final case class MainDocRenderer(constants : Constants = Constants(), baseDocx : Docx = Docx(),
                                 config : LexmlToDocxConfig):
  val st0: RendererState = RendererState(constants = constants, endnoteIdMap = Map("aaa" -> "bbb"))
    
  val reformatRules : Seq[(String,RPr => RPr)] = Seq(
      WordMarker.makeOr(constants.expressoesEmBold.to(Seq)) -> WordMarker.AddBold
      )

  val renderers = Renderers(config)
  def makeEndnotes(notas : Seq[(Int,Option[String],Nota)]) :
    (Seq[(String,Seq[DocxTextComponent])],Seq[String],Map[String,String]) = {
    val orphans = notas.collect { (seq,None,_) => seq.toString }.to(Seq)
    val endnotes = notas.map { (seq,_,nota) =>
      val contentsM = for {
        _ <- mapM_(nota.contents)(renderers.paragraph)
      } yield ()
      val contents = contentsM.makeDocxCompSeq(st0).value._1
      val contents1 = reformatRules.foldLeft(contents) { case (d,(regex,change)) =>
        d.flatMap { x => WordMarker(regex,change)(x) }
      }
      (seq.toString,contents1)
    }
    val idToSeq = notas.collect { 
      case (seq,Some(id),_) => (id,seq.toString) 
      case (seq,None,_) => (seq.toString,seq.toString)
      }.toMap    
    (endnotes,orphans,idToSeq)
  }

  def render(doc: LexmlDocument[_]): MainDocRendererResult =
    val notas = doc.metadado.notas.zipWithIndex.map { (nota,idx) =>
      (idx+1,nota.id,nota)
    }
    
    val (endnotes,orphans,idToSeq) = makeEndnotes(notas)

    val contents = if orphans.nonEmpty then
      doc.contents.mapNorma { norma =>
        val epigrafe = norma.contents.epigrafe.map { ep =>
          val elems1 = ep.inlineSeq.mixedElems.elems ++
            orphans.map { id => Left(NotaReferenciada(IDREF(id))) }
          ep.copy(inlineSeq = ep.inlineSeq.copy(mixedElems = Mixed(elems1)))
        }
        norma.copy(contents = norma.contents.copy(epigrafe = epigrafe))
      } else doc.contents

    val st0_1 = st0.copy(endnoteIdMap = idToSeq)

    val (d,st1) = renderers.lexmlDocument(contents).makeMainDoc(st0_1).value
    val d1 = d.copy(d.contents.filterNot(_.isEmpty))           
    val d2 = reformatRules.foldLeft(d1) { case (d,(regex,change)) =>
      WordMarker(regex,change)(d)
    }
    
    val docx = baseDocx.copy(mainDoc = d2, hyperlinks = st1.hrefToHrefData.values.to(Seq),
        endnotes = endnotes)
    MainDocRendererResult(
        docx=docx,        
        unsupportedCases = st1.unsupportedCases)
  end render
end MainDocRenderer

object PackageRenderer:
  private def logger = LoggerFactory.getLogger(classOf[PackageRenderer])
  type ReplaceFunc = Option[Array[Byte]] => Option[Array[Byte]]
  def xmlToByteArray(e : scala.xml.Elem): Array[Byte] =
    import java.io.*
    import scala.xml.*
    val w = StringWriter()
    XML.write(w,e,"utf-8",true,null,MinimizeMode.Always)
    w.close()
    w.toString.getBytes("utf-8")

class PackageRenderer(config : LexmlToDocxConfig):
  import PackageRenderer.*
  
  private lazy val referenceEntries =
    import java.io.*
    import java.util.zip.*
    val zis = ZipInputStream(new ByteArrayInputStream(config.referenceDocx))
    val b = Map.newBuilder[String,Array[Byte]]
    var ze : ZipEntry = zis.getNextEntry()
    while(ze != null) {
      val data = IOUtils.toByteArray(zis)
      b += (ze.getName -> data)
      zis.closeEntry()
      ze = zis.getNextEntry()
    }
    zis.close()
    b.result()
    
  private def writeReplace(transf : (String,ReplaceFunc)*) : Array[Byte] =
    import java.io.*
    import java.util.zip.*
    val m : Map[String,ReplaceFunc] =
      transf.groupBy(_._1).view.mapValues { l =>
        val l1 = l.map(_._2)
        val f = l1.foldLeft((x => x) : ReplaceFunc) { (sofar,f) => { x => sofar(f(x)) } }
        f
      }.toMap
    
    //transf.toMap
    val bos = ByteArrayOutputStream()
    val zos = ZipOutputStream(bos)
    val m1 = referenceEntries ++ m.flatMap{ (k,f) =>
      f(referenceEntries.get(k)).map(d => (k,d)) }      
    m1.foreach { (name,data) =>
      val ze = ZipEntry(name)
      ze.setSize(data.length)
      zos.putNextEntry(ze)
      zos.write(data)
      zos.closeEntry()
    }
    zos.close()        
    bos.toByteArray
  end writeReplace

  lazy val stylesElem: Elem = DefaultStyles.styles.asXML  //  DocxMainPartRenderer.stylesElem

  def render(doc: SomeLexmlDocument,
             extraReplace: Seq[(String, PackageRenderer.ReplaceFunc)]): Array[Byte] =
    render(doc.value,extraReplace)
  def render(doc : LexmlDocument[_],
      extraReplace : Seq[(String,PackageRenderer.ReplaceFunc)] = Seq()) : Array[Byte] =
    val baseDocx = Docx(
        baseRelationships =
          referenceEntries.get("word/_rels/document.xml.rels").map { f =>            
            Relationships.fromByteArray(f)
          }.getOrElse(Relationships())
        )    

    val renderer = MainDocRenderer(Constants.default,baseDocx,config)
    val res = renderer.render(doc)
    res.unsupportedCases.foreach { (msg,x) =>
      logger.warn(s"Caso não suportado pelo renderer: $msg\n$x")
    }        
    def subst(v : Array[Byte]) : ReplaceFunc = _ => Some(v)
    val files = res.docx.files
    val replaceFuncs = files.view.mapValues { subst }.to(Seq)  ++ extraReplace
    writeReplace(replaceFuncs*)
  end render
end PackageRenderer