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

final case class HrefData(id : String, tooltip : Option[String] = None, anchor : Option[String] = None,
    rPr : Option[RPr] = None)

trait ParRendererState[T <: ParRendererState[T]] {
  def getBase : Option[URI]
  def setBase(base : Option[URI]) : T 
  def addRef(href : URI) : (T,HrefData)
  def getHyperlinkRPr : Option[RPr]
  def addUnsupported(msg : String, elem : Any) : T
  def currentRPrStyle : Option[RPr]
  def setCurrentRPrStyle(rPr : Option[RPr]) : T
}

trait RunRendererState[T <: RunRendererState[T]] {
  def currentRPrStyle : Option[RPr]
}

trait MainDocRendererState[T <: MainDocRendererState[T]] {  
  def omissisParStyle : Option[PPr]
  def addUnsupported(msg : String, elem : Any) : T
  def nomeAgrupadorPredefPPrStyle(tipo : TipoAgrupadorPredef) : Option[PPr]
  def rotuloAgrupadorPredefPPrStyle(tipo : TipoAgrupadorPredef) : Option[PPr]
  def nomeAgrupadorPredefRPrStyle(tipo : TipoAgrupadorPredef) : Option[RPr]
  def rotuloAgrupadorPredefRPrStyle(tipo : TipoAgrupadorPredef) : Option[RPr]  
  def epigrafeParStyle : Option[PPr]
  def ementaParStyle : Option[PPr]
  def preambuloParStyle : Option[PPr]
  def formulaPromulgacaoParStyle : Option[PPr]
  def textoAlteracaoRPrStyle : Option[RPr]
  def setBase(baseUri : Option[URI]) : T
  def indentAlteracao : Ind           
  def rotuloStyleRPrForDispositivo(t : TipoDispositivo) : Option[RPr]
  def contentStyleRPrForDispositivo(t : TipoDispositivo) : Option[RPr]
  def contentStylePPrForDispositivo(t : TipoDispositivo) : Option[PPr]      
}


final case class Constants(
   hyperlinkRPr : Option[RPr] = None,
   hrefIdPrefix : String = "id_href_",
   omissisParStyle : Option[PPr] = None,
   nomeAgrupadorPredefPPrStyles : Map[TipoAgrupadorPredef,PPr] = Map(),
   rotuloAgrupadorPredefPPrStyles : Map[TipoAgrupadorPredef,PPr] = Map(),
   nomeAgrupadorPredefRPrStyles : Map[TipoAgrupadorPredef,RPr] = Map(),
   rotuloAgrupadorPredefRPrStyles : Map[TipoAgrupadorPredef,RPr] = Map(),   
   epigrafeParStyle : Option[PPr] = None,
   ementaParStyle : Option[PPr] = None,
   preambuloParStyle : Option[PPr] = None,
   formulaPromulgacaoParStyle : Option[PPr] = None,
   textoAlteracaoRPrStyle : Option[RPr] = None,
   indentAlteracao : Ind = Ind(start = 100, hanging = 100),
   rotuloStyleRPrForDispositivos : Map[TipoDispositivo,RPr] = Map(),
   contentStyleRPrForDispositivos : Map[TipoDispositivo,RPr] = Map(),
   contentStylePPrForDispositivos  : Map[TipoDispositivo,PPr] = Map()      
) 

final case class RendererState(
    base : Option[URI] = None,
    hrefNext : Int = 1,
    hrefToId : Map[URI,String] = Map(),    
    unsupportedCases : Seq[(String,Any)] = Seq(),
    currentRPrStyle : Option[RPr] = None,    
    constants : Constants
    ) extends ParRendererState[RendererState] 
        with MainDocRendererState[RendererState] 
        with RunRendererState[RendererState] {  
  override def getBase = base
  override def setBase(b : Option[URI]) = copy(base = b)
  override def addRef(href : URI) = 
    hrefToId.get(href) match {
    case Some(id) => (this,HrefData(id = id))
    case None =>
      val idNum = hrefNext
      val id = constants.hrefIdPrefix + idNum
      (copy(hrefToId = hrefToId + (href -> id), hrefNext = hrefNext + 1),HrefData(id=id))                
  }
  def mapGetConsiderDefault[K,V](m : Map[K,V],k : K) : Option[V] = {
    val res = m.get(k).orElse {
      try { Some(m(k)) } catch { case _ : Exception => None }
    }
    println(s"mapGetConsiderDefault: k = ${k}, m = {$m}, m.get(k) = ${m.get(k)}")
    print("     m(k) = ")
    try {
      println(m(k))
    } catch { case _ : Exception => println("Exception!") }
    res
  }
  
  override def getHyperlinkRPr : Option[RPr] = constants.hyperlinkRPr
  override def addUnsupported(msg : String, elem : Any) =
    copy(unsupportedCases = unsupportedCases :+ ((msg,elem)))      
  override def setCurrentRPrStyle(rPr : Option[RPr]) =
    copy(currentRPrStyle = rPr)
  override def omissisParStyle = constants.omissisParStyle
  override def nomeAgrupadorPredefPPrStyle(tipo : TipoAgrupadorPredef) = 
    mapGetConsiderDefault(constants.nomeAgrupadorPredefPPrStyles,tipo)
  override def rotuloAgrupadorPredefPPrStyle(tipo : TipoAgrupadorPredef) = 
    mapGetConsiderDefault(constants.rotuloAgrupadorPredefPPrStyles,tipo)
  override def nomeAgrupadorPredefRPrStyle(tipo : TipoAgrupadorPredef) = 
    mapGetConsiderDefault(constants.nomeAgrupadorPredefRPrStyles,tipo)
  override def rotuloAgrupadorPredefRPrStyle(tipo : TipoAgrupadorPredef) = 
    mapGetConsiderDefault(constants.rotuloAgrupadorPredefRPrStyles,tipo)  
    
  override def epigrafeParStyle = constants.epigrafeParStyle
  override def ementaParStyle = constants.ementaParStyle
  override def preambuloParStyle = constants.preambuloParStyle
  override def formulaPromulgacaoParStyle = constants.formulaPromulgacaoParStyle
  override def textoAlteracaoRPrStyle = constants.textoAlteracaoRPrStyle  
  override def indentAlteracao = constants.indentAlteracao            
  override def rotuloStyleRPrForDispositivo(t : TipoDispositivo) 
    = mapGetConsiderDefault(constants.rotuloStyleRPrForDispositivos,t)
  override def contentStyleRPrForDispositivo(t : TipoDispositivo) 
    = mapGetConsiderDefault(constants.contentStyleRPrForDispositivos,t)
  override def contentStylePPrForDispositivo(t : TipoDispositivo) 
    = mapGetConsiderDefault(constants.contentStylePPrForDispositivos,t)
}

object RendererState {
  implicit val mergeable = new Mergeable2[RendererState,RendererState] {
    override def merge(a : RendererState, b : RendererState) =
      RendererState(
          base = a.base,
          hrefNext = b.hrefNext,
          hrefToId = b.hrefToId,
          unsupportedCases = b.unsupportedCases,
          currentRPrStyle = a.currentRPrStyle,          
          constants = a.constants)
    def extract(a : RendererState) = a                          
  }
}

object Renderers extends RunBuilderOps[RendererState] with ParBuilderOps[RendererState] 
  with MainDocBuilderOps[RendererState] {
  
  type RunRenderer[A] = PB[A] 
  
  type ParRenderer[A] = MB[A]
  
  import RendererState.mergeable
      
       
  def rotulo(r : Rotulo) : RunRenderer[Unit] = 
    runM_()(text(r.rotulo + " "))
    
  
  def omissis() : RunRenderer[Unit] = 
    runM_()(ptab(alignment = PTA_Right,leader = Some(TL_Dot),
            relativeTo = PTB_Margin ))  
                
  def parOmissis(x : Omissis) : ParRenderer[Unit] = 
    aspasP(x.abreAspas,x.fechaAspas) {
    for {
      style <- inspectMDState(_.omissisParStyle)
      _ <- parM(style)(omissis)
    } yield (())
  }            
  
  def agrupadorGenerico(ag : AgrupadorGenerico) : ParRenderer[Unit] = 
    modifyMDState(_.addUnsupported("agrupadorGenerico: não suportado: ",ag))
    
  def agrupadorPredef(ag : AgrupadorPredef) : ParRenderer[Unit] = 
  aspasP(ag.abreAspas,ag.fechaAspas) {      
      for {    
        nomeAgrupadorPPrStyle <- inspectMDState(_.nomeAgrupadorPredefPPrStyle(ag.tipoAgrupador))        
        nomeAgrupadorRPrStyle <- inspectMDState(_.nomeAgrupadorPredefRPrStyle(ag.tipoAgrupador))
        rotuloAgrupadorPPrStyle <- inspectMDState(_.rotuloAgrupadorPredefPPrStyle(ag.tipoAgrupador))
        rotuloAgrupadorRPrStyle <- inspectMDState(_.rotuloAgrupadorPredefRPrStyle(ag.tipoAgrupador))
        _ = println(s"ag.tipoAgrupador = ${ag.tipoAgrupador} rotuloAgrupadorRPrStyle = ${rotuloAgrupadorRPrStyle}")
        _ <- ag.rotulo.ifDef(x => parM(rotuloAgrupadorPPrStyle)(
            withStyleRunRenderer(rotuloAgrupadorRPrStyle)(
            rotulo(x)
            )))
        _ <- ag.nomeAgrupador.ifDef(x => parM(nomeAgrupadorPPrStyle)(
            withStyleRunRenderer(nomeAgrupadorRPrStyle)(
            inlineSeq(x.inlineSeq)
            )))        
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
      newBase = oldBase.map(_.resolve(r.base.uri)).orElse(Some(r.base.uri))
      _ <- modifyPState(_.setBase(newBase))
      _ <- inlineSeq(r.inlineSeq)
      _ <- modifyPState(_.setBase(oldBase))      
    } yield (())
  
  def remissao(r : Remissao) : RunRenderer[Unit] = for {
      base <- inspectPState(_.getBase) 
      href = base.map(_.resolve(r.href.uri)).getOrElse(r.href.uri)
      hd <- modifyPStateV(_.addRef(href))
      rPr <- inspectPState(_.getHyperlinkRPr)
      _ <- runM_(rPr)(hyperlink(id = Some(hd.id),anchor=hd.anchor,tooltip=hd.tooltip)(inlineSeq(r.inlineSeq)))
    } yield (())
  
  def articulacao(a : Articulacao) : ParRenderer[Unit] =
      mapM_(a.elems)(hierarchicalElement)
  
  def formulaPromulgacao(e : FormulaPromulgacao) : ParRenderer[Unit] =
    aspasP(e.abreAspas,e.fechaAspas)(
    for {
      pPr <- inspectMDState(_.formulaPromulgacaoParStyle)
      _ <- parM(pPr)(inlineSeq(e.inlineSeq))
    } yield (()))
      
  def epigrafe(e : Epigrafe) : ParRenderer[Unit] =
    aspasP(e.abreAspas,e.fechaAspas)(
    for {
      pPr <- inspectMDState(_.epigrafeParStyle)
      _ <- parM(pPr)(inlineSeq(e.inlineSeq))
    } yield (()))
          
    
  def ementa(e : Ementa) : ParRenderer[Unit] =
    aspasP(e.abreAspas,e.fechaAspas)(
    for {
      pPr <- inspectMDState(_.ementaParStyle)
      _ <- parM(pPr)(inlineSeq(e.inlineSeq))
    } yield (())) 
    
  def preambulo(pb : Preambulo) : ParRenderer[Unit] =
    aspasP(pb.abreAspas,pb.fechaAspas)(
      mapM_(pb.inlineSeqs)(preambuloLine))
          
  def preambuloLine(pl : PreambuloLine) : ParRenderer[Unit] =
  	for {
      pPr <- inspectMDState(_.preambuloParStyle)
      _ <- parM(pPr)(inlineSeq(pl.inlineSeq))
    } yield (())
  
  def genHtmlInlineElement(ge : GenHtmlInlineElement) : RunRenderer[Unit] = 
    ge.tipoHtmlInlineElement match {
      case TGHIE_B => boldened(inlineSeq(ge.inlineSeq))
      case TGHIE_I => italicized(inlineSeq(ge.inlineSeq))
      case TGHIE_Sub => inSubscript(inlineSeq(ge.inlineSeq))
      case TGHIE_Sup => inSuperscript(inlineSeq(ge.inlineSeq))      
      case TGHIE_Ins => insertion(ge)         
      case TGHIE_Del => deletion(ge)
      case TGHIE_Dfn => definition(ge)
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
    runWithRPrChange(_.copy(vertAlign = Some(VA_Subscript)))(rr)
  
  def inSuperscript[T](rr : RunRenderer[T]) : RunRenderer[T] = 
    runWithRPrChange(_.copy(vertAlign = Some(VA_Superscript)))(rr)
    
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
      case ie : LXInlineElement => lxInlineElement(ie)
      case hie: HTMLinlineElement => htmlInlineElement(hie)
      case x => modifyPState(_.addUnsupported("lxInlineElement: não suportado",x))
    }
  
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
     modifyPState(_.addUnsupported("inLang: não suportado",lang)) >> rr       
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
    } yield (())
  }  
  
  def lexmlDocument(doc : LexmlDocument) : ParRenderer[Unit] = documentContents(doc.contents)
    
  def documentContents(contents : DocumentContents) : ParRenderer[Unit] = contents match {
    case n : Norma => norma(n)
    case pj : ProjetoNorma => projetoNorma(pj)
    case x => modifyMDState(_.addUnsupported("documentContents: não suportado",x))
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
  
  
  def alteracaoElement(a : AlteracaoElement) : ParRenderer[Unit] = {             
      val inside = a match {      
        case x : BlockElement => blockElement(x)
        case x : Container => container(x)
        case x : FormulaPromulgacao => formulaPromulgacao(x)
        case x : Epigrafe => epigrafe(x)
        case x : Ementa => ementa(x)
        case x : Preambulo => preambulo(x)
        case x : HierarchicalElement => hierarchicalElement(x)
        case x : LXContainer => lxContainer(x,false)
        case _ => modifyMDState(_.addUnsupported("alteracaoElement: não suportado",a))
      }   
      inside
      //aspasP(a.abreAspas,a.fechaAspas)(inside)      
    }
  
  
  
  def lxContainer(lc : LXContainer,skipFirst : Boolean) : ParRenderer[Unit] = lc match {
      case d : Dispositivo => dispositivo(d,skipFirst)
      case o : Omissis => if (skipFirst) { State.pure(()) } else { parOmissis(o) }
      case _ => modifyMDState(_.addUnsupported("lxContainer: não suportado",lc))
    }
    
  def dispositivo(d : Dispositivo, skipFirst : Boolean) : ParRenderer[Unit] = d match {
    case dg : DispositivoGenerico => dispositivoGenerico(dg,skipFirst)
    case dn : DispositivoPredefNA => dispositivoPredefNA(dn,skipFirst)
    case a : Artigo => artigo(a)
  }

  def dispositivoGenerico(dg : DispositivoGenerico, skipFirst : Boolean) : ParRenderer[Unit] =
      modifyMDState(_.addUnsupported("dispositivoGenerico: não suportado",dg))
  
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
  
  def dispositivoPredefNA(d : DispositivoPredefNA,skipFirst : Boolean) : ParRenderer[Unit] = 
    aspasP(d.abreAspas,d.fechaAspas) {
    for {
      rotuloRPr <- inspectMDState(_.rotuloStyleRPrForDispositivo(d.tipoDispositivo))
      contentRPr <- inspectMDState(_.contentStyleRPrForDispositivo(d.tipoDispositivo))
      contentPPr <- inspectMDState(_.contentStylePPrForDispositivo(d.tipoDispositivo))
      (firstInlineSeq ,restInlineSeqs)  = (d.conteudo match {
          case None => (State.pure(()),List())
          case Some(t : TextoDispositivo) => (
              t.inlineSeqs.map(_.inlineSeq).headOption.ifDef(x => withStyleRunRenderer(contentRPr)(inlineSeq(x))),
              t.inlineSeqs.tail.map(_.inlineSeq).to[List])
          case Some(OmissisSimples) => (
              omissis,
              List())      
        }) : (RunRenderer[Unit], Seq[InlineSeq])
      (head,tail) /* : (ParRenderer[Unit],Seq[InlineSeq]) */ = (if(!skipFirst) {        
        val r1 = d.rotulo.ifDef(x => withStyleRunRenderer(rotuloRPr)(rotulo(x)))
        val c1 = firstInlineSeq
        val l1 = parM(contentPPr)(r1 >> c1)
        (l1,restInlineSeqs)
      } else {
        (State.pure(()),restInlineSeqs)
      }) : (ParRenderer[Unit],Seq[InlineSeq])
      rendPar = (p : InlineSeq) => 
        parM(contentPPr)(withStyleRunRenderer(contentRPr)(inlineSeq(p)))
      tail1 = mapM_(tail)(rendPar)
      firstPart = head >> mapM_(tail)(rendPar)          
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
      _ <- r.ifDef{rot => withStyleRunRenderer(rotuloRPr)(rotulo(rot))}
      _ <- conteudo.ifDef(x => x)
    } yield (()))       
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
          runM_(rPrLabelRotuloArtigo)(text(label + " ")) >>
          runM_(rPrNumRotuloArtigo)(text(num))
        case _ =>
          modifyPState(_.addUnsupported("rótulo de artigo",r)) >>
          runM_(rPrNumRotuloArtigo)(text(r.rotulo))
      }
    }    
  }
  
  def artigo(a : Artigo) : ParRenderer[Unit] = 
    aspasP(a.abreAspas,a.fechaAspas)(for {
      rotuloRPr <- inspectMDState(_.rotuloStyleRPrForDispositivo(a.tipoDispositivo))
      contentRPr <- inspectMDState(_.contentStyleRPrForDispositivo(a.tipoDispositivo))
      contentPPr <- inspectMDState(_.contentStylePPrForDispositivo(a.tipoDispositivo))
      rotuloArt = a.rotulo.ifDef(rotuloArtigo)
      selectFirst = ({
        case t : TextoDispositivo => (inlineSeq(t.inlineSeqs.head.inlineSeq),true)  
        case OmissisSimples => (omissis,true)        
      } : PartialFunction[ConteudoDispositivo,(RunRenderer[Unit],Boolean)])
      (conteudo,skipFirst) = (a.containers.head match {      
          case d : DispositivoPredefNA if d.tipoDispositivo == TDP_Caput => 
            d.conteudo.collect(selectFirst).getOrElse(State.pure(()),false)
          case _ => (State.pure(()),false)
      }) : (RunRenderer[Unit],Boolean)
      _ <- parM(contentPPr)(rotuloArt >> conteudo)            
      _ <- a.containers.headOption.ifDef(x => lxContainer(x,skipFirst))
      _ <- mapM_(a.containers.tail){x => lxContainer(x,false) }    
    } yield (()))
  
  def blockElement(b : BlockElement) : ParRenderer[Unit] = b match {    
    case x : HTMLBlock => htmlBlock(x)
    case _ => modifyMDState(_.addUnsupported("blockElement: não suportado: ",b))
  }
  
  def htmlBlock(b : HTMLBlock) : ParRenderer[Unit] = b match {
    case x : Paragraph => paragraph(x)
    case x : HTMLList => htmlList(x)
    case x : Table => table(x)
    case _ => modifyMDState(_.addUnsupported("htmlBlock: não suportado: ",b))
  }
  
  def paragraph(p : Paragraph) : ParRenderer[Unit] = 
     aspasP(p.abreAspas,p.fechaAspas)(parM()(inlineSeq(p.inlineSeq)))
    
  def htmlList(hl : HTMLList) : ParRenderer[Unit] = 
    modifyMDState(_.addUnsupported("htmlList: não suportado: ",hl))
    
  def table(t : Table) : ParRenderer[Unit] = 
    modifyMDState(_.addUnsupported("table: não suportado: ",t))
    
  def container(e : Container) : ParRenderer[Unit] = 
    modifyMDState(_.addUnsupported("container: não suportado: ",e))
 
    
  def aspasP[A](abreAspas : Boolean, fechaAspas : Boolean)(rr : ParRenderer[A]) : ParRenderer[A] = State { st =>
    val st0 = st.copy(contents = Seq())
    val (st1,res) = rr.run(st0).value
    val v2 = mergeable.merge(st.value,st1.value)
    val elems = addAspasP(st1.contents,abreAspas,fechaAspas)
    val st2 = st.copy(contents = st.contents ++ elems,value = v2)
    (st2,res)
  }  
    
  //TODO: Inserir nota de alteração
         
  def addAspasP(els : Seq[DocxTextComponent],abreAspas : Boolean, fechaAspas : Boolean)
          : Seq[DocxTextComponent] = {
    if((abreAspas || fechaAspas) && els.exists(_.isInstanceOf[P])) {
      val (head,tail1) = els.span(!_.isInstanceOf[P])
      val revTail1 = tail1.reverse
      val (revTail,revMiddle) = revTail1.span(!_.isInstanceOf[P])      
      val tail = revTail.reverse
      val middle = revMiddle.reverse.collect { case x : P => x }
      val middle1 = if(abreAspas) { insertAbreAspas(middle.head) +: middle.tail } else middle
      val middle2 = if(fechaAspas) { middle1.init :+ insertFechaAspas(middle1.last) } else middle1
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
      val rPr2 = mergeRPr(rPr1, rPr)
      val _ = println(s"runM_: rPr=${rPr}, rPr1=${rPr1}, rPr2=${rPr2}")
      _ <- runM(rPr2)(rr)
    } yield (())
}

final case class MainDocRendererResult(
    doc : DocxMainDocument,
    idToHref : Map[String,URI],
    unsupportedCases : Seq[(String,Any)])

final case class MainDocRenderer(constants : Constants = Constants()) {  
  def render(doc : LexmlDocument) : MainDocRendererResult = {
    println("MainDocRenderer.render constants = " + constants)
    val st0 = RendererState(constants = constants) 
    val (d,st1) = Renderers.lexmlDocument(doc).makeMainDoc(st0).value
    val d1 = d.copy(d.contents.filterNot(_.isEmpty))
    MainDocRendererResult(
        doc=d1,
        idToHref = st1.hrefToId.to[Seq].map(_.swap).toMap,
        unsupportedCases = st1.unsupportedCases)
  }
}