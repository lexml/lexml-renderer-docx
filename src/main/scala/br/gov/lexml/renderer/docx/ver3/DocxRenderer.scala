package br.gov.lexml.renderer.docx.ver3

import br.gov.lexml.doc._
import scala.xml._
import java.net.URI
import cats._
import cats.implicits._
import cats.data._
import cats.syntax.all._

abstract sealed class ParagraphType(    
    val linkedSuffix : String = null,
    val detParStyleId : String = null,
    val detCharStyleId : String = null) extends Product {
  lazy val (parStyle,charStyle,styleElems) = Option(linkedSuffix) match {
    case Some(idSuffix) =>
      val ((idp,p),(idc,c)) = DocxMainPartRenderer.basicLinkedStyles(idSuffix, idSuffix, None,None,None,None,Some(-1))
      (Some(idp),Some(idc),Seq(p,c))
    case None =>
      val idp = Option(detParStyleId)
      val idc = Option(detCharStyleId)
      val par = idp.map(x => DocxMainPartRenderer.basicParStyle(x, x))
      val char = idc.map(x => DocxMainPartRenderer.basicCharStyle(x, x))
      (idp,idc,Seq(par,char).flatten)        
  }
}

object ParagraphType {
  lazy val types = Set[ParagraphType](
      PT_FORMULA_PROMULGACAO,
      PT_EPIGRAFE,
      PT_EMENTA,
      PT_PREAMBULO,
      PT_PARTE_ROTULO,
      PT_PARTE_NOME,
      PT_LIVRO_ROTULO,
      PT_LIVRO_NOME,
      PT_TITULO_ROTULO ,
      PT_TITULO_NOME,
      PT_CAPITULO_ROTULO,
      PT_CAPITULO_NOME,
      PT_SECAO_ROTULO,
      PT_SECAO_NOME,
      PT_SUBSECAO_ROTULO,
      PT_SUBSECAO_NOME,
      PT_OMISSIS,
      PT_ARTIGO,
      PT_ARTIGO_ROTULO,
      PT_CAPUT_CONTENT,
      PT_PAR,
      PT_PAR_ROTULO,
      PT_PAR_CONTENT,
      PT_INC,
      PT_INC_ROTULO,
      PT_INC_CONTENT,
      PT_ALINEA,
      PT_ALINEA_ROTULO,
      PT_ALINEA_CONTENT,
      PT_ITEM,
      PT_ITEM_ROTULO,
      PT_ITEM_CONTENT,
      PT_PENA,
      PT_PENA_ROTULO,
      PT_PENA_CONTENT,
      PT_ALTERACAO
      )
   val customStyles = 
     types.to[IndexedSeq].flatMap{ x =>
       println("x = " + x)
       x.styleElems } 
  
       
     
}
    
case object PT_FORMULA_PROMULGACAO extends ParagraphType("FormulaPromulgacao")
case object PT_EPIGRAFE extends ParagraphType("Epigrafe")      
case object PT_EMENTA extends ParagraphType("Ementa")      
case object PT_PREAMBULO extends ParagraphType("Preambulo")
case object PT_PARTE_ROTULO extends ParagraphType("ParteRotulo")      
case object PT_PARTE_NOME extends ParagraphType("ParteNome")      
case object PT_LIVRO_ROTULO extends ParagraphType("LivroRotulo")      
case object PT_LIVRO_NOME extends ParagraphType("LivroNome")      
case object PT_TITULO_ROTULO  extends ParagraphType("TituloRotulo")      
case object PT_TITULO_NOME extends ParagraphType("TituloNome")      
case object PT_CAPITULO_ROTULO extends ParagraphType("CapituloRotulo")      
case object PT_CAPITULO_NOME extends ParagraphType("CapituloNome")      
case object PT_SECAO_ROTULO extends ParagraphType("SecaoRotulo")      
case object PT_SECAO_NOME extends ParagraphType("SecaoNome")      
case object PT_SUBSECAO_ROTULO extends ParagraphType("SubsecaoRotulo")      
case object PT_SUBSECAO_NOME extends ParagraphType("SubsecaoNome")      
case object PT_OMISSIS extends ParagraphType("Omissis")
case object PT_ARTIGO extends ParagraphType("Artigo")      
case object PT_ARTIGO_ROTULO extends ParagraphType("ArtigoRotulo")

case object PT_CAPUT_CONTENT extends ParagraphType(detCharStyleId="Caput")

case object PT_PAR extends ParagraphType(detParStyleId="Paragrafo")
case object PT_PAR_ROTULO extends ParagraphType(detCharStyleId="ParagrafoRotulo")      
case object PT_PAR_CONTENT extends ParagraphType(detCharStyleId="ParagrafoContent")

case object PT_INC extends ParagraphType(detParStyleId="Inciso")
case object PT_INC_ROTULO extends ParagraphType(detCharStyleId="IncisoRotulo")      
case object PT_INC_CONTENT extends ParagraphType(detCharStyleId="IncisoContent")

case object PT_ALINEA extends ParagraphType(detParStyleId="Alinea")
case object PT_ALINEA_ROTULO extends ParagraphType(detCharStyleId="AlineaRotulo")      
case object PT_ALINEA_CONTENT extends ParagraphType(detCharStyleId="AlineaContent")

case object PT_ITEM extends ParagraphType(detParStyleId="Item")
case object PT_ITEM_ROTULO extends ParagraphType(detCharStyleId="ItemRotulo")      
case object PT_ITEM_CONTENT extends ParagraphType(detCharStyleId="ItemContent")

case object PT_PENA extends ParagraphType(detParStyleId="Pena")
case object PT_PENA_ROTULO extends ParagraphType(detCharStyleId="PenaRotulo")      
case object PT_PENA_CONTENT extends ParagraphType(detCharStyleId="PenaContent")


case object PT_ALTERACAO extends ParagraphType("Alteracao")    

abstract sealed class SubSup extends Product
  
case object SS_Sub extends SubSup
case object SS_Sup extends SubSup

object Defaults {
  val indent = Ind(start = 500, end = 0, hanging = 500,
      firstLine = 500)
}

final case class StyleContext(
      paragraphType : Option[ParagraphType] = None,
      parStyle : Option[String] = None,
      charStyle : Option[String] = None,            
      boldSet : Option[Boolean] = None,
      italicsSet : Option[Boolean] = None,
      subSup : Option[SubSup] = None,
      capsMode : Option[CapsMode] = None) {
  def toPPr(indented : Boolean = false) : NodeSeq = {
    PPr(
        ind = if(indented) { Some(Defaults.indent) } else { None },    
        style = parStyle
        ).asXML
  }
  def toRPr : NodeSeq = {
    RPr(
    bold = boldSet,
    italics = italicsSet,
    capsMode = capsMode,
    //color : Option[RGB] = None,
    //lang : Option[String] = None,
    //fonts : Option[Fonts] = None,
    rStyle = charStyle,
    //strike : Option[Boolean] = None,
    //sz : Option[Int] = None,
    //szCs : Option[Int] = None,
    //underline : Option[UnderlineOption] = None,
    vertAlign = subSup match {
      case None => None
      case Some(SS_Sub) => Some(VA_Subscript)
      case Some(SS_Sup) => Some(VA_Superscript)
    }
    ).asXML    
  }
  
  def bold = { copy(boldSet = Some(true)) }
  def italics = { copy(italicsSet = Some(true)) }
  def sup = { copy(subSup = Some(SS_Sup)) }
  def sub = { copy(subSup = Some(SS_Sub)) }
  def caps = { copy(capsMode = Some(CM_Caps)) }
  def centered = { this }
  
  def +(s : StyleContext) =
      StyleContext(paragraphType = s.paragraphType orElse paragraphType,
           parStyle = s.parStyle orElse parStyle,
           charStyle = s.charStyle orElse charStyle,
           boldSet = s.boldSet orElse boldSet,
           italicsSet = s.italicsSet orElse italicsSet,
           subSup = s.subSup orElse subSup,
           capsMode = s.capsMode orElse capsMode
          )
}

final case class ParRendererState(    
    pars : Seq[Elem] = Seq(),
    sideData : SideData = SideData(),
    indented : Boolean = false,
    abreAspas : Boolean = false,
    fechaAspas : Boolean = false) {
    def base(base : URI) : ParRendererState = {      
      copy(sideData = sideData.base(base))
    }
    def setAbreAspas = copy(abreAspas = true)
    def setFechaAspas = copy(fechaAspas = true)
    def resolveUri(uri : URI) = {
      sideData.resolveUri(uri)
    }    
    def indent = copy(indented = true)
}

    
    
final case class RunRendererState(
    style : StyleContext = StyleContext(),
    runs : Seq[Node] = Seq(),    
    sideData : SideData = SideData(),
    lang : Option[String] = None
    )     {  
  def optLang(l : Option[Lang]) : RunRendererState =
      l.map(lang).getOrElse(this)
      
    def lang(l : Lang) : RunRendererState = 
      if(l.isDefault) {
        copy(lang = None)
      } else {
        copy(lang = Some(l.code))
      }    
    def base(base : URI) = {      
      copy(sideData = sideData.base(base))
    }
    def resolveUri(uri : URI) = {
      sideData.resolveUri(uri)
    }
}
    
final case class SideData(
    nextId : Long = 1L,
    refUriToId : Map[URI,String] = Map(),
    refIdToUri : Map[String,URI] = Map(),
    formulas : Map[String,Elem] = Map(),
    baseUri : Option[URI] = None) {
  def refId(prefix : String, ref : URI) : (SideData,String) = { 
    refUriToId.get(ref) match {
      case Some(id) => (this,id)
      case None =>
        val idnum = nextId
        val id = prefix + "_" + idnum.toString
        (copy(refUriToId = refUriToId + (ref -> id),
             refIdToUri = refIdToUri + (id -> ref),
             nextId = nextId + 1),id)
    }    
    
  }
  def base(base : URI) = {
    val bu = baseUri.map(_.resolve(base)).getOrElse(base)
    copy(baseUri = Some(bu))
  }
  def resolveUri(uri : URI) = {
    baseUri.map(_.resolve(uri)).getOrElse(uri)
  }

  def formulaId(prefix : String, elem : Elem) : (SideData,String) = {     
    val idnum = nextId
    val id = prefix + "_" + idnum.toString
    (copy(formulas = formulas + (id -> elem),
         nextId = nextId + 1),id)
  }
}
    
        
object DocxBodyRenderer {
  private [DocxBodyRenderer] object Renderer {
    type ParRenderer[A] = State[ParRendererState,A]
    
    type RunRenderer[A] = State[RunRendererState,A]    
   
    def mapM_[T,S](x : Seq[T])(f : T => State[S,_]) : State[S,Unit] = {
      if(x.isEmpty) { State.pure(()) } else {
        val (h,t) = (x.head,x.tail)
        for {
          _ <- f(h)
          _ <- mapM_(t)(f)
        } yield (())            
      }
    }
    
    implicit class MyRichState[S,A](x : State[S,A]) {
      def >>[B](y : State[S,B]) = x.flatMap(_ => y)
    }
    
    def unit[S] : State[S,Unit] = State.pure(())
    
    def par[T](pt : ParagraphType)(f : RunRenderer[T]) : ParRenderer[T] = {
      val style = StyleContext( 
              paragraphType = Some(pt), 
              parStyle = pt.parStyle,
              charStyle = pt.charStyle)
      parStyle(style)(f)    	
    }
    
    def optPar[T](pt : Option[ParagraphType] = None)(f : RunRenderer[T]) : ParRenderer[T] = {
      val style = StyleContext( 
              paragraphType = pt, 
              parStyle = pt.flatMap(_.parStyle),
              charStyle = pt.flatMap(_.charStyle))
      parStyle(style)(f)    	
    }
    
    def styled[T](f : StyleContext => StyleContext)(g : RunRenderer[T]) : RunRenderer[T] = State { rrs =>
      val rrs0 = rrs.copy(runs = Seq(), style = f(rrs.style))
      val (rrs1,v) = g.run(rrs0).value
      (rrs.copy(sideData = rrs1.sideData, runs = rrs.runs ++ rrs1.runs),v)    
    }
    
    def runStyle[T](s : StyleContext)(g : RunRenderer[T]) : RunRenderer[T] =
      styled(_ + s)(g)
      
    def parStyle[T](style : StyleContext = StyleContext())(g : RunRenderer[T]) : ParRenderer[T] = State { prs =>
      val g1 : RunRenderer[T] = for {
         _ <- if (prs.abreAspas) { run ("“") } else { unit[RunRendererState] }
         v <- g         
      } yield (v)
      val rrs0 = RunRendererState(
              style = style,
              sideData = prs.sideData)          
      val (rrs,res) = g1.run(rrs0).value
      val p = (<w:p>
				{ style.toPPr(prs.indented) }				
				{ rrs.runs }
				</w:p>)
  	  (prs.copy(sideData = rrs.sideData, pars = prs.pars :+ p, abreAspas = false, fechaAspas = false),res)		
    }
    
    def abreAspas = modifyPRS(_.setAbreAspas)
    def fechaAspas = modifyPRS(_.setFechaAspas)
    def clearAspas = modifyPRS(_.copy(abreAspas = false, fechaAspas = false))
    
    val abreAspasRun = (
        <w:r><w:rPr/><w:t>”</w:t></w:r>
        )
    
    val fechaAspasRun = (
        <w:r><w:rPr/><w:t>”</w:t></w:r>
        )
    
    
    def addFechaAspas : ParRenderer[Unit] = {      
      def add(e : Elem) = {
        e.copy(child = e.child :+ fechaAspasRun)                 
      }
      State { (prs : ParRendererState) => prs.pars.lastOption match {
        case Some(x : Elem) => (prs.copy(pars = prs.pars.init :+ add(x)),())
        case _ => (prs,())          
        }                             
      }
    }
    
    def wrapped[T](f : RunRenderer[T])(g : NodeSeq => Node) : RunRenderer[T] = State { rrs =>
      val rrs0 = rrs.copy(runs = Seq())
      val (rrs1,v) = f.run(rrs0).value
      (rrs0.copy(sideData = rrs1.sideData, runs = rrs.runs :+ g(rrs1.runs)),v)
    }
    
    def addRun(n : Node) : RunRenderer[Unit] =  State { rrs =>    
      (rrs.copy(runs = rrs.runs :+ n),())
    }
          
    def run(txt : String) : RunRenderer[Unit] = for {
      rrs <- getRRS
      val el = (
          <w:r>
				{rrs.style.toRPr}
				<w:t xml:space="preserve">{txt}</w:t>
				</w:r>
          )
      _ <- addRun(el)
    } yield (())
    
    def withBase[T](uri : URI)(f : RunRenderer[T]) : RunRenderer[T] = State { rrs =>
      f.run(rrs.base(uri)).value
    }
    
    def inspectRRS[T](f : RunRendererState => T) = State.inspect(f)
    
    def modifyRRS(f : RunRendererState => RunRendererState) = State.modify(f)
    
    val getRRS = State.get[RunRendererState]
    
    def setRRS(x : RunRendererState) = State.set(x)
    
    def inspectPRS[T](f : ParRendererState => T) = State.inspect(f)
    
    def modifyPRS(f : ParRendererState => ParRendererState) = State.modify(f)
    
    val getPRS = State.get[ParRendererState]
    
    def setPRS(x : ParRendererState) = State.set(x)
    
    def parWithBase[T](b : URI)(f : ParRenderer[T]) : ParRenderer[T] = State { prs =>
      val b0 = prs.sideData.baseUri    
      val (prs1,res) = f.run(prs.base(b)).value
      val prs2 = prs1.copy(sideData = prs1.sideData.copy(baseUri = b0))    
      (prs2,res)
    }
    
    def parWithBaseOpt[T](b : Option[URI])(f : ParRenderer[T]) : ParRenderer[T] = 
      b.map(x => parWithBase(x)(f)).getOrElse(f)
    
    
    def boldened[T](f : RunRenderer[T]) : RunRenderer[T] = styled(_.bold)(f)
    
    def italicized[T](f : RunRenderer[T]) : RunRenderer[T] = styled(_.italics)(f)
    
    def inSuperscript[T](f : RunRenderer[T]) : RunRenderer[T] = styled(_.sup)(f)
  
    def inSubscript[T](f : RunRenderer[T]) : RunRenderer[T] = styled(_.sub)(f)
    
    def inLang[T](lang : String)(f : RunRenderer[T]) : RunRenderer[T] = for {
      l <- inspectRRS(_.lang)
      _ <- modifyRRS(_.copy(lang = Some(lang)))
      v <- f
      _ <- modifyRRS(_.copy(lang = l))
    } yield (v)
    
    def omissisRun = addRun (
        <w:tab w:val="right" w:pos="8640" w:leader="dot"/>
        )
    
    def ref[T](uri : URI)(f : String => RunRenderer[T]) : RunRenderer[T] = 
      State { rrs =>
        val uri1 = rrs.resolveUri(uri)
        val (sd1,id) = rrs.sideData.refId("ref",uri1)           
        f(id).run(rrs.copy(sideData = sd1)).value
      }
    
    def form[T](elem : Elem)(f : String => RunRenderer[T]) : RunRenderer[T] = 
      State { rrs =>      
        val (sd1,id) = rrs.sideData.formulaId("formula",elem)           
        f(id).run(rrs.copy(sideData = sd1)).value
      }
    
    def hyperlink[T](href : URI)(f : RunRenderer[T]) : RunRenderer[T] = ref(href) { id =>
        wrapped(f) { rs =>
          <w:hyperlink r:id={id}>
      	{rs}
				</w:hyperlink>
        }  
    }
    
    def render(doc : LexmlDocument) : ParRenderer[Unit] = render(doc.contents)
    
    def render(contents : DocumentContents) : ParRenderer[Unit] = contents match {
      case n : Norma => render(n)
      case pj : ProjetoNorma => render(pj)
      case x => sys.error("Não suportado: " + x)
    }
    
    implicit class RichOption1[T](v : Option[T]) {
      def ifDef[A](f : T => State[A,Q] forSome { type Q }) : State[A,Unit] = v match {
        case None => State { (x : A) => (x,()) }
        case Some(x) => State { st =>
          val (st1,_) = f(x).run(st).value
          (st1,())
        }
      }
    }
        
    def render(pj : ProjetoNorma) : ParRenderer[Unit] = render(pj.norma)
    
    def render(norma : Norma) : ParRenderer[Unit] = render(norma.contents)
    
    def render(hs : HierarchicalStructure) : ParRenderer[Unit] = {
      for {
        _ <- hs.formulaPromulgacao.ifDef(render)
        _ <- hs.epigrafe.ifDef(render)
        _ <- hs.ementa.ifDef(render)
        _ <- hs.preambulo.ifDef(render)
        _ <- render(hs.articulacao)      
      } yield (())
    }
    
    def render(f : FormulaPromulgacao) : ParRenderer[Unit] = 
      par(PT_FORMULA_PROMULGACAO)(render(f.inlineSeq))
    
    def render(il : InlineSeq) : RunRenderer[Unit] = {
      val x = render(il.mixedElems)
      il.lang.map(l => inLang(l.code)(x)).getOrElse(x)    
    }
    
    def render(m : Mixed[InlineElement]) : RunRenderer[Unit] = render[InlineElement](m,render)
    
    def render[T](m : Mixed[T], f : T => RunRenderer[Unit]) : RunRenderer[Unit] = for {
      elems <- State.pure(m.elems.toList)
      _ <- mapM_(elems)(x => x.fold(f,run))      
    } yield (())
    
    def render(ie : InlineElement) : RunRenderer[Unit] = ie match {
      case ie : LXInlineElement => render(ie)
      case hie: HTMLinlineElement => render(hie)
      case x => sys.error(s"render(InlineElement): não suportado: ${x}")
    }
    
    def render(e : LXInlineElement) : RunRenderer[Unit] = e match {
      case x : Remissao => render(x)
      case x : RemissaoMultipla => render(x)
      case x : Alteracao => render(x)
      case x => sys.error(s"render(LXInlineElement): não suportado: ${x}")
    }
    
    def render(hie : HTMLinlineElement) : RunRenderer[Unit] = hie match {
      case x : Anchor => render(x)
      case x : Span => render(x)
      case x : GenHtmlInlineElement => render(x)    
    }
    
    def render(ge : GenHtmlInlineElement) : RunRenderer[Unit] = ge.tipoHtmlInlineElement match {
      case TGHIE_B => boldened(render(ge.inlineSeq))
      case TGHIE_I => italicized(render(ge.inlineSeq))
      case TGHIE_Sub => inSubscript(render(ge.inlineSeq))
      case TGHIE_Sup => inSuperscript(render(ge.inlineSeq))
      case TGHIE_Ins => sys.error("render(GenHtmlInlineElement): não suportado: " + ge)
      case TGHIE_Del => sys.error("render(GenHtmlInlineElement): não suportado: " + ge)
      case TGHIE_Dfn => sys.error("render(GenHtmlInlineElement): não suportado: " + ge)
    }    
    
    def render(s : Span) : RunRenderer[Unit] = {
      val remissao = Remissao(href=s.href,inlineSeq=s.inlineSeq)
      render(remissao)    
    }
    
    
    def renderAnchor(a : Anchor) :  RunRenderer[Unit] = {
      sys.error(s"renderAnchor: não suportado: ${a}")    
    }
    
    //Incomplete from here
      
    def render(e : Epigrafe) : ParRenderer[Unit] = 
      par(PT_EPIGRAFE)(render(e.inlineSeq))
    
    
    def render(e : Ementa) : ParRenderer[Unit] = 
        par(PT_EMENTA)(render(e.inlineSeq))
    
    def render(pb : Preambulo) : ParRenderer[Unit] =
        mapM_(pb.inlineSeqs)(render)
    
    def render(pl : PreambuloLine) : ParRenderer[Unit] =
      par(PT_PREAMBULO)(render(pl.inlineSeq))
        
    def render(a : Articulacao) : ParRenderer[Unit] = mapM_(a.elems)(render)    
    
        
    def render(r : Remissao) : RunRenderer[Unit] = for {
      href <- inspectRRS(_.resolveUri(r.href.uri))
      _ <- hyperlink(href)(render(r.inlineSeq))
    } yield (())
        
    def render(r : RemissaoMultipla) : RunRenderer[Unit] = State { rss =>
      val rss0 = rss.base(r.base.uri)
      val b0 = rss.sideData.baseUri
      val (rss1,_) = render(r.inlineSeq).run(rss0).value
      val sd1 = rss1.sideData.copy(baseUri = b0)
      (rss1.copy(sideData = sd1),())
    } 
    
    def render(he : HierarchicalElement) : ParRenderer[Unit] = he match {
      case x : Agrupador => render(x)
      case x : Artigo => render(x)
      case x : Omissis => render(x)    
    }
    
    def render(ag : Agrupador) : ParRenderer[Unit] = ag match {
      case x : AgrupadorPredef => render(x)
      case x : AgrupadorGenerico => render(x)
    }
    
    final case class AgrupadorParTypes(
        rotulo : ParagraphType,
        nomeAgrupador : ParagraphType)
    
    def agParType(t : TipoAgrupadorPredef) : AgrupadorParTypes = t match {
      case TAP_Parte => AgrupadorParTypes(PT_PARTE_ROTULO,PT_PARTE_NOME)
      case TAP_Livro => AgrupadorParTypes(PT_LIVRO_ROTULO,PT_LIVRO_NOME)
      case TAP_Titulo => AgrupadorParTypes(PT_TITULO_ROTULO,PT_TITULO_NOME)
      case TAP_Capitulo => AgrupadorParTypes(PT_CAPITULO_ROTULO,PT_CAPITULO_NOME)
      case TAP_Secao => AgrupadorParTypes(PT_SECAO_ROTULO,PT_SECAO_NOME)
      case TAP_Subsecao => AgrupadorParTypes(PT_SUBSECAO_ROTULO,PT_SUBSECAO_NOME)     
    }
    
    def render(ag : AgrupadorPredef) : ParRenderer[Unit] = {
      val apt = agParType(ag.tipoAgrupador)
      for {    
        _ <- ag.nomeAgrupador.ifDef(x => par(apt.nomeAgrupador)(render(x.inlineSeq)))
        _ <- ag.rotulo.ifDef(x => par(apt.rotulo)(run(x.rotulo)))
        _ <- mapM_(ag.elems)(render)
      } yield (())
    }
        
    def render(ag : AgrupadorGenerico) : ParRenderer[Unit] = 
      sys.error("render(AgrupadorGenerico): não suportado: " + ag)
    
    def render(x : Omissis) : ParRenderer[Unit] = par(PT_OMISSIS)(omissis)
    
    def omissis : RunRenderer[Unit] = run("...(omissis)...")
      
    
    def render(r : Rotulo, pt : ParagraphType) : RunRenderer[Unit] = 
      styled(x => x.copy(charStyle = pt.charStyle.orElse(x.charStyle)))(run(r.rotulo) >> run(" "))
     
     
      
    def render(a : Artigo) : ParRenderer[Unit] = {     
      val rotulo = a.rotulo.ifDef(x => render(x,PT_ARTIGO_ROTULO))
      def selectFirst : PartialFunction[ConteudoDispositivo,(RunRenderer[Unit],Boolean)] =  {
        case t : TextoDispositivo => (render(t.inlineSeqs.head.inlineSeq),true) : (RunRenderer[Unit],Boolean) 
        case OmissisSimples => (run("..."),true) : (RunRenderer[Unit],Boolean)       
      }      
      val (conteudo,skipFirst) : (RunRenderer[Unit],Boolean) = a.containers.head match {      
          case d : DispositivoPredefNA if d.tipoDispositivo == TDP_Caput => 
            d.conteudo.collect(selectFirst).getOrElse(State.pure(()),false)
          case _ => (State.pure(()),false)
      }
      val head = par(PT_ARTIGO) {
        rotulo >> conteudo
      }
      for {
        _ <- head
        _ <- a.containers.headOption.ifDef(x => render(x,skipFirst))
        _ <- mapM_(a.containers.tail){x => render(x,false) }
      } yield (())
    }
    
    def render(lc : LXContainer,skipFirst : Boolean) : ParRenderer[Unit] = lc match {
      case d : Dispositivo => render(d,skipFirst)
      case o : Omissis => if (skipFirst) { State.pure(()) } else { render(o) }
      case _ => sys.error("render(LXContainer): não suportado: " + lc)
    }
    
    def render(d : Dispositivo, skipFirst : Boolean) : ParRenderer[Unit] = d match {
      case dg : DispositivoGenerico => render(dg,skipFirst)
      case dn : DispositivoPredefNA => render(dn,skipFirst)
      case a : Artigo => render(a)
    }
      
    def render(dg : DispositivoGenerico, skipFirst : Boolean) : ParRenderer[Unit] =
      sys.error("render(DispositivoGenerico): não suportado: " + dg)
          
    def parTypesByDispType(d : TipoDispositivo) : (Option[ParagraphType],Option[ParagraphType],ParagraphType) = d match {
      case TDP_Caput => (None,None,PT_CAPUT_CONTENT)
      case TDP_Paragrafo => (Some(PT_PAR),Some(PT_PAR_ROTULO),PT_PAR_CONTENT)
      case TDP_Inciso => (Some(PT_INC),Some(PT_INC_ROTULO),PT_INC_CONTENT)
      case TDP_Alinea => (Some(PT_ALINEA),Some(PT_ALINEA_ROTULO),PT_ALINEA_CONTENT)
      case TDP_Item => (Some(PT_ITEM),Some(PT_ITEM_ROTULO),PT_ITEM_CONTENT)
      case TDP_Pena => (Some(PT_PENA),Some(PT_PENA_ROTULO),PT_PENA_CONTENT)
      case _ => sys.error("parTypeByDispType: unexpected: " + d)
    }
      
    def render(d : DispositivoPredefNA,skipFirst : Boolean) : ParRenderer[Unit] = {
      val (pt,ptRotulo,ptContent) = parTypesByDispType(d.tipoDispositivo)
      val rStyle = StyleContext(charStyle = ptRotulo.flatMap(_.charStyle))
      val cStyle = StyleContext(charStyle = ptContent.charStyle)
      val (firstInlineSeq,restInlineSeqs) : (RunRenderer[Unit],Seq[InlineSeq]) = d.conteudo match {
        case None => (unit[RunRendererState],List())
        case Some(t : TextoDispositivo) => (
            t.inlineSeqs.map(_.inlineSeq).headOption.ifDef(x => runStyle(cStyle)(render(x))),
            t.inlineSeqs.tail.map(_.inlineSeq).to[List])
        case Some(OmissisSimples) => (
            omissisRun,
            List())       
      }       
      val (head,tail) : (ParRenderer[Unit],Seq[InlineSeq]) = if(!skipFirst) {        
        val r1 = d.rotulo.ifDef(x => runStyle(rStyle)(run(x.rotulo) >> run(" ")))
        val c1 = firstInlineSeq
        val l1 = optPar(pt)(r1 >> c1)
        (l1,restInlineSeqs)
      } else {
        (State.pure(()),restInlineSeqs)
      }
      def rendPar(p : InlineSeq) : ParRenderer[Unit] = optPar(pt)(runStyle(cStyle)(render(p)))
      val tail1 : ParRenderer[Unit] = mapM_(tail)(rendPar)
      val firstPart : ParRenderer[Unit] = 
        head >> mapM_(tail)(rendPar) >> (if (d.fechaAspas) { addFechaAspas } else { unit })    
       
      for {      
        _ <- firstPart
        _ <- d.alteracao.ifDef(render)
        _ <- mapM_(d.containers){x => render(x,false) }
      } yield (())        
    }    
    
    def renderDispHead(pt : ParagraphType, rotuloStyle : Option[String], r : Option[Rotulo], conteudo : Option[RunRenderer[Unit]]) : 
      ParRenderer[Unit] = {
      val runRenderer  : RunRenderer[Unit] = for { 
        _ <- r.ifDef(rot => styled(x => x.copy(charStyle = rotuloStyle orElse x.charStyle))(run(rot.rotulo)))
        _ <- conteudo.ifDef(x => x)
      } yield (())
      par(pt)(runRenderer)    
    }    
    
    /*
    def render(cd : ConteudoDispositivo) : RunRenderer[Unit] = cd match {
      case t : TextoDispositivo => render(t.inlineSeqs)
      case OmissisSimples => run("...(omissis)...")
    } */
    
    def render(a : Alteracao) : ParRenderer[Unit] = 
      parWithBaseOpt (a.base.map(_.uri)) {
        indentAlteracao {
          mapM_(a.mixedElems.elems) { 
            case Right(txt) => par(PT_ALTERACAO)(run(txt))
            case Left(a) => render(a)
          }
        }      
      }
    
    def indentAlteracao(f : ParRenderer[Unit]) : ParRenderer[Unit] = State { prs =>
      val p0 = prs.indented    
      val (prs1,res) = f.run(prs.indent).value
      (prs1.copy(indented = p0),res) 
    }
    
    def render(a : AlteracaoElement) : ParRenderer[Unit] = {            
      val inside = a match {      
        case x : BlockElement => render(x)
        case x : Container => render(x)
        case x : FormulaPromulgacao => render(x)
        case x : Epigrafe => render(x)
        case x : Ementa => render(x)
        case x : Preambulo => render(x)
        case x : HierarchicalElement => render(x)
        case x : LXContainer => render(x,false)
        case _ => sys.error("render(AlteracaoElement): elemento não suportado: " + a)
      }
      println(s"AlteracaoElement: abreAspas: ${a.abreAspas}, fechaAspas: ${a.fechaAspas}, element: ${a}")
      for {
        _ <- if(a.abreAspas) { abreAspas } else { unit[ParRendererState]  }        
        _ <- inside
        _ <- if(a.fechaAspas) { addFechaAspas } else { unit[ParRendererState]  }        
        _ <- clearAspas
      } yield (())      
    }
    
    def render(b : BlockElement) : ParRenderer[Unit] = b match {    
      case x : HTMLBlock => render(x)
      case _ => sys.error("render(BlockElement): não suportado: " + b)
    }
    
    def render(b : HTMLBlock) : ParRenderer[Unit] = b match {
      case x : Paragraph => render(x)
      case x : HTMLList => render(x)
      case x : Table => render(x)
      case _ => sys.error("render(HTMLBlock): não suportado: " + b)
    }
    
    def render(p : Paragraph) : ParRenderer[Unit] = 
      parStyle(StyleContext())(render(p.inlineSeq))
    
    def render(hl : HTMLList) : ParRenderer[Unit] = 
      sys.error("render(HTMLList): não suportado: " + hl)
    
    def render(t : Table) : ParRenderer[Unit] = 
      sys.error("render(Table): não suportado: " + t)
    
    def render(c : Container) : ParRenderer[Unit] = c match {
      case _ => sys.error("render(Container): não suportado: " + c)    
    }
  }
  
  def renderLexmlDocument(doc : LexmlDocument) = {
    val (prs,_) = Renderer.render(doc).run(ParRendererState()).value
    DocxBodyRenderResult(
        NodeSeq.fromSeq(prs.pars),prs.sideData.formulas,prs.sideData.refIdToUri)
  }    
}
      
final case class DocxBodyRenderResult(
    body : NodeSeq,
    formulas : Map[String,Elem],
    refs : Map[String,URI])
    
object DocxMainPartRenderer {
  
  lazy val fixedStyles = (
       <w:styles xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" mc:Ignorable="w14">
  <w:docDefaults>
    <w:rPrDefault>
      <w:rPr>
        <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri" w:eastAsia="Calibri" w:cs="" w:asciiTheme="minorHAnsi" w:cstheme="minorBidi" w:eastAsiaTheme="minorHAnsi" w:hAnsiTheme="minorHAnsi"/>
        <w:szCs w:val="22"/>
        <w:lang w:val="pt-BR" w:eastAsia="en-US" w:bidi="ar-SA"/>
      </w:rPr>
    </w:rPrDefault>
    <w:pPrDefault>
      <w:pPr/>
    </w:pPrDefault>
  </w:docDefaults>
  <w:latentStyles w:defLockedState="0" w:defUIPriority="99" w:defSemiHidden="0" w:defUnhideWhenUsed="0" w:defQFormat="0" w:count="371">
    <w:lsdException w:name="Normal" w:uiPriority="0" w:qFormat="1"/>    
    <w:lsdException w:name="Default Paragraph Font" w:uiPriority="1" w:semiHidden="1" w:unhideWhenUsed="1"/>            
  </w:latentStyles>
  <w:style w:type="paragraph" w:styleId="Normal" w:default="1">
    <w:name w:val="Normal"/>
    <w:qFormat/>
    <w:pPr>
      <w:widowControl/>
      <w:bidi w:val="0"/>
      <w:spacing w:lineRule="auto" w:line="259" w:before="0" w:after="160"/>
      <w:jc w:val="left"/>
    </w:pPr>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri" w:eastAsia="Calibri" w:cs="" w:asciiTheme="minorHAnsi" w:cstheme="minorBidi" w:eastAsiaTheme="minorHAnsi" w:hAnsiTheme="minorHAnsi"/>
      <w:color w:val="auto"/>
      <w:kern w:val="0"/>
      <w:sz w:val="22"/>
      <w:szCs w:val="22"/>
      <w:lang w:val="pt-BR" w:eastAsia="en-US" w:bidi="ar-SA"/>
    </w:rPr>
  </w:style>
  <w:style w:type="character" w:styleId="DefaultParagraphFont" w:default="1">
    <w:name w:val="Default Paragraph Font"/>
    <w:uiPriority w:val="1"/>
    <w:semiHidden/>
    <w:unhideWhenUsed/>
    <w:qFormat/>
    <w:rPr/>
  </w:style>
  
  <w:style w:type="character" w:styleId="PreambuloparChar" w:customStyle="1">
    <w:name w:val="preambulo_par Char"/>
    <w:basedOn w:val="DefaultParagraphFont"/>
    <w:link w:val="preambulopar"/>
    <w:qFormat/>
    
    <w:rPr/>
  </w:style>
  
  <w:style w:type="character" w:styleId="LivrorotuloChar" w:customStyle="1">
    <w:name w:val="livro_rotulo Char"/>
    <w:basedOn w:val="DefaultParagraphFont"/>
    <w:link w:val="livrorotulo"/>
    <w:qFormat/>
    
    <w:rPr/>
  </w:style>
  
  <w:style w:type="character" w:styleId="Artigorotulo" w:customStyle="1">
    <w:name w:val="artigo_rotulo"/>
    <w:basedOn w:val="DefaultParagraphFont"/>
    <w:uiPriority w:val="1"/>
    <w:qFormat/>
    
    <w:rPr/>
  </w:style>
 
  <w:style w:type="character" w:styleId="LivrorotuloaltChar" w:customStyle="1">
    <w:name w:val="livro_rotulo_alt Char"/>
    <w:basedOn w:val="LivrorotuloChar"/>
    <w:link w:val="livrorotuloalt"/>
    <w:qFormat/>
    
    <w:rPr/>
  </w:style>
  
  <w:style w:type="character" w:styleId="Artigorotuloalt" w:customStyle="1">
    <w:name w:val="artigo_rotulo_alt"/>
    <w:basedOn w:val="Artigorotulo"/>
    <w:uiPriority w:val="1"/>
    <w:qFormat/>
    
    <w:rPr/>
  </w:style>
  
  <w:style w:type="character" w:styleId="LivrotituloChar" w:customStyle="1">
    <w:name w:val="livro_titulo Char"/>
    <w:basedOn w:val="DefaultParagraphFont"/>
    <w:link w:val="livrotitulo"/>
    <w:qFormat/>
    
    <w:rPr/>
  </w:style>
  
  <w:style w:type="paragraph" w:styleId="Heading">
    <w:name w:val="Heading"/>
    <w:basedOn w:val="Normal"/>
    <w:next w:val="TextBody"/>
    <w:qFormat/>
    <w:pPr>
      <w:keepNext w:val="true"/>
      <w:spacing w:before="240" w:after="120"/>
    </w:pPr>
    <w:rPr>
      <w:rFonts w:ascii="Liberation Sans" w:hAnsi="Liberation Sans" w:eastAsia="Liberation Sans" w:cs="Liberation Sans"/>
      <w:sz w:val="28"/>
      <w:szCs w:val="28"/>
    </w:rPr>
  </w:style>
  <w:style w:type="paragraph" w:styleId="TextBody">
    <w:name w:val="Body Text"/>
    <w:basedOn w:val="Normal"/>
    <w:pPr>
      <w:spacing w:lineRule="auto" w:line="276" w:before="0" w:after="140"/>
    </w:pPr>
    <w:rPr/>
  </w:style>
  <w:style w:type="paragraph" w:styleId="List">
    <w:name w:val="List"/>
    <w:basedOn w:val="TextBody"/>
    <w:pPr/>
    <w:rPr/>
  </w:style>
  <w:style w:type="paragraph" w:styleId="Caption">
    <w:name w:val="Caption"/>
    <w:basedOn w:val="Normal"/>
    <w:qFormat/>
    <w:pPr>
      <w:suppressLineNumbers/>
      <w:spacing w:before="120" w:after="120"/>
    </w:pPr>
    <w:rPr>
      <w:i/>
      <w:iCs/>
      <w:sz w:val="24"/>
      <w:szCs w:val="24"/>
    </w:rPr>
  </w:style>
  <w:style w:type="paragraph" w:styleId="Index">
    <w:name w:val="Index"/>
    <w:basedOn w:val="Normal"/>
    <w:qFormat/>
    <w:pPr>
      <w:suppressLineNumbers/>
    </w:pPr>
    <w:rPr/>
  </w:style>
  <w:style w:type="paragraph" w:styleId="Preambulopar" w:customStyle="1">
    <w:name w:val="preambulo_par"/>
    <w:basedOn w:val="Normal"/>
    <w:link w:val="preambuloparChar"/>
    <w:qFormat/>
    
    <w:pPr/>
    <w:rPr/>
  </w:style>
  <w:style w:type="paragraph" w:styleId="Livrorotulo" w:customStyle="1">
    <w:name w:val="livro_rotulo"/>
    <w:basedOn w:val="Normal"/>
    <w:next w:val="Normal"/>
    <w:link w:val="livrorotuloChar"/>
    <w:qFormat/>
    
    <w:pPr/>
    <w:rPr/>
  </w:style>
  <w:style w:type="paragraph" w:styleId="Livrorotuloalt" w:customStyle="1">
    <w:name w:val="livro_rotulo_alt"/>
    <w:basedOn w:val="Livrorotulo"/>
    <w:next w:val="Normal"/>
    <w:link w:val="livrorotuloaltChar"/>
    <w:qFormat/>
    
    <w:pPr/>
    <w:rPr/>
  </w:style>
  <w:style w:type="paragraph" w:styleId="Livrotitulo" w:customStyle="1">
    <w:name w:val="livro_titulo"/>
    <w:basedOn w:val="Normal"/>
    <w:link w:val="livrotituloChar"/>
    <w:qFormat/>
    
    <w:pPr/>
    <w:rPr/>
  </w:style>
  <w:style w:type="numbering" w:styleId="NoList" w:default="1">
    <w:name w:val="No List"/>
    <w:uiPriority w:val="99"/>
    <w:semiHidden/>
    <w:unhideWhenUsed/>
    <w:qFormat/>
  </w:style>
  <w:style w:type="table" w:default="1" w:styleId="Tabelanormal">
    <w:name w:val="Normal Table"/>
    <w:uiPriority w:val="99"/>
    <w:semiHidden/>
    <w:unhideWhenUsed/>
    <w:tblPr>
      <w:tblInd w:w="0" w:type="dxa"/>
      <w:tblCellMar>
        <w:top w:w="0" w:type="dxa"/>
        <w:left w:w="108" w:type="dxa"/>
        <w:bottom w:w="0" w:type="dxa"/>
        <w:right w:w="108" w:type="dxa"/>
      </w:tblCellMar>
    </w:tblPr>
  </w:style>
</w:styles>       

  )
  
  def basicStyle(typ: String, name : String, id : String, basedOn : Option[String] = None,
      pPr : Option[PPr] = None,rPr : Option[RPr] = None, link : Option[String] = None,
      uiPriority : Option[Int] = None) = {
    <w:style w:type={typ} w:styleId={id} w:customStyle="1">
    <w:name w:val={name}/>
    {basedOn.map(x => <w:basedOn w:val={x}/>).getOrElse(NodeSeq.Empty)}
    {link.map(x => <w:link w:val={x}/>).getOrElse(NodeSeq.Empty)}    
    {uiPriority.map(x => <w:uiPriority w:val={x.toString}/>).getOrElse(NodeSeq.Empty)}
    <w:qFormat/>    
    {pPr.map(_.asXML).getOrElse(NodeSeq.Empty)}
    {rPr.map(_.asXML).getOrElse(NodeSeq.Empty)}
    <w:rPr/>
  </w:style>
  }
  
  def basicParStyle(name : String, id : String, basedOn : Option[String] = None,
      pPr : Option[PPr] = None, link : Option[String] = None,
      uiPriority : Option[Int] = None) =
        basicStyle("paragraph",name,id,basedOn,pPr,None,link,uiPriority)
  
  def basicCharStyle(name : String, id : String, basedOn : Option[String] = None,
      rPr : Option[RPr] = None, link : Option[String] = None,
      uiPriority : Option[Int] = None) =
        basicStyle("character",name,id,basedOn,None,rPr,link,uiPriority)
  
  def basicLinkedStyles(nameSuffix : String, idSuffix : String, basedOnPar : Option[String] = None,
      basedOnChar : Option[String] = None, pPr : Option[PPr] = None,
      rPr : Option[RPr] = None, uiPriority : Option[Int] = None) = {
    val parId = "par_" + idSuffix
    val charId = "char_" + idSuffix
    (parId -> basicParStyle("par" + nameSuffix, parId,basedOnPar,pPr,Some(charId),uiPriority),
     charId -> basicCharStyle("char" + nameSuffix, charId,basedOnChar,rPr,Some(parId),uiPriority))
  }            
  
  
            
  def render(doc : LexmlDocument) = {
    val bodyRes = DocxBodyRenderer.renderLexmlDocument(doc)
    val body = bodyRes.body
    val mainDocElem = (
      <w:document xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" mc:Ignorable="w14 wp14">    
			<w:body>
			  {body}
				<w:sectPr>
	  	    <w:type w:val="nextPage"/>
  	    	<w:pgSz w:w="11906" w:h="16838"/>
	      	<w:pgMar w:left="1701" w:right="1701" w:header="0" w:top="1417" w:footer="0" w:bottom="1417" w:gutter="0"/>
      		<w:pgNumType w:fmt="decimal"/>
    	  	<w:formProt w:val="false"/>
  	    	<w:textDirection w:val="lrTb"/>
	      	<w:docGrid w:type="default" w:linePitch="360" w:charSpace="4096"/>
    		</w:sectPr>
  		</w:body>
	  	</w:document>)
	  val stylesElem = 
	    fixedStyles.copy(child = fixedStyles.child ++ ParagraphType.customStyles)
	      
	  (mainDocElem,stylesElem)	  	
  }
}