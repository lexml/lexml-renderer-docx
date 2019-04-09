package br.gov.lexml.renderer.docx.ver3

import br.gov.lexml.doc._
import scala.xml._
import java.net.URI
import cats._
import cats.implicits._
import cats.data._


abstract sealed class ParagraphType(
    val parStyle : Option[String],
    val charStyle : Option[String]) extends Product

case object PT_FORMULA_PROMULGACAO 
  extends ParagraphType(
      parStyle = Some("parFormulaPromulgacao"),
      charStyle = Some("charFormulaPromulgacao"))

case object PT_EPIGRAFE 
  extends ParagraphType(
      parStyle = Some("parEpigrafe"),
      charStyle = Some("charEpigrafe"))

case object PT_EMENTA 
  extends ParagraphType(
      parStyle = Some("parEmenta"),
      charStyle = Some("charEmenta"))

case object PT_PREAMBULO 
  extends ParagraphType(
      parStyle = Some("parPreambulo"),
      charStyle = Some("charPreambulo"))

case object PT_PARTE_ROTULO
  extends ParagraphType(
      parStyle=Some("parParteRotulo"),
      charStyle=Some("charparteRotulo"))
case object PT_PARTE_NOME
  extends ParagraphType(
      parStyle=Some("parParteNome"),
      charStyle=Some("charparteNome"))
case object PT_LIVRO_ROTULO
  extends ParagraphType(
      parStyle=Some("parLivroRotulo"),
      charStyle=Some("charlivroRotulo"))
case object PT_LIVRO_NOME
  extends ParagraphType(
      parStyle=Some("parLivroNome"),
      charStyle=Some("charlivroNome"))
case object PT_TITULO_ROTULO
  extends ParagraphType(
      parStyle=Some("parTituloRotulo"),
      charStyle=Some("chartituloRotulo"))
case object PT_TITULO_NOME
  extends ParagraphType(
      parStyle=Some("parTituloNome"),
      charStyle=Some("charTituloNome"))
case object PT_CAPITULO_ROTULO
  extends ParagraphType(
      parStyle=Some("parCapituloRotulo"),
      charStyle=Some("charCapituloRotulo"))
case object PT_CAPITULO_NOME
  extends ParagraphType(
      parStyle=Some("parCapituloNome"),
      charStyle=Some("charCapituloNome"))
case object PT_SECAO_ROTULO
  extends ParagraphType(
      parStyle=Some("parSecaoRotulo"),
      charStyle=Some("charSecaoRotulo"))
case object PT_SECAO_NOME
  extends ParagraphType(
      parStyle=Some("parSecaoNome"),
      charStyle=Some("charSecaoNome"))
case object PT_SUBSECAO_ROTULO
  extends ParagraphType(
      parStyle=Some("parSubsecaoRotulo"),
      charStyle=Some("charSubsecaoRotulo"))
case object PT_SUBSECAO_NOME
  extends ParagraphType(
      parStyle=Some("parSubsecaoNome"),
      charStyle=Some("charSubsecaoNome"))
case object PT_OMISSIS
  extends ParagraphType(
      parStyle=Some("parOmissis"),
      charStyle=Some("charOmissis"))

case object PT_ARTIGO
  extends ParagraphType(
      parStyle=Some("parArtigo"),
      charStyle=Some("charArtigo"))

case object PT_TITULO_ARTIGO
  extends ParagraphType(
      parStyle=Some("parTituloArtigo"),
      charStyle=Some("charTituloArtigo"))


abstract sealed class SubSup extends Product
  
case object SS_Sub extends SubSup
case object SS_Sup extends SubSup

final case class StyleContext(
      paragraphType : Option[ParagraphType] = None,
      parStyle : Option[String] = None,
      charStyle : Option[String] = None,            
      boldSet : Boolean = false,
      italicsSet : Boolean = false,
      subSup : Option[SubSup] = None) {
  def toPPr : NodeSeq = NodeSeq.Empty
  def toRPr : NodeSeq = NodeSeq.Empty
  
  def bold = { copy(boldSet = true) }
  def italics = { copy(italicsSet = true) }
  def sup = { copy(subSup = Some(SS_Sup)) }
  def sub = { copy(subSup = Some(SS_Sub)) }
}

final case class ParRendererState(    
    pars : Seq[Elem] = Seq(),
    sideData : SideData = SideData())

    
    
final case class RunRendererState(
    style : StyleContext = StyleContext(),
    runs : Seq[Node] = Seq(),    
    sideData : SideData = SideData(),
    lang : Option[String] = None,
    baseUri : Option[URI] = None
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
      val bu = baseUri.map(_.resolve(base)).getOrElse(base)
      copy(baseUri = Some(bu))
    }
    def resolveUri(uri : URI) = {
      baseUri.map(_.resolve(uri)).getOrElse(uri)
    }
}
    
final case class SideData(
    nextId : Long = 1L,
    refUriToId : Map[URI,String] = Map(),
    refIdToUri : Map[String,URI] = Map(),
    formulas : Map[String,Elem] = Map()) {
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
  
  def formulaId(prefix : String, elem : Elem) : (SideData,String) = {     
    val idnum = nextId
    val id = prefix + "_" + idnum.toString
    (copy(formulas = formulas + (id -> elem),
         nextId = nextId + 1),id)
  }
}
    
        
trait RendererStateDsl {
  type ParRenderer[A] = State[ParRendererState,A]
  
  type RunRenderer[A] = State[RunRendererState,A]
         
  def par[T](pt : ParagraphType)(f : RunRenderer[T]) : ParRenderer[T] = State { prs =>
    val style = StyleContext(
            paragraphType = Some(pt), 
            parStyle = pt.parStyle,
            charStyle = pt.charStyle)
    val rrs0 = RunRendererState(
            style = style,
            sideData = prs.sideData)          
    val (rrs,res) = f.run(rrs0).value
    val p = (<w:p>
				{ style.toPPr }
				{ rrs.runs }
				</w:p>)
	  (prs.copy(sideData = rrs.sideData, pars = prs.pars :+ p),res)		
  }
  
  def styled[T](f : StyleContext => StyleContext)(g : RunRenderer[T]) : RunRenderer[T] = State { rrs =>
    val rrs0 = rrs.copy(runs = Seq(), style = f(rrs.style))
    val (rrs1,v) = g.run(rrs0).value
    (rrs0.copy(sideData = rrs1.sideData, runs = rrs0.runs ++ rrs1.runs),v)    
  }
  
  def wrapped[T](f : RunRenderer[T])(g : NodeSeq => Node) : RunRenderer[T] = State { rrs =>
    val rrs0 = rrs.copy(runs = Seq())
    val (rrs1,v) = f.run(rrs0).value
    (rrs0.copy(sideData = rrs1.sideData, runs = rrs0.runs :+ g(rrs1.runs)),v)
  }
  
  def run(txt : String) : RunRenderer[Unit] = State { rrs =>
    val el = (
        <w:r>
				{rrs.style.toRPr}
				<w:t>{txt}</w:t>
				</w:r>
        )
    (rrs.copy(runs = rrs.runs :+ el),())
  }
  
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
  
  def render[T](m : Mixed[T], f : T => RunRenderer[_]) : RunRenderer[Unit] = for {
    elems <- State.pure(m.elems.toList)
    _ <- elems.traverse(x => x.fold(x => f(x) flatMap { _ => State.pure(()) },run))      
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
      pb.inlineSeqs.to[List].traverse(render).flatMap{ _ => State.pure(()) }
  
  def render(pl : PreambuloLine) : ParRenderer[Unit] =
    par(PT_PREAMBULO)(render(pl.inlineSeq))
      
  def render(a : Articulacao) : ParRenderer[Unit] = for {
    _ <- a.elems.to[List].traverse(render)    
  } yield (())
      
  def render(r : Remissao) : RunRenderer[Unit] = for {
    href <- inspectRRS(_.resolveUri(r.href.uri))
    _ <- hyperlink(href)(render(r.inlineSeq))
  } yield (())
      
  def render(r : RemissaoMultipla) : RunRenderer[Unit] = State { rss =>
    val rss0 = rss.base(r.base.uri)
    val (rss1,_) = render(r.inlineSeq).run(rss0).value
    (rss1.copy(baseUri = rss0.baseUri),())
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
      _ <- ag.elems.to[List].traverse(render)
    } yield (())
  }
      
  def render(ag : AgrupadorGenerico) : ParRenderer[Unit] = 
    sys.error("render(AgrupadorGenerico): não suportado: " + ag)
  
  def render(x : Omissis) : ParRenderer[Unit] = par(PT_OMISSIS)(omissis)
  
  def omissis : RunRenderer[Unit] = run("...(omissis)...")
    
  
  def render(r : Rotulo, pt : ParagraphType) : RunRenderer[Unit] = 
    styled(x => x.copy(charStyle = pt.charStyle.orElse(x.charStyle)))(run(r.rotulo))
  
  def render(a : Artigo) : ParRenderer[Unit] = {
    val rotulo = a.rotulo.ifDef(x => render(x,PT_ARTIGO))
    val first : RunRenderer[Unit] = a.containers.head match {
      case  x : Omissis => run("...(omissis)...")
      case x : DispositivoPredefNA if x.tipoDisp == TDP_Caput =>
        renderDispWithRotulo(rotulo,x)        
      case x => sys.error("render(Artigo): primeiro sub-elemento não suportador: " + x)
    }
    for {  
      _ <- first       
      _ <- a.containers.to[List].tail.traverse(render)
    } yield (())
  }
  
  def renderDispWithRotulo(rotulo : RunRenderer[Unit], d : DispositivoPredefNA) = {
    par(
  }
  
  def render(lc : LXContainer) : ParRenderer[Unit] = ???
  
  
  def render(a : Alteracao) : RunRenderer[Unit] = ???
  
  
  
}
      
class DocxRenderer {
  
}