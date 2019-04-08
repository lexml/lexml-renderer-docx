package br.gov.lexml.renderer.docx.ver2

import br.gov.lexml.doc._
import scala.xml._
import java.net.URI


abstract sealed class ParagraphType(
    val parStyle : Option[String],
    val charStyle : Option[String]) extends Product

case object PT_FORMULA_PROMULGACAO 
  extends ParagraphType(
      parStyle = Some("parFormulaPromulgacao"),
      charStyle = Some("charFormulaPromulgacao"))

class DocxRendererOutput {
  def registerLexmlURN(urn : URI) : Option[String] = {
    None
  }
  def registerFormula(f : Formula) : Option[String] = {
    None
  }
  def registerComment(initial : String, msg : String) : Option[String] = {
    None
  }
  def outputXml(xml : Node) {
    //FIXME
  }
  abstract sealed class SubSup extends Product
  
  case object SS_Sub extends SubSup
  case object SS_Sup extends SubSup
  
  final case class Context(
      paragraphType : Option[ParagraphType] = None,
      parStyle : Option[String] = None,
      charStyle : Option[String] = None,
      lang : Option[String] = None,
      baseUri : Option[URI] = None,
      boldSet : Boolean = false,
      italicsSet : Boolean = false,
      subSup : Option[SubSup] = None) {
    
    def optLang(l : Option[Lang]) : Context =
      l.map(lang).getOrElse(this)
      
    def lang(l : Lang) : Context = 
      if(l.isDefault) {
        copy(lang = None)
      } else {
        copy(lang = Some(l.code))
      }
    lazy val rPr : Option[Elem] = {
      //FIXME
      Some(<w:rPr/>)
    }
    def base(base : URI) = {
      val bu = baseUri.map(_.resolve(base)).getOrElse(base)
      copy(baseUri = Some(bu))
    }
    def resolveUri(uri : URI) = {
      baseUri.map(_.resolve(uri)).getOrElse(uri)
    }
    def bold = { copy(boldSet = true) }
    def italics = { copy(italicsSet = true) }
    def sup = { copy(subSup = Some(SS_Sup)) }
    def sub = { copy(subSup = Some(SS_Sub)) }
  }
      
  def outputParagraph(parType : ParagraphType,inlineSeq : InlineSeq) {
    val parStyle = parType.parStyle
    val charStyle = parType.charStyle
    val ctx = Context(paragraphType = Some(parType),
        parStyle = parStyle,
        charStyle = charStyle)
    outputXml(renderParagraph(inlineSeq,ctx))
  }
  
  def renderParagraph(inlineSeq : InlineSeq,ctx : Context) = {            
    <w:p>
			{ ctx.parStyle.map { p =>
			    <w:pPr><w:pStyle w:val={p}></w:pStyle></w:pPr>
				}
			}
			{ renderInlineSeq(inlineSeq,ctx) }					
			</w:p>    		
  }
  
  def renderInlineSeq(inlineSeq : InlineSeq, ctx : Context) = {    
    renderMixed(inlineSeq.mixedElems,
        renderInlineElement, ctx.optLang(inlineSeq.lang))
  }
  
  def renderMixed[T](      
      melems : Mixed[T],
      tRenderer : (T,Context) => NodeSeq,
      ctx : Context) : NodeSeq = 
    NodeSeq.fromSeq(melems.elems.flatMap {
      case Right(x) => renderString(x,ctx)
      case Left(x) => tRenderer(x,ctx)
    })
  
  def renderString(txt : String, ctx : Context) = {
    val rPr : Option[Elem] = ctx.rPr
    (
        <w:r>
				{rPr.getOrElse(NodeSeq.Empty)}
				<w:t>{txt}</w:t>
				</w:r>
		)	  
  }
  
  def renderInlineElement(inl : InlineElement,ctx : Context) : NodeSeq = 
    inl match {
    case x : LXInlineElement => renderLXInlineElement(x,ctx)
    case el : EmLinha => renderEmLinha(el,ctx)    
    case nr : NotaReferenciada => renderNotaReferenciada(nr,ctx)
    case m : Marcador => renderMarcador(m,ctx)
    case im : Img => renderImg(im,ctx)
  }
  
  def renderLXInlineElement(inl : LXInlineElement,ctx : Context) : NodeSeq = {
    inl match {
      case r : Remissao => renderRemissao(r,ctx)
      case r : RemissaoMultipla => renderRemissaoMultipla(r,ctx)
      case x : Formula => renderFormula(x,ctx)      
    }
  }
  
  def renderRemissaoMultipla(r : RemissaoMultipla,ctx : Context) : NodeSeq = {
    renderInlineSeq(r.inlineSeq,ctx.base(r.base.uri))    
  }
  
  def renderRemissao(r : Remissao,ctx : Context) : NodeSeq = {
    val uri = ctx.resolveUri(r.href.uri) 
    lazy val inside = renderInlineSeq(r.inlineSeq,ctx)
    registerLexmlURN(uri) match {
      case Some(id) => (
        <w:hyperlink r:id={id}>
				{inside}
				</w:hyperlink>
      )
      case None => inside
    }
  }
  
  def renderFormula(f : Formula, ctx : Context) : NodeSeq = {
    registerFormula(f) match {
      case None => NodeSeq.Empty
      case Some(id) =>
        (<w:contentPart r:id={id}/>)
    }    
  }
  
  def renderComment(initials : String, msg : String, ctx : Context) = {
    registerComment(initials,msg) match {
      case None => NodeSeq.Empty
      case Some(id) =>
        <w:commentReference w:id={id}/>
    }
    
  }
  
  def renderUnsupported(x : Any, ctx : Context) : NodeSeq = {
    renderComment("compat","Elemento não suportado: " + x,ctx)
  }
  
  
  def renderEmLinha(el : EmLinha, ctx : Context) : NodeSeq = {
    renderUnsupported(el,ctx)
  }
      
  def renderNotaReferenciada(nr : NotaReferenciada, ctx : Context) : NodeSeq = {
    renderUnsupported(nr,ctx)
  }
  def renderMarcador(m : Marcador, ctx : Context) : NodeSeq = {
    renderUnsupported(m,ctx)
  }  
  def renderImg(img : Img, ctx : Context) : NodeSeq = {
    renderUnsupported(img,ctx)
  }
  
  def renderHTMLinlineElement(he : HTMLinlineElement, ctx : Context) : NodeSeq = he match {
    case x : Anchor => renderAnchor(x,ctx)
    case x : Span => renderSpan(x,ctx)
    case x : GenHtmlInlineElement => renderGenHtmlInlineElement(x,ctx)    
  }
  
  def renderAnchor(a : Anchor,ctx : Context) : NodeSeq = {
    renderUnsupported(a,ctx)    
  }
  
  def renderSpan(s : Span,ctx : Context) : NodeSeq = {
    val remissao = Remissao(href=s.href,inlineSeq=s.inlineSeq)
    renderRemissao(remissao,ctx)    
  }
  
  def renderGenHtmlInlineElement(ge : GenHtmlInlineElement, ctx : Context) = ge.tipoHtmlInlineElement match {
    case TGHIE_B => renderBold(ge,ctx)
    case TGHIE_I => renderItalics(ge,ctx)
    case TGHIE_Sub => renderSub(ge,ctx)
    case TGHIE_Sup => renderSup(ge,ctx)
    case TGHIE_Ins => renderIns(ge,ctx)
    case TGHIE_Del => renderDel(ge,ctx)
    case TGHIE_Dfn => renderDfn(ge,ctx)
  }
  
  def renderBold(ge : GenHtmlInlineElement, ctx : Context) : NodeSeq = {
    renderInlineSeq(ge.inlineSeq,ctx.bold)    
  }
  
  def renderItalics(ge : GenHtmlInlineElement, ctx : Context) : NodeSeq = {
    renderInlineSeq(ge.inlineSeq,ctx.italics)
  }
  
  def renderSub(ge : GenHtmlInlineElement, ctx : Context) : NodeSeq = {
    renderInlineSeq(ge.inlineSeq,ctx.sub)
  }
  
  def renderSup(ge : GenHtmlInlineElement, ctx : Context) : NodeSeq = {
    renderInlineSeq(ge.inlineSeq,ctx.sup)
  }
  
  def renderIns(ge : GenHtmlInlineElement, ctx : Context) : NodeSeq = {
    renderUnsupported(ge,ctx)
  }
  
  def renderDel(ge : GenHtmlInlineElement, ctx : Context) : NodeSeq = {
    renderUnsupported(ge,ctx)
  }
  
  def renderDfn(ge : GenHtmlInlineElement, ctx : Context) : NodeSeq = {
    renderUnsupported(ge,ctx)
  }
  
}

class DocxRenderer {
  val out = new DocxRendererOutput()
  
  def renderLexmlDocument(doc : LexmlDocument) = {
    renderDocumentContents(doc.contents)
  }
  
  def renderDocumentContents(contents : DocumentContents) =
    contents match {
    case n : Norma => renderNorma(n)
    case pj : ProjetoNorma => renderProjetoNorma(pj)
    case _ => sys.error("renderDocumentContents: unsupported: " + contents)
  }
      
  def renderProjetoNorma(pj : ProjetoNorma) = {
    renderAutorProjetoSeq(pj.autorProjeto)
    renderNorma(pj.norma)
    renderJustificacaoSeq(pj.justificacao)
  }
  
  def renderAutorProjetoSeq(autorProjetoSeq : Seq[String]) = {
    autorProjetoSeq.foreach(renderAutorProjeto)
  }
  
  def renderJustificacaoSeq(justificacaoSeq : Seq[Justificacao]) = {
    justificacaoSeq.foreach(renderJustificacao)
  }
  
  def renderAutorProjeto(autorProjeto : String) = {
    
  }
  
  def renderJustificacao(justificacao : Justificacao) {
    
  }
  
  def renderNorma(norma : Norma) = {
    renderHierarchicalStructure(norma.contents)  
  }
  
  def renderHierarchicalStructure(hs : HierarchicalStructure) = {
    hs.formulaPromulgacao.foreach(renderFormulaPromulgacao)
    hs.epigrafe.foreach(renderEpigrafe)
    hs.ementa.foreach(renderEmenta)
    hs.preambulo.foreach(renderPreambulo)
    renderArticulacao(hs.articulacao)
  }
  
  def renderFormulaPromulgacao(fp : FormulaPromulgacao) = {
    outputParagraph(PT_FORMULA_PROMULGACAO,fp.inlineSeq)
  }
  
  def renderEpigrafe(ep : Epigrafe) = {
    
  }
  
  def renderEmenta(em : Ementa) = {
    
  }
  
  def renderPreambulo(pb : Preambulo) = {
    
  }    
  
  def renderArticulacao(ar : Articulacao) = {
    
  }
  
  
}