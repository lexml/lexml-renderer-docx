package br.gov.lexml.renderer.docx

import scala.xml._

object Namespaces {
  val xsd = "http://www.w3.org/2001/XMLSchema"
  val xml = "http://www.w3.org/XML/1998/namespace"
  val xlink = "http://www.w3.org/1999/xlink"
  val mathml = "http://www.w3.org/1998/Math/MathML"
}


trait LexmlXmlParsing {
  implicit class ElemExt(xml : Elem) {
    def subElem(f : Elem => scala.xml.NodeSeq) = f(xml).headOption.collect { case e : Elem => e }  
    def elem(label : String) = subElem(_ \ label).get
    def elemOpt(label : String) = subElem(_ \ label)  
    def elemChoiceOpt(labels : String*) : Option[Elem] = labels.flatMap(elemOpt).headOption
    def elemChoice(labels : String*) : Elem = elemChoiceOpt(labels : _*).get
    def elems(label : String) = (xml \ label).to[Seq].collect { case e : Elem => e }
    def elemsChoiceP(p : String => Boolean) = xml.child.to[Seq].collect { case e : Elem if p(e.label) => e }
    def elemsNS(ns : String, label : String) = (xml \ ("@{" + ns + "}" + label)).to[Seq].collect { case e : Elem => e }
    def optElemNS(ns : String, label : String) = elemsNS(ns,label).headOption
    def elemNS(ns : String, label : String) = optElemNS(ns,label).get
    def optAttr(label : String) = xml.attributes.get(label).headOption.map(_.headOption).flatten.map(_.text)
    def optAttrNS(uri : String, label : String) = xml.attributes.get(uri,xml,label).headOption.map(_.headOption).flatten.map(_.text)
    def attr(label : String) = optAttr(label).get
    def attrNS(uri : String, label : String) = optAttrNS(uri,label).get  
  }
}


abstract sealed class LexmlSchema(tipo : String) {
  override def toString() = tipo
}

case object LS_RIGIDO extends LexmlSchema("rigido")

case object LS_FLEXIVEL extends LexmlSchema("flexivel")

final case class LexmlXmlRenderParams(lexml_schema : LexmlSchema = LS_RIGIDO)

trait AttrData {
  def attrSeq : Seq[Option[((Option[String],String),String)]]  
}

trait LexmlRenderable {
  final def xml(label : Option[String] = None)(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = {
    val node = (label,xml_in(xml_params)) match {
      case (Some(x),e : Elem) => e.copy(label = x)
      case (_,x) => x
    }
    node match {
      case e : Elem =>
        val newAttr = attrData.flatMap(_.attrSeq).flatten.foldLeft(e.attributes){ 
          case (md,((Some(pref),label),value)) => new PrefixedAttribute(pref,label,value,md)
          case (md,((None,label),value)) => new UnprefixedAttribute(label,value,md)     
        }
        if(newAttr.isEmpty) { e } else { e.copy(attributes = newAttr) }
      case _ => node
    }
  }
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) : Node  
  def attrData : Seq[AttrData] = Seq()
}

final case class LexmlDocument(metadado : MetaSection, documentContent : DocumentContent) extends LexmlRenderable {
  def xml_in(label : Option[String] = None)(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (<LexML xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       xmlns:xlink="http://www.w3.org/1999/xlink"
       xmlns="http://www.lexml.gov.br/1.0"
       xmlns:math="http://www.ora.com/XSLTCookbook/math"
       xsi:schemaLocation={s"http://www.lexml.gov.br/1.0 http://projeto.lexml.gov.br/esquemas/lexml-br-${xml_params.lexml_schema}.xsd"}>
 { metadado.xml(Some("Metadado")) }
 { documentContent.xml() }
 </LexML>)
}
    
object LexmlDocument extends LexmlXmlParsing {
  def apply(xml : Elem) : LexmlDocument = {
   val metadado = MetaSection(xml.elem("Metadado"))
   val documentContent : DocumentContent =     
      DocumentContent(xml.elemChoice("Norma","ProjetoNorma","Jurisprudencia","DocumentoGenerico","Anexo"))
   LexmlDocument(metadado,documentContent)
  }
}

final case class MetaSection(
    identificacao : IdentificacaoType,
    cicloDeVida : Option[CicloDeVida] = None,
    eventosGerados : Option[EventosGerados] = None,
    notas : Seq[Notas] = Seq(),
    recursos : Option[Recursos] = None,
    metadadoProprietario : Seq[MetadadoProprietario] = Seq()
)  extends LexmlRenderable {
   def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
    <InvalidLabel>
			{identificacao.xml}
		  {cicloDeVida.map(_.xml).getOrElse(NodeSeq.Empty)}
		  {eventosGerados.map(_.xml).getOrElse(NodeSeq.Empty)}
		  {NodeSeq.fromSeq(notas.map(_.xml))}
		  {recursos.map(_.xml).getOrElse(NodeSeq.Empty)}
		  {NodeSeq.fromSeq(metadadoProprietario.map(_.xml))}		  
		</InvalidLabel>
   )
}

object MetaSection extends LexmlXmlParsing {
  def apply(xml : Elem) : MetaSection = {    
    val identificacao = IdentificacaoType(xml.elem("Identificacao"))
    println("identificao = " + identificacao)
    val cicloDeVida = xml.elemOpt("CicloDeVida").map{CicloDeVida(_)}
    val eventosGerados = xml.elemOpt("EventosGerados").map{EventosGerados(_)}
    val notas = xml.elems("Notas").map{Notas(_)}
    val recursos = xml.elemOpt("Recursos").map{Recursos(_)}
    val metadadoProprietario = xml.elems("MetadadoProprietario").map{MetadadoProprietario(_)}
    MetaSection(identificacao,cicloDeVida,eventosGerados,notas,recursos,metadadoProprietario)    
  }
}


abstract sealed class DocumentContent extends Product with LexmlRenderable

object DocumentContent {
  def apply(elem : Elem) : DocumentContent = elem.label match {
    case "Norma" => Norma(elem)
    case "ProjetoNorma" => ProjetoNorma(elem)
    case "Jurisprudencia" => Jurisprudencia(elem)
    case "Anexo" => Anexo(elem)
  }
}

final case class Norma(contents : HierarchicalStructure) extends DocumentContent {
   def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = 
     contents.xml(Some("Norma"))
}

object Norma {
  def apply(xml : Elem) : Norma = Norma(HierarchicalStructure(xml))
}

final case class HierarchicalStructure(
  articulacao : Articulacao,
  parteInicial : Option[ParteInicial] = None,    
  parteFinal : Option[ParteFinal] = None,
  anexos : Option[Anexos] = None,
  lang : String = "pt-BR"
) extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
      <InvalidLabel>
        {parteInicial.map(_.xml()).getOrElse(NodeSeq.Empty)}
				{articulacao.xml()}
				{parteFinal.map(_.xml()).getOrElse(NodeSeq.Empty)}
				{anexos.map(_.xml()).getOrElse(NodeSeq.Empty)}
		  </InvalidLabel>
      )
  private lazy val localAttrData = AG_lang(lang)
  def attrData = Seq(localAttrData)
}


object HierarchicalStructure extends LexmlXmlParsing {
  def apply(xml : Elem) : HierarchicalStructure = {
    val parteInicial = xml.elemOpt("ParteInicial").map{ParteInicial(_) }
    val articulacao = Articulacao(xml.elem("Articulacao"))
    val parteFinal = xml.elemOpt("ParteFinal").map{ ParteFinal(_) }
    val anexos = xml.elemOpt("Anexos").map { Anexos(_) }
    val lang = xml.optAttrNS(Namespaces.xml,"lang").getOrElse("pt-BR")
    HierarchicalStructure(articulacao,parteInicial,parteFinal,anexos,lang)
  }
}

final case class ProjetoNorma(
    norma : HierarchicalStructure,
    justificacao : Seq[OpenStructure] = Seq(),
    autorProjeto : Seq[String] = Seq()
)  extends DocumentContent {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
  <ProjetoNorma>
		{ norma.xml("Norma") }
		{ justificacao.map({_.xml("Justificacao")}).getOrElse(NodeSeq.Empty) }
		{ autorProjeto.map({_.xml()}).getOrElse(NodeSeq.Empty) }
	</ProjetoNorma>)
}

object ProjetoNorma extends LexmlXmlParsing {
  def apply(xml : Elem) : ProjetoNorma = {
     val norma = HierarchicalStructure(xml.elem("Norma"))
     val justificacao = xml.elems("Justificacao").map{OpenStructure(_)}
     val autorProjeto = xml.elems("AuthorProjeto").map(_.text)
     ProjetoNorma(norma,justificacao,autorProjeto)
  }
}

final case class Jurisprudencia(xml : Elem) extends DocumentContent {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = xml
}


final case class Anexo(anexoDoc : AnexoDoc) extends DocumentContent {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
      <Anexo>
				{anexoDoc.xml()}
				</Anexo>
      )
}

object Anexo extends LexmlXmlParsing {
  def apply(xml : Elem) : Anexo =  {
    val doc = xml.elemChoice("DocumentoArticulado","DocumentoGenerico") match {
      case x if x.label == "DocumentoArticulado" => AnexoDocumentoArticulado(x)
      case x => AnexoDocumentoGenerico(x)
    } 
    Anexo(doc)
  }
}

sealed trait AnexoDoc extends Product with LexmlRenderable

final case class AnexoDocumentoArticulado(doc : HierarchicalStructure) extends AnexoDoc {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = 
      doc.xml(Some("DocumentoArticulado"))
}

object AnexoDocumentoArticulado extends LexmlXmlParsing {
  def apply(xml : Elem) : AnexoDocumentoArticulado = 
    AnexoDocumentoArticulado(HierarchicalStructure(xml))      
}


final case class AnexoDocumentoGenerico(doc : OpenStructure) extends AnexoDoc {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) =
    doc.xml(Some("DocumentoGenerico"))
}

object AnexoDocumentoGenerico extends LexmlXmlParsing {
  def apply(xml : Elem) : AnexoDocumentoGenerico = 
    AnexoDocumentoGenerico(OpenStructure(xml))      
}

final case class ParteInicial(
  formulacaoPromulgacao : Option[textoSimplesType] = None,
  epigrafe : Option[inlineReq] = None 
)  extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
    <ParteInicial>
			{formulacaoPromulgacao.map{x => x.xml("FormulaPromulgacao")}.getOrElse(NodeSeq.Empty)}
			{epigrafe.map{x => x.xml("Epigrafe")}.getOrElse(NodeSeq.Empty)}
		</ParteInicial>
      )
}

object ParteInicial extends LexmlXmlParsing {
  def apply(xml : Elem) : ParteInicial = {
    val formulacaoPromulgacao = xml.elemOpt("FormulacaoPromulgacao").map{textoSimplesType(_)}
    val epigrafe = xml.elemOpt("Epigrafe").map{inlineReq(_)}
    ParteInicial(formulacaoPromulgacao,epigrafe)
  }
}

final case class Articulacao(coreopt : AG_coreopt = AG_coreopt(), elems : Seq[hierElement] = Seq()) extends LexmlRenderable  {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
      <Articulacao>
				{NodeSeq.fromSeq(elems.map(_.xml()))}
		  </Articulacao>)
	def attrData = Seq(coreopt)
}

object Articulacao extends LexmlXmlParsing {
  def apply(xml : Elem) : Articulacao = 
    Articulacao(AG_coreopt(xml),xml.elemsChoiceP(hierElement.labels).map{hierElement(_)})
}

/*final case class HTMLattrs(
    class_attr : Option[String] = None, 
    style : Option[String] = None,
    title : Option[String] = None)  extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
      )
}

object HTMLattrs extends LexmlXmlParsing {
  def apply(xml : Elem) : HTMLattrs = {
    val class_attr = xml.optAttr("class")
    val style = xml.optAttr("style")
    val title = xml.optAttr("title")
    HTMLattrs(class_attr,style,title)
  }
}
*/

final case class ParteFinal(assinatura : AssinaturaT,localDataFecho : Option[textoSimplesType] = None)  extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
    <ParteFinal>
			{localDataFecho.map{_.xml("LocalDataFecho")}.getOrElse(NodeSeq.Empty)}
			{assinatura.xml()}
		</ParteFinal>
      )
}

object ParteFinal extends LexmlXmlParsing {
  def apply(xml : Elem) : ParteFinal = {
    val localDataFecho = xml.elemOpt("LocalDataFecho").map{textoSimplesType(_)}
    val assinatura = ParteFinal.assinatura(xml.elemChoice("AssinaturaGrupo","Assinaturas","Assinatura"))
    ParteFinal(assinatura,localDataFecho)
  }
  def assinatura(xml : Elem) : AssinaturaT = xml.label match {
    case "AssinaturaGrupo" => AssinaturaGrupo(xml)
    case "Assinaturas" => Assinaturas(xml)
    case "Assinatura" => Assinatura(xml)
  }
}

trait AssinaturaT extends Product with LexmlRenderable

final case class Anexos(referenciasAnexo : Seq[refURN] = Seq()) extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
      <Anexos>
				{NodeSeq.fromSeq(referenciasAnexo.map(_.xml("ReferenciaAnexo")))}
			</Anexos>
      )
}

object Anexos extends LexmlXmlParsing {
  def apply(xml : Elem) : Anexos = 
    Anexos(xml.elems("ReferenciaAnexo").map(refURN(_)))  
}

final case class refURN(
    alvoURN : Option[String] = None, fonteURN : Option[String] = None,
    coreopt : AG_coreopt = AG_coreopt()) extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
    <InvalidLabel/>
      )
  private val localAttrData = new AttrData { 
    override def attrSeq = 
      Seq(alvoURN.map{x => ((None,"AlvoURN"),x)},
          fonteURN.map{x => ((None,"FonteURN"),x)}) ++ coreopt.attrSeq
  }
  def attrData = Seq(coreopt,localAttrData)
}

object refURN extends LexmlXmlParsing {
  def apply(xml : Elem) : refURN =
    refURN(xml.optAttr("AlvoURN"),xml.optAttr("FonteURN"),AG_coreopt(xml))  
}

final case class OpenStructure(   
    partePrincipal : Option[PartePrincipal] = None,
    anexos : Option[Anexos] = None,
    coreopt : AG_coreopt = AG_coreopt()) extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
    <InvalidLabel>
			{partePrincipal.map(_.xml()).getOrElse(NodeSeq.Empty)}
			{anexos.map(_.xml()).getOrElse(NodeSeq.Empty)}
		</InvalidLabel>
      )
  def attrData = Seq(coreopt)
}

object OpenStructure extends LexmlXmlParsing {
  def apply(xml : Elem) : OpenStructure = {
    val partePrincipal = xml.elemOpt("PartePrincipal").map{PartePrincipal(_)}
    val anexos = xml.elemOpt("Anexos").map{ Anexos(_) }
    OpenStructure(partePrincipal,anexos,AG_coreopt(xml))
  }
}

final case class textoSimplesType(corereq : AG_corereq, p : Option[inline] = None) extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
      <InvalidLabel>{p.map{_.xml("p")}.getOrElse(NodeSeq.Empty)}</InvalidLabel>
      )
  def attrData = Seq(corereq)
}

object textoSimplesType extends LexmlXmlParsing {
  def apply(xml : Elem) : textoSimplesType =
    textoSimplesType(AG_corereq(xml),xml.elemOpt("p").map{inline(_)})
}


final case class CicloDeVida(eventos : Seq[Evento] = Seq()) extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
    <CicloDeVida>
			{NodeSeq.fromSeq(eventos.map(_.xml()))}
		</CicloDeVida>
      )
}

object CicloDeVida extends LexmlXmlParsing {
  def apply(xml : Elem) : CicloDeVida =
    CicloDeVida(xml.elems("Evento").map{Evento(_)})
}

final case class EventosGerados(eventos : Seq[Evento] = Seq()) extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
    <EventosGerados>
			{NodeSeq.fromSeq(eventos.map(_.xml()))}
		</EventosGerados>
      )
}
      
object EventosGerados extends LexmlXmlParsing {
  def apply(xml : Elem) : EventosGerados =
    EventosGerados(xml.elems("Evento").map{Evento(_)})
}

final case class Notas(notas : Seq[Nota] = Seq()) extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
    <Notas>
			{NodeSeq.fromSeq(notas.map(_.xml()))}
		</Notas>
      )
}

object Notas extends LexmlXmlParsing {
  def apply(xml : Elem) : Notas =
    Notas(xml.elems("Nota").map{Nota(_)})
}

final case class Nota(
    texto : textoType,
    exporta : Option[String] = None,
    dataInclusao : Option[String] = None,
    autor : Option[String] = None) extends LexmlRenderable {  
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = texto.xml("Nota")
  private val localAttrData = new AttrData { 
    override def attrSeq = Seq(
        exporta.map{x => ((None,"exporta"),x)},
        dataInclusao.map{x => ((None,"dataInclusao"),x)},
        autor.map{x => ((None,"autor"),x)}
        )
  }
  def attrData = Seq(localAttrData)
  
}

object Nota extends LexmlXmlParsing {
  def apply(xml : Elem) : Nota = 
    Nota(textoType(xml),xml.optAttr("exporta"),
        xml.optAttr("dataInclusao"),
        xml.optAttr("autor"))
}

final case class Recursos(recursos : Seq[Recurso] = Seq())

object Recursos extends LexmlXmlParsing {
  def apply(xml : Elem) : Recursos =
    Recursos(xml.elems("Recurso").map{Recurso(_)})
}

final case class Recurso(corereq : AG_corereq) extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = 
      (<InvalidLabel/>)
  def attrData = Seq(corereq)  
}

object Recurso {
  def apply(xml : Elem) : Recurso = Recurso(AG_corereq(xml))
}

final case class MetadadoProprietario(source : AG_source,
    contents : NodeSeq = NodeSeq.Empty) extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
      <MetadadoProprietario>{contents}</MetadadoProprietario>)
  def attrData = Seq(source)
}
    
object MetadadoProprietario extends LexmlXmlParsing {
  def apply(xml : Elem) : MetadadoProprietario =
    MetadadoProprietario(AG_source(xml),xml.child)
}
    
final case class textoType(corereq : AG_corereq, pars : Seq[inline] = Seq()) extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
    <InvalidLabel>
		{NodeSeq.fromSeq(pars.map{_.xml("p")})}
    </InvalidLabel>  
      )
  def attrData = Seq(corereq)
}

object textoType extends LexmlXmlParsing {
  def apply(xml : Elem) : textoType =
    textoType(AG_corereq(xml),xml.elems("p").map{inline(_)})  
}

final case class inlineReq(
    corereq : AG_corereq,
    elems : Seq[inlineComponent] = Seq())  extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
      <InvalidLabel>
			{NodeSeq.fromSeq(elems.map{_.xml("p")})}
			</InvalidLabel>)
  def attrData = Seq(corereq)
}
    
object inlineReq  extends LexmlXmlParsing {
  def apply(xml : Elem) : inlineReq = {
   lazy val elems = xml.child.to[Seq].collect { 
     case e : Elem if inlineElement.labels(e.label) => inlineElement(e)
     case e : Elem if markerElement.labels(e.label) => markerElement(e)
     case t : scala.xml.Text => new InlineText(t.text)
   } 
   inlineReq(AG_corereq(xml),elems)
  }
}  

final case class inline(
    coreopt : AG_coreopt = AG_coreopt(),
    elems : Seq[inlineComponent] = Seq()) extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
      <InvalidLabel>
			{NodeSeq.fromSeq(elems.map{_.xml("p")})}
			</InvalidLabel>)
  def attrData = Seq(coreopt)
}
    
object inline  extends LexmlXmlParsing {
  def apply(xml : Elem) : inline = {
   lazy val elems : Seq[inlineComponent] = xml.child.to[Seq].collect { 
     case e : Elem if inlineElement.labels(e.label) => inlineElement(e)
     case e : Elem if markerElement.labels(e.label) => markerElement(e)
     case t : scala.xml.Text => new InlineText(t.text)
   } 
   inline(AG_coreopt(xml),elems)
  }
}
 
trait inlineComponent extends Product with LexmlRenderable 

/*final case class markerreq(corereq : AG_corereq)  extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
      ???
      )
}

object markerreq {
  def apply(xml : Elem) : markerreq = markerreq(AG_corereq(xml))  
}
*/
final case class AssinaturaGrupo(
    assinatura : AssinaturaGrupoChoice,
    nomeGrupo : Option[String] = None) extends AssinaturaT {
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
      <AssinaturaGrupo>
					{nomeGrupo.map{x => <NomeGrupo>{x}</NomeGrupo>}}
					{assinatura.xml()}
			</AssinaturaGrupo>
      )
}
    
object AssinaturaGrupo extends LexmlXmlParsing {
  def apply(xml : Elem) : AssinaturaGrupo = {
    val nomeGrupo = xml.elemOpt("NomeGrupo").map(_.text)
    val assinatura = AssinaturaGrupo.assinatura(xml.elemChoice("Assinaturas","Assinatura"))
    AssinaturaGrupo(assinatura,nomeGrupo)
  }
  def assinatura(xml : Elem) : AssinaturaGrupoChoice = xml.label match {    
    case "Assinaturas" => Assinaturas(xml)
    case "Assinatura" => Assinatura(xml)
  }
}

trait AssinaturaGrupoChoice extends AssinaturaT

final case class Assinaturas(assinaturas : Seq[Assinatura] = Seq()) extends AssinaturaGrupoChoice {
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
      <Assinaturas>{NodeSeq.fromSeq(assinaturas.map{_.xml()})}</Assinaturas>
      )
}

object Assinaturas extends LexmlXmlParsing {
  def apply(xml : Elem) : Assinaturas = 
    Assinaturas(xml.elems("Assinatura").map{Assinatura(_)})
}

final case class Assinatura(nomesPessoas : Seq[String] = Seq(),
    cargos : Seq[String] = Seq()) extends AssinaturaGrupoChoice {
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
      <Assinatura>
				{NodeSeq.fromSeq(nomesPessoas.map{x => <NomePessoa>{x}</NomePessoa>})}
				{NodeSeq.fromSeq(cargos.map{x => <Cargo>{x}</Cargo>})}
			</Assinatura>
      )
}
    
object Assinatura extends LexmlXmlParsing {
  def apply(xml : Elem) : Assinatura = 
    Assinatura(xml.elems("NomePessoa").map{_.text},xml.elems("Cargo").map{_.text})
}

final case class PartePrincipal(partes : Seq[PartePrincipalPart] = Seq(),coreopt : AG_coreopt = AG_coreopt()) extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
    <PartePrincipal>
			{NodeSeq.fromSeq(partes.map{_.xml()})}
		</PartePrincipal>
      )
  def attrData = Seq(coreopt)
}

object PartePrincipal extends LexmlXmlParsing {
  def apply(xml : Elem) : PartePrincipal = 
    PartePrincipal(xml.elemsChoiceP(PartePrincipalPart.labels).map{PartePrincipalPart(_)},
        AG_coreopt(xml))
}

final case class Evento(corereq : AG_corereq, date : AG_date,
    elems : Seq[Evento_Elem] = Seq()) extends LexmlRenderable {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
      <Evento>
			{NodeSeq.fromSeq(elems.map(_.xml()))}
     </Evento>
      )
  def attrData = Seq(corereq,date)
}

object Evento extends LexmlXmlParsing {
  def apply(xml : Elem) : Evento =     
    Evento(AG_corereq(xml),AG_date(xml),xml.elemsChoiceP(Evento_Elem.labels)
        .map{Evento_Elem(_)})
}



trait Evento_Elem extends Product with LexmlRenderable

object Evento_Elem {    
  val labels = Set("Criacao") ++ EventoRefURN.labels                   
  def apply(xml : Elem) : Evento_Elem = xml.label match {
    case "Criacao" => Criacao(xml)
    case x if EventoRefURN.labels(x) => EventoRefURN(x,refURN(xml))  
  }
}

final case class Criacao(projetosNormaOrigem : Seq[refURN] = Seq(),
    julgadosOrigemAnulacao : Seq[refURN] = Seq(),
    mensagensVetoAplicados : Seq[refURN] = Seq()) extends Evento_Elem {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
    <Criacao>
		{NodeSeq.fromSeq(projetosNormaOrigem.map{_.xml("ProjetoNormaOrigem")})}
		{NodeSeq.fromSeq(julgadosOrigemAnulacao.map{_.xml("JulgadoOrigemAnulacao")})}
		{NodeSeq.fromSeq(mensagensVetoAplicados.map{_.xml("MensagemVetoAplicado")})}
		</Criacao>
    )
}

object Criacao extends LexmlXmlParsing {
  def apply(xml : Elem) : Criacao = Criacao (
    xml.elems("ProjetoNormaOrigem").map{refURN(_)},
    xml.elems("JulgadoOrigemAnulacao").map{refURN(_)},
    xml.elems("MensagemVetoAplicado").map{refURN(_)}
    )
}

final case class EventoRefURN(tipo : String,ref : refURN) extends Evento_Elem  {
  def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = 
    ref.xml(Some(tipo))
}

object EventoRefURN {
  val labels = Set("Publicacao","EntradaEmVigor","Retificacao",
                   "Republicacao", "RevogacaoTotal", "AnulamentoTotal",
                   "AlteracaoFragmento")
  def apply(xml : Elem) : EventoRefURN = EventoRefURN(xml.label,refURN(xml))
}

final case class InlineText(text : String) extends inlineComponent with list_item_element {
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = Text(text)
}

trait hierElement extends Product with LexmlRenderable

object hierElement {
  val labels = LXhier.labels ++ Set("AgrupamentoHierarquico")
  def apply(xml : Elem) : hierElement = xml.label match {
    case x if LXhier.labels(x) => LXhier(xml)
    case "AgrupamentoHierarquico" => AgrupamentoHierarquico(xml)
  }
}

trait ContainerElement extends PartePrincipalPart 

object ContainerElement {
  def labels = HTMLcontainer.labels ++ Set("Agrupamento")
  def apply(xml : Elem) : ContainerElement = xml.label match {
    case x if HTMLcontainer.labels(x) => HTMLcontainer(xml)
    case "Agrupamento" => Agrupamento(xml)
  }
}

trait HTMLcontainer extends ContainerElement 

object HTMLcontainer {
  val labels = Set("div")
  def apply(xml : Elem) : HTMLcontainer = xml.label match {
    case "div" => div(xml)
  }
}

final case class div(blocks : blocksreq) extends HTMLcontainer {
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = 
    blocks.xml("div")
}

object div {
  def apply(xml : Elem) : div = div(blocksreq(xml))
}

final case class Agrupamento(blocks : blocksreq, nome : AG_nome) extends ContainerElement {
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = 
      blocks.xml("Agrupamento")
  def attrData = Seq(nome)
}

object Agrupamento  {
  def apply(xml : Elem) : Agrupamento = Agrupamento(blocksreq(xml),AG_nome(xml))
}

final case class blocksreq(corereq : AG_corereq, blockElems : Seq[blockElement] = Seq()) extends LexmlRenderable {
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = 
    <InvalidLabel>
			{NodeSeq.fromSeq(blockElems.map{_.xml()})}
		</InvalidLabel>
  def attrData = Seq(corereq)
}

object blocksreq extends LexmlXmlParsing {
  def apply(xml : Elem) : blocksreq = 
    blocksreq(AG_corereq(xml),
        xml.elemsChoiceP(blockElement.labels).map{blockElement(_)})  
}

trait blockElement extends PartePrincipalPart

object blockElement {
  def labels = HTMLblock.labels ++ Set("ConteudoExterno","Bloco")
  def apply(xml : Elem) : blockElement = xml.label match {
    case x if HTMLblock.labels(x) => HTMLblock(xml)
    case "ConteudoExterno" => ConteudoExterno(xml.child)
    case "Bloco" => Bloco(xml)
  }
}

final case class ConteudoExterno(contents : NodeSeq = NodeSeq.Empty) extends blockElement  {
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = 
    <ConteudoExterno>{contents}</ConteudoExterno>
}

final case class Bloco(nome : AG_nome, contents : inline = inline()) extends blockElement  {
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = 
    (<Bloco>{contents.xml()}</Bloco>)
  def attrData = Seq(nome)
}

object Bloco extends LexmlXmlParsing {
  def apply(xml : Elem) : Bloco =
    Bloco(AG_nome(xml),inline(xml))
}

trait HTMLblock extends blockElement 

object HTMLblock {
  val labels = Set("p","ul","ol","table")
  def apply(xml : Elem) : HTMLblock = xml.label match {
    case "p" => P(xml)
    case "ul" => ul(xml)
    case "ol" => ol(xml)
    case "table" => table(xml)
  }
}

final case class table(
    idreq : AG_idreq,
    htmlAttrs : AG_HTMLattrs = AG_HTMLattrs(),
    rows : Seq[tr] = Seq()) extends HTMLblock {
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
      <table>{NodeSeq.fromSeq(rows.map(_.xml()))}</table>
  )
  def attrData = Seq(idreq,htmlAttrs)
}

object table extends LexmlXmlParsing {
  def apply(xml : Elem) : table = table(
      AG_idreq(xml),AG_HTMLattrs(xml),xml.elems("tr").map{tr(_)})
}

final case class tr(htmlAttrs : AG_HTMLattrs = AG_HTMLattrs(),
    cells : Seq[table_cell] = Seq()) extends LexmlRenderable {
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = (
      <tr>{NodeSeq.fromSeq(cells.map{_.xml()})}</tr>
      )
  def attrData = Seq(htmlAttrs)
}

object tr extends LexmlXmlParsing {
  def apply(xml : Elem) : tr = {
    val cells = xml.elemsChoiceP({x => x == "th" || x == "td"}).collect {
      case e if e.label == "th" => th(e)
      case e if e.label == "td" => td(e)
    }
    tr(AG_HTMLattrs(xml),cells)
  }
}
    
sealed trait table_cell extends Product with LexmlRenderable

trait table_cell_parser[T <: table_cell] {
  def build : (AG_cellattrs,inline) => T
  def apply(xml : Elem) : T = 
    build(AG_cellattrs(xml),inline(xml))    
}

final case class th(attrs : AG_cellattrs = AG_cellattrs(), contents : inline = inline()) extends table_cell {
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = 
      contents.xml("th")
      
  def attrData = Seq(attrs)
}

object th extends table_cell_parser[th] {
  def build = th.apply
}

final case class td(attrs : AG_cellattrs = AG_cellattrs(), contents : inline = inline()) extends table_cell {
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = 
      contents.xml("td")
      
  def attrData = Seq(attrs)
}

object td extends table_cell_parser[td] {
  def build = td.apply
}

final case class P(contents : inline = inline()) extends HTMLblock with inlineComponent with list_item_element {
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = 
    contents.xml("p")
}

object P {
  def apply(xml : Elem) : P = P(inline(xml))
}


final case class ul(items : Seq[li] = Seq()) extends inlineComponent with HTMLblock with list_item_element {
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = 
    (<ul>{NodeSeq.fromSeq(items.map{_.xml()})}</ul>)
}

object ul extends LexmlXmlParsing { 
  def apply(xml : Elem) : ul = ul(xml.elems("li").map{li(_)})
}

final case class ol(items : Seq[li] = Seq()) extends inlineComponent with HTMLblock with list_item_element {
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) = 
    (<ol>{NodeSeq.fromSeq(items.map{_.xml()})}</ol>)
}

object ol extends LexmlXmlParsing { 
  def apply(xml : Elem) : ol = ol(xml.elems("li").map{li(_)})
}

trait list_item_element extends Product with LexmlRenderable

final case class li(items : Seq[list_item_element] = Seq()) extends LexmlRenderable {  
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) =
    (<li>{NodeSeq.fromSeq(items.map{_.xml()})}</li>)
}

object li extends LexmlXmlParsing {
  def apply(xml : Elem) : li = {
    val elems : Seq[list_item_element] = xml.child.to[Seq].collect { 
      case e : Elem if inlineElement.labels(e.label) => inlineElement(e)
      case e : Elem if markerElement.labels(e.label) => markerElement(e)
      case t : scala.xml.Text => new InlineText(t.text)  
      case e : Elem if e.label == "ul" => ul(e)
      case e : Elem if e.label == "ol" => ol(e)
      case e : Elem if e.label == "p" => P(e)
    }
    li(elems)
  }
}

trait PartePrincipalPart extends Product with LexmlRenderable

object PartePrincipalPart {
  def apply(xml : Elem) : PartePrincipalPart = {
    if(ContainerElement.labels(xml.label)) {
      ContainerElement(xml)
    } else if (blockElement.labels(xml.label)) {
      blockElement(xml)
    } else {
      AgrupamentoHierarquico(xml)
    }    
  }
  def labels = Set("AgrupamentoHierarquico") ++ ContainerElement.labels ++ blockElement.labels
}

final case class AgrupamentoHierarquico(
    nome : AG_nome,
    hierarchy : hierarchy,
    elems : Seq[LXhierCompleto] = Seq()) extends PartePrincipalPart with LXhier {  
  override def xml_in(implicit xml_params : LexmlXmlRenderParams = LexmlXmlRenderParams()) =
    (<AgrupamentoHierarquico>
			{hierarchy.xml().child}
			{NodeSeq.fromSeq(elems.map{_.xml()})}
		</AgrupamentoHierarquico>)
  def attrData = Seq(nome)
}

object AgrupamentoHierarquico extends LexmlXmlParsing {
  def apply(xml : Elem) : AgrupamentoHierarquico = 
    AgrupamentoHierarquico(AG_nome(xml),hierarchy(xml),
        xml.elemsChoiceP(LXhierCompleto.labels).map { LXhierCompleto(_) })
}

trait LXhier extends LXhierCompleto with hierElement 

object LXhier {    
  val labels = Set("Parte","Livro","Titulo","Capitulo","Secao","Artigo")
  def apply(xml : Elem) : LXhier = xml.label match {
    case "Parte" => Parte(xml)
    case "Livro" => Livro(xml)
    case "Titulo" => Titulo(xml)
    case "Capitulo" => Capitulo(xml)
    case "Secao" => Secao(xml)
    case "Artigo" => ArtigoType(xml)
  }
}

trait AgrupamentoParser[T] extends LexmlXmlParsing {
  def apply(xml : Elem) : T = 
    build(hierarchy(xml),xml.elemsChoiceP(LXhierCompleto.labels).map{LXhierCompleto(_)})
  
  def build : (hierarchy,Seq[LXhierCompleto]) => T
}

//20190320 - STOPPED HERE - Inserting rendering code

final case class Parte(hierarchy : hierarchy,elems : Seq[LXhierCompleto] = Seq()) extends LXhier

object Parte extends AgrupamentoParser[Parte] {
  def build = Parte.apply
}
 
final case class Livro(hierarchy : hierarchy,elems : Seq[LXhierCompleto] = Seq()) extends LXhier

object Livro extends AgrupamentoParser[Livro] {
  def build = Livro.apply
}

final case class Titulo(hierarchy : hierarchy,elems : Seq[LXhierCompleto] = Seq()) extends LXhier

object Titulo extends AgrupamentoParser[Titulo] {
  def build = Titulo.apply
}

final case class Capitulo(hierarchy : hierarchy,elems : Seq[LXhierCompleto] = Seq()) extends LXhier

object Capitulo extends AgrupamentoParser[Capitulo] {
  def build = Capitulo.apply
}

final case class Secao(hierarchy : hierarchy,elems : Seq[LXhierCompleto] = Seq()) extends LXhier

object Secao extends AgrupamentoParser[Secao] {
  def build = Secao.apply
}

final case class Subsecao(hierarchy : hierarchy,elems : Seq[LXhierCompleto] = Seq()) extends LXhier

object Subsecao extends AgrupamentoParser[Subsecao] {
  def build = Subsecao.apply
}

final case class ArtigoType(
    rotulo : String,
    corereqArt : AG_corereqArt,
    linkopt : AG_linkopt = AG_linkopt(),
    tituloDispositivo : Option[inline] = None,    
    elems : Seq[art_elem] = Seq()) extends LXhier

object ArtigoType extends LexmlXmlParsing {
  def apply(xml : Elem) : ArtigoType = {
    val tituloDispositivo = xml.elemOpt("TituloDispositivo").map{inline(_)}
    val rotulo = xml.elem("Rotulo").text
    val artElems = xml.elemsChoiceP({l => LXcontainer.labels(l) || l == "DispositivoGenerico"}).map{ArtigoType.artElem(_)}
    ArtigoType(
        rotulo,
        AG_corereqArt(xml),AG_linkopt(xml),        
        tituloDispositivo,        
        artElems)
  }
  def artElem(xml : Elem) : art_elem = xml.label match {
    case x if LXcontainer.labels(x) => LXcontainer(xml)
    case "DispositivoGenerico" => DispositivoGenerico(xml)
  }
}

trait art_elem extends Product

final case class DispositivoGenerico(nome : AG_nome, disp : DispositivoType) extends art_elem

object DispositivoGenerico  {
  def apply(xml : Elem) : DispositivoGenerico = 
    DispositivoGenerico(AG_nome(xml),DispositivoType(xml))
}

abstract sealed trait LXhierCompleto extends Product

object LXhierCompleto {
  val labels = LXhier.labels ++ Set("Subsecao")
  def apply(xml : Elem) : LXhierCompleto = xml.label match {
    case x if (LXhier.labels(x)) => LXhier(xml)
    case "Subsecao" => Subsecao(xml)    
  }
}

final case class hierarchy(corereq : AG_corereq,
    rotulo : Option[String] = None,
    nomeAgrupador : Option[inline] = None,
    agrupamentosHierarquicos : Seq[AgrupamentoHierarquico] = Seq()) 

object hierarchy extends LexmlXmlParsing {
  def apply(xml : Elem) : hierarchy = hierarchy(
    AG_corereq(xml),
    xml.elemOpt("Rotulo").map { _.text },
    xml.elemOpt("NomeAgrupador").map { inline(_) },
    xml.elems("AgrupamentoHierarquico").map { AgrupamentoHierarquico(_) })  
}

trait LXcontainer extends art_elem 

object LXcontainer {  
  val labels = Set("Caput","Paragrafo","Inciso","Alinea","Item")
  def apply(xml : Elem) : LXcontainer = xml.label match {
    case "Caput" => Caput(xml)
    case "Paragrafo" => Paragrafo(xml)
    case "Inciso" => Inciso(xml)
    case "Alinea" => Alinea(xml)
    case "Item" => Item(xml)
  }
}

final case class Caput(disp :  DispositivoType) extends LXcontainer

object Caput extends LexmlXmlParsing { 
  def apply(xml : Elem) : Caput = Caput(DispositivoType(xml))
}

final case class Inciso(disp :  DispositivoType) extends LXcontainer

object Inciso extends LexmlXmlParsing { 
  def apply(xml : Elem) : Inciso = Inciso(DispositivoType(xml))
}

final case class Paragrafo(disp :  DispositivoType) extends LXcontainer

object Paragrafo extends LexmlXmlParsing { 
  def apply(xml : Elem) : Paragrafo = Paragrafo(DispositivoType(xml))
}

final case class Alinea(disp :  DispositivoType) extends LXcontainer

object Alinea extends LexmlXmlParsing { 
  def apply(xml : Elem) : Alinea = Alinea(DispositivoType(xml))
}

final case class Item(disp :  DispositivoType) extends LXcontainer

object Item extends LexmlXmlParsing { 
  def apply(xml : Elem) : Item = Item(DispositivoType(xml))
}

final case class DispositivoType(
  tituloDispositivo : Option[inline] = None,
  rotulo : Option[String] = None,
  pars : Seq[inline] = Seq(),
  alteracao : Option[AlteracaoType] = None,
  containers : Seq[LXcontainer] = Seq(),
  dispsGenericos : Seq[DispositivoGenerico] = Seq(),
  pena : Option[PenaType] = None)

object DispositivoType extends LexmlXmlParsing { 
  def apply(xml : Elem) : DispositivoType = {
    val tituloDispositivo = xml.elemOpt("TituloDispositivo").map{inline(_)}
    val rotulo = xml.elemOpt("Rotulo").map(_.text)
    val pars = xml.elems("p").map{inline(_)}
    val alteracao = xml.elemOpt("Alteracao").map{AlteracaoType(_)}
    val containers = xml.elemsChoiceP(LXcontainer.labels).map{LXcontainer(_)}
    val dispsGenericos = xml.elems("DispositivoGenerico").map{DispositivoGenerico(_)}
    val pena = xml.elemOpt("Pena").map{PenaType(_)}
    DispositivoType(tituloDispositivo,rotulo,pars,alteracao,containers,dispsGenericos,pena)
  }
}

final case class PenaType(corereq : AG_corereq, linkopt : AG_linkopt = AG_linkopt(),
    rotulo : Option[String] = None,
    pars : Seq[inline] = Seq(),
    dispsGenericos : Seq[DispositivoGenerico] = Seq())

object PenaType extends LexmlXmlParsing {
  def apply(xml : Elem) : PenaType = {
    val rotulo = xml.elemOpt("Rotulo").map(_.text)
    val pars = xml.elems("p").map{inline(_)}
    val dispsGenericos = xml.elems("DispositivoGenerico").map{DispositivoGenerico(_)}
    PenaType(AG_corereq(xml),AG_linkopt(xml),
        rotulo,pars,dispsGenericos)        
  }
}

final case class AlteracaoType(corereq : AG_corereq,base : Option[String] = None) 

object AlteracaoType extends LexmlXmlParsing {
  def apply(xml : Elem) : AlteracaoType = 
    AlteracaoType(AG_corereq(xml),xml.optAttrNS(Namespaces.xml,"base"))
}

final case class Alteracao(alt : AlteracaoType) extends LXinline

object Alteracao { 
  def apply(xml : Elem) : Alteracao = Alteracao(AlteracaoType(xml))
}

trait inlineElement extends inlineComponent with list_item_element 

object inlineElement {
  val labels = LXinline.labels ++ HTMLinline.labels ++ Set("EmLinha")
  def apply(xml : Elem) : inlineElement = xml.label match {
    case x if LXinline.labels(x) => LXinline(xml)
    case x if HTMLinline.labels(x) => HTMLinline(xml)
    case "EmLinha" => EmLinha(xml)
  }
}

final case class EmLinha(nome : AG_nome, contents : inline = inline()) extends inlineElement 

object EmLinha {
  def apply(xml : Elem) : EmLinha = EmLinha(AG_nome(xml),inline(xml))
}

trait markerElement extends inlineComponent with list_item_element 

object markerElement {
  val labels = LXmarker.labels ++ HTMLmarker.labels ++ Set("Marcador")
  def apply(xml : Elem) : markerElement = xml.label match {
    case x if LXmarker.labels(x) => LXmarker(xml)
    case x if HTMLmarker.labels(x) => HTMLmarker(xml)
    case "Marcador" => Marcador(xml)
  }
}

final case class Marcador(nome : AG_nome, corereq : AG_corereq) extends markerElement

object Marcador {
  def apply(xml : Elem) : Marcador = Marcador(AG_nome(xml),AG_corereq(xml))
}

trait LXinline extends inlineElement 

object LXinline {
  val labels = Set("Remissao","RemissaoMultipla","Alteracao","Formula")
  def apply(xml : Elem) : LXinline = xml.label match {
    case "Remissao" => Remissao(xml)
    case "RemissaoMultipla" => RemissaoMultipla(xml)
    case "Alteracao" => Alteracao(xml)
    case "Formula" => Formula(xml)
  }
}

final case class Remissao(link : AG_link, contents : inline = inline()) extends LXinline

object Remissao {
  def apply(xml : Elem) : Remissao = Remissao(AG_link(xml),inline(xml))
}

final case class RemissaoMultipla(base : String, contents : inline = inline()) extends LXinline

object RemissaoMultipla extends LexmlXmlParsing {
  def apply(xml : Elem) : RemissaoMultipla = RemissaoMultipla(
      xml.attrNS(Namespaces.xml,"base"),inline(xml))
}

final case class Formula(
    mathml : Option[Elem] = None,
    fonte : Option[String] = None,
    tipo : Option[String] = None) extends LXinline 

object Formula extends LexmlXmlParsing {
  def apply(xml : Elem) : Formula = {
    val mathml = xml.optElemNS(Namespaces.mathml,"math")
    val fonte = xml.optAttr("fonte")
    val tipo = xml.optAttr("tipo")
    Formula(mathml,fonte,tipo)
  }
}

sealed trait HTMLinline extends inlineElement 

object HTMLinline {
  val labels = Set("span", "b", "i", "a", "sub", "sup", "ins", "del", "dfn")
  def apply(xml : Elem) : HTMLinline = xml.label match {
    case "span" => span(xml)
    case "b" => bold(xml)
    case "i" => italics(xml)
    case "a" => anchor(xml)
    case "sub" => sub(xml)
    case "sup" => sup(xml)
    case "ins" => ins(xml)
    case "del" => del(xml)
    case "dfn" => dfn(xml)
  }
}

final case class span(link : AG_link, contents : inline = inline()) extends HTMLinline  

object span {
  def apply(xml : Elem) : span = span(AG_link(xml),inline(xml))
}

final case class bold(link : AG_link, contents : inline = inline()) extends HTMLinline  

object bold {
  def apply(xml : Elem) : bold = bold(AG_link(xml),inline(xml))
}

final case class italics(link : AG_link, contents : inline = inline()) extends HTMLinline  

object italics {
  def apply(xml : Elem) : italics = italics(AG_link(xml),inline(xml))
}

final case class sub(link : AG_link, contents : inline = inline()) extends HTMLinline  

object sub {
  def apply(xml : Elem) : sub = sub(AG_link(xml),inline(xml))
}

final case class sup(link : AG_link, contents : inline = inline()) extends HTMLinline  

object sup {
  def apply(xml : Elem) : sup = sup(AG_link(xml),inline(xml))
}

final case class ins(link : AG_link, contents : inline = inline()) extends HTMLinline  

object ins {
  def apply(xml : Elem) : ins = ins(AG_link(xml),inline(xml))
}

final case class del(link : AG_link, contents : inline = inline()) extends HTMLinline  

object del {
  def apply(xml : Elem) : del = del(AG_link(xml),inline(xml))
}

final case class dfn(link : AG_link, contents : inline = inline()) extends HTMLinline  

object dfn {
  def apply(xml : Elem) : dfn = dfn(AG_link(xml),inline(xml))
}


final case class anchor(link : AG_link, target : Option[String] = None, name : AG_HTMLname = AG_HTMLname(), contents : inline = inline())
  extends HTMLinline

object anchor extends LexmlXmlParsing {  
  def apply(xml : Elem) : anchor = 
    anchor(AG_link(xml),xml.optAttr("target"),AG_HTMLname(xml),inline(xml))
}

trait LXmarker extends markerElement 

object LXmarker {
  val labels = Set("NotaReferenciada")
  def apply(xml : Elem) : LXmarker = xml.label match {
    case "NotaReferenciada" => NotaReferenciada(xml) 
  }
}


final case class NotaReferenciada(mopt : markeropt = markeropt(),link : AG_linkID = AG_linkID()) extends LXmarker

object NotaReferenciada {
  def apply(xml : Elem) : NotaReferenciada = 
    NotaReferenciada(markeropt(xml),AG_linkID(xml))
}

final case class markeropt(coreopt : AG_coreopt = AG_coreopt()) 

object markeropt {
  def apply(xml : Elem) : markeropt = markeropt(AG_coreopt(xml))    
}

trait HTMLmarker extends markerElement 

object HTMLmarker {
  val labels = Set("img")
  def apply(xml : Elem) : HTMLmarker = xml.label match {
    case "img" => img(xml)
  }
}

final case class img(src : String, mopt : markeropt = markeropt(),
    alt : Option[String] = None,
    width : Option[Int] = None,
    height : Option[Int] = None) extends HTMLmarker

object img extends LexmlXmlParsing {
  def apply(xml : Elem) : img = {
    val src = xml.attr("src")
    val alt = xml.optAttr("alt")
    val width = xml.optAttr("width").map(_.toInt)
    val height = xml.optAttr("height").map(_.toInt)
    img(src,markeropt(xml),alt,width,height)
  }
}

 
final case class IdentificacaoType(urn : String)

object IdentificacaoType extends LexmlXmlParsing { 
  def apply(xml : Elem) : IdentificacaoType = 
    IdentificacaoType(xml.attr("URN"))
}


/* Attribute Groups */


final case class AG_coreopt(htmlAttrs : AG_HTMLattrs = AG_HTMLattrs(), 
    enactment : AG_enactment = AG_enactment(), idopt : AG_idopt = AG_idopt())  extends AttrData {
  override def attrSeq = htmlAttrs.attrSeq ++
    enactment.attrSeq ++ idopt.attrSeq
}

object AG_coreopt {
  def apply(xml : Elem) : AG_coreopt = 
    AG_coreopt(AG_HTMLattrs(xml),AG_enactment(xml),AG_idopt(xml))
}

final case class AG_lang(lang : String = "pt-BR") extends AttrData {
  override def attrSeq = if (lang == "pt-BR") { Seq() } else {
    Seq(Some(((Some("xml"),"lang"),lang))) }
  }

object AG_lang extends LexmlXmlParsing {
  def apply(xml : Elem) : AG_lang = AG_lang(xml.optAttrNS(Namespaces.xml,"lang").getOrElse("pt-BR"))
}

final case class AG_HTMLattrs(
     `class` : Option[String] = None, 
      style : Option[String] = None, 
      title : Option[String] = None,
      lang : AG_lang = AG_lang()) extends AttrData {
  override def attrSeq = Seq(
      `class`.map{x => ((None,"class"),x)},
      style.map{x => ((None,"style"),x)},
      title.map{x => ((None,"title"),x)}) ++ lang.attrSeq
}      

object AG_HTMLattrs extends LexmlXmlParsing {
  def apply(elem : Elem) : AG_HTMLattrs = 
    AG_HTMLattrs(elem.optAttr("class"),elem.optAttr("style"),elem.optAttr("title"),AG_lang(elem))
}

final case class AG_enactment(period : AG_period = AG_period(), situacao : Option[Situacao] = None) extends AttrData {
  override def attrSeq = period.attrSeq ++
    Seq(situacao.map({x => ((None,"situacao"),x.value)}))
}

object AG_enactment extends LexmlXmlParsing  {
  def apply(xml : Elem) : AG_enactment = {
    val sit = xml.optAttr("situacao").flatMap{ x => Situacao(x) }  
    AG_enactment(period = AG_period(xml),situacao = sit)
  }
}

final case class AG_idopt(id : Option[String] = None) extends AttrData {
  override def attrSeq = Seq(id.map{x => ((None,"id"),x)})
}

object AG_idopt extends LexmlXmlParsing {
  def apply(xml : Elem) : AG_idopt = AG_idopt(xml.optAttr("id"))
}

final case class AG_period(
    eventoInicial : Option[String] = None,
    eventoFinal : Option[String] = None) extends AttrData {
  override def attrSeq = Seq(
      eventoInicial.map{x => ((None,"eventoInicial"),x)},
      eventoFinal.map{x => ((None,"eventoFinal"),x)})
      
}

object AG_period extends LexmlXmlParsing {
  def apply(xml : Elem) : AG_period = {
    val eventoInicial = xml.optAttr("eventoInicial")
    val eventoFinal = xml.optAttr("eventoFinal")
    AG_period(eventoInicial,eventoFinal)
  }
}

final case class AG_nome(nome : String) extends AttrData {
  override def attrSeq = Seq(Some((None,"nome"),nome))
}

object AG_nome extends LexmlXmlParsing {
  def apply(xml : Elem) : AG_nome = AG_nome(xml.attr("nome"))
}

final case class AG_corereq(idreq : AG_idreq, htmlAttrs : AG_HTMLattrs = AG_HTMLattrs(), 
    enactment : AG_enactment = AG_enactment()) extends AttrData {
  override def attrSeq = idreq.attrSeq ++ htmlAttrs.attrSeq ++ enactment.attrSeq
}

object AG_corereq extends LexmlXmlParsing {
  def apply(xml : Elem) : AG_corereq = 
    AG_corereq(AG_idreq(xml),AG_HTMLattrs(xml),AG_enactment(xml))
}

final case class AG_idreq(id : String) extends AttrData {
  override def attrSeq = Seq(Some((None,"id"),id))
}

object AG_idreq extends LexmlXmlParsing {
  def apply(xml : Elem) : AG_idreq = 
    AG_idreq(xml.attr("id"))
}

final case class AG_date(date : String) extends AttrData {
  override def attrSeq = Seq(Some((Some("xsd"),"date"),date))
}

object AG_date extends LexmlXmlParsing {
  def apply(xml : Elem) : AG_date = 
    AG_date(xml.attrNS(Namespaces.xsd,"date"))
}

final case class AG_link(href : String)  extends AttrData {
  override def attrSeq = Seq(Some((Some("xsd"),"href"),href))
}

object AG_link extends LexmlXmlParsing {
  def apply(xml : Elem) : AG_link = 
    AG_link(xml.attrNS(Namespaces.xlink,"href"))
}

final case class AG_HTMLname(name : Option[String] = None) extends AttrData {
  override def attrSeq = Seq(name.map{x => ((None,"name"),x)})
} 

object AG_HTMLname extends LexmlXmlParsing {
  def apply(xml : Elem) : AG_HTMLname =
    AG_HTMLname(xml.optAttr("name"))
}

final case class AG_linkID(nota : Option[String] = None) extends AttrData {
  override def attrSeq = Seq(nota.map{x => ((None,"nota"),x)})
}

object AG_linkID extends LexmlXmlParsing {
  def apply(xml : Elem) : AG_linkID =
    AG_linkID(xml.optAttr("nota"))
}

final case class AG_source(fonte : String) extends AttrData {
  override def attrSeq = Seq(Some(((None,"fonte"),fonte)))
}

object AG_source extends LexmlXmlParsing {
  def apply(xml : Elem) : AG_source =
    AG_source(xml.attr("fonte"))
}

final case class AG_corereqArt(
    idreqArt : AG_idreqArt,
    htmlAttrs : AG_HTMLattrs = AG_HTMLattrs(),
    enactment : AG_enactment = AG_enactment(),    
    textoOmitido : Boolean = false) extends AttrData {
  override def attrSeq = idreqArt.attrSeq ++
     htmlAttrs.attrSeq ++
     enactment.attrSeq ++
     (if (textoOmitido) { Seq(Some(((None,"textoOmitido"),"s"))) } else { Seq() })     
}

object AG_corereqArt extends LexmlXmlParsing {
  def apply(xml : Elem) : AG_corereqArt = 
    AG_corereqArt(AG_idreqArt(xml),AG_HTMLattrs(xml),AG_enactment(xml),
        xml.optAttr("textoOmitido").isDefined)
}


final case class AG_idreqArt(id : String) extends AttrData {
  override def attrSeq = Seq(Some(((None,"id"),id)))
}

object AG_idreqArt extends LexmlXmlParsing {
  def apply(xml : Elem) : AG_idreqArt =
    AG_idreqArt(xml.attr("id"))
} 

final case class AG_linkopt(href : Option[String] = None) extends AttrData {
  override def attrSeq = Seq(href.map{ x => ((Some("xsd"),"href"),x) })
}

object AG_linkopt  extends LexmlXmlParsing {
  def apply(xml : Elem) : AG_linkopt =
    AG_linkopt(xml.optAttrNS(Namespaces.xsd,"href"))
}



final case class AG_cellattrs(rowspan : Int = 1,colspan : Int = 1) extends AttrData {
  override def attrSeq = Seq(
      if (rowspan == 1) {None} else {Some(((None,"rowspan"),rowspan.toString))},
      if (colspan == 1) {None} else {Some(((None,"colspan"),colspan.toString))}
      )
}

object AG_cellattrs extends LexmlXmlParsing {
  def apply(xml : Elem) : AG_cellattrs =
    AG_cellattrs(xml.optAttr("rowspan").map(_.toInt).getOrElse(1),
        xml.optAttr("colspan").map(_.toInt).getOrElse(1))  
}

/* Attribute Values */

class AttributeValue(val attrValueType : String, val value : String) {  
  final override def toString() = "{" + attrValueType + "}" + value
}

abstract sealed class Situacao(val value : String) extends AttributeValue("TipoSituacao",value)

class AttributeValueEnumBuilder[T <: AttributeValue](vals : T*) {
  def apply(value : String) = vals.find(x => x.value == value)
}


case object Sit_Omissis extends Situacao("omissis")
case object Sit_Revogado extends Situacao("revogado")
case object Sit_Suspenso extends Situacao("suspenso")
case object Sit_Vetado extends Situacao("vetado")
case object Sit_Superado extends Situacao("superado")

object Situacao extends AttributeValueEnumBuilder[Situacao](Sit_Omissis,Sit_Revogado,Sit_Suspenso,Sit_Vetado,Sit_Superado)

