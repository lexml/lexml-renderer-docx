package br.gov.lexml.renderer.docx

import scala.xml._

trait XmlRepr {
  def xml : Elem  
  def subElem(f : Elem => scala.xml.NodeSeq) = f(xml).headOption.collect { case e : Elem => e }  
  def elem(label : String) = subElem(_ \ label).get
  def elemOpt(label : String) = subElem(_ \ label)  
  def elemChoiceOpt(labels : String*) = labels.flatMap(elemOpt).headOption
  def elemChoice(labels : String*) = elemChoiceOpt(labels : _*).get
  def elems(label : String) = (xml \ label).to[Seq].collect { case e : Elem => e }
  def elemsChoiceP(p : String => Boolean) = xml.child.to[Seq].collect { case e : Elem if p(e.label) => e } 
  def optAttr(label : String) = xml.attributes.get(label).headOption.map(_.headOption).flatten.map(_.text)
  def optAttrNS(uri : String, label : String) = xml.attributes.get(uri,xml,label).headOption.map(_.headOption).flatten.map(_.text)
  def attr(label : String) = optAttr(label).get
}

class LexmlDocument(val xml : Elem) extends XmlRepr {
  lazy val metadado : Metadado = 
    new Metadado((xml \ "Metadado").head.asInstanceOf[Elem])
  lazy val documentContent : DocumentContent = {    
    DocumentContent(elemChoice("Norma","ProjetoNorma","Jurisprudencia","DocumentoGenerico","Anexo"))
  }
    
}

class Metadado(val xml : Elem) extends XmlRepr {
  //TODO: completar
}

abstract sealed class DocumentContent extends XmlRepr

object DocumentContent {
  def apply(elem : Elem) = elem.label match {
    case "Norma" => new Norma(elem)
    case "ProjetoNorma" => new ProjetoNorma(elem)
    case "Jurisprudencia" => new Jurisprudencia(elem)
    case "Anexo" => new Anexo(elem)
  }
}

class Norma(val xml : Elem) extends DocumentContent {
  //TODO: completar
}

class ProjetoNorma(val xml : Elem) extends DocumentContent {
  lazy val norma = new Norma(elem("Norma"))
  lazy val justificacao = elems("Justificacao").map(new OpenStructure(_))
  lazy val autorProjeto = elems("AuthorProjeto").map(_.text)
}

class Jurisprudencia(val xml : Elem) extends DocumentContent 

class Anexo(val xml : Elem) extends DocumentContent {
  //TODO: completar
}

class AutorProjeto(val xml : Elem) extends XmlRepr {
  //TODO: completar
}

class OpenStructure(val xml : Elem) extends XmlRepr {
  lazy val partePrincipal = elemOpt("PartePrincipal").map(new PartePrincipal(_))
  lazy val anexos = elemOpt("Anexos").map(new Anexos(_))
}

trait ContainerElement extends XmlRepr with PartePrincipalPart

object ContainerElement {
  //TODO: completar
  def labels = Set("")
  def apply(xml : Elem) : ContainerElement = ???
}

trait BlockElement extends XmlRepr with PartePrincipalPart

object BlockElement {
  //TODO: completar
  def labels = Set("")
  def apply(xml : Elem) : BlockElement = ???
}

trait PartePrincipalPart extends XmlRepr

object PartePrincipalPart {
  def apply(xml : Elem) : PartePrincipalPart = {
    if(ContainerElement.labels(xml.label)) {
      ContainerElement(xml)
    } else if (BlockElement.labels(xml.label)) {
      BlockElement(xml)
    } else {
      new AgrupamentoHierarquico(xml)
    }    
  }
  def labels = Set("AgrupamentoHierarquico") ++ ContainerElement.labels ++ BlockElement.labels
}

class PartePrincipal(val xml : Elem) extends XmlRepr with AG_coreopt {
  lazy val partes = elemsChoiceP(PartePrincipalPart.labels)
}

class Anexos(val xml : Elem) extends XmlRepr {
  //TODO: completar
}

class AgrupamentoHierarquico(val xml : Elem) extends XmlRepr with PartePrincipalPart with Hierarchy with LXhierCompleto with AG_nome {
  //TODO: Acho que a relacao com Hierarchy não está OK, revisar a partir do schema
}

abstract sealed trait LXhier extends LXhierCompleto {
  this : XmlRepr =>
}

object LXhier {  
  /* TODO: faltam ver as definicoes de:
   * Choice of:
   * _Parte
   * _Livro
   * _Titulo
   * _Capitulo
   * _Secao
   * _Artigo
   * (all abstract)
   */
  val labels = Set("")
  def apply(xml : Elem) : LXhier = ???
}

abstract sealed trait LXhierCompleto {
  this : XmlRepr =>    
}

object LXhierCompleto {
  //TODO: Falta ver a definicao de _Subsecao
  /* Choice of:
   * LXhier
   * _Subsecao
   */
  val labels = Set("")
  def apply(xml : Elem) : LXhierCompleto = {
    if (LXhier.labels(xml.label)) {
      LXhier(xml)
    } else {
      ??? /* _Subsecao (abstract) */
    }
  }
}

trait Hierarchy extends AG_corereq {
  this : XmlRepr =>  
    lazy val rotulo = elemOpt("Rotulo").map { _.text }
    lazy val nomeAgrupador = elemOpt("NomeAgrupador").map { new inline(_) }
    lazy val agrupamentoHierarquico = elems("AgrupamentoHierarquico").map { new AgrupamentoHierarquico(_) }    
}

class inline(val xml : Elem) extends XmlRepr {
  //TODO: completar
}

/* Attribute Groups */


trait AttributeGroup {
  this : XmlRepr =>
    
}

trait AG_coreopt extends AttributeGroup with AG_HTMLattrs with AG_enactment with AG_idopt {
  this : XmlRepr =>
}

trait AG_HTMLattrs extends AttributeGroup {
  this : XmlRepr =>
    lazy val class_attr = optAttr("class")
    lazy val style = optAttr("style")
    lazy val title = optAttr("title")
    lazy val lang = optAttrNS("http://www.w3.org/XML/1998/namespace","lang").getOrElse("pt-BR")
}

trait AG_enactment extends AttributeGroup with AG_period {
  this : XmlRepr =>
    lazy val situacao = optAttr("situacao").map(Situacao(_))
}

trait AG_idopt extends AttributeGroup {
  this : XmlRepr =>
    lazy val id = optAttr("id")
}

trait AG_period extends AttributeGroup {
  this : XmlRepr =>
  lazy val eventoInicial = optAttr("eventoInicial")
  lazy val eventoFinal = optAttr("eventoFinal")
}

trait AG_nome extends AttributeGroup {
  this : XmlRepr =>
    lazy val nome = attr("nome")
}

trait AG_corereq extends AG_HTMLattrs with AG_enactment with AG_idreq {
  this : XmlRepr =>    
}

trait AG_idreq extends AttributeGroup {
  this : XmlRepr =>
    lazy val id = attr("id")
}

/* Attribute Values */

class AttributeValue(val attrValueType : String, val value : String) {  
  final override def toString() = "{" + attrValueType + "}" + value
}

abstract sealed class Situacao(value : String) extends AttributeValue("TipoSituacao",value)

class AttributeValueEnumBuilder[T <: AttributeValue](vals : T*) {
  def apply(value : String) = vals.find(x => x.value == value)
}


case object Sit_Omissis extends Situacao("omissis")
case object Sit_Revogado extends Situacao("revogado")
case object Sit_Suspenso extends Situacao("suspenso")
case object Sit_Vetado extends Situacao("vetado")
case object Sit_Superado extends Situacao("superado")

object Situacao extends AttributeValueEnumBuilder[Situacao](Sit_Omissis,Sit_Revogado,Sit_Suspenso,Sit_Vetado,Sit_Superado)

