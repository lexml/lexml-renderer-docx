package br.gov.lexml.renderer.docx.docxmodel

import scala.xml._

import scala.reflect.runtime.universe._

import scala.reflect.ClassTag

import br.gov.lexml.renderer.docx.docxmodel.builders.implicits.RichOption1


final case class StyleOptField[T](
    val ft : StyleFieldType[T],val value : T)
     extends XmlComponent with Comparable[Any] {
  this.ensuring(value!=null)
  def asXML = ft.toXML(value)  
  override def compareTo(x : Any) : Int = x match {
    case f : StyleOptField[_] => ft.compareTo(f.ft)
    case _ => -1
  }
}

object StyleOptField {
  implicit def sofOrdering[T] = new Ordering[StyleOptField[T]] {
    def compare(x: StyleOptField[T], y: StyleOptField[T]): Int =
      x.ft.compareTo(y.ft)

  }
}

sealed abstract class StyleFieldType[T](
    val name : String,
    val pos : Int)
  (implicit ct : ClassTag[T]) extends Comparable[Any] {
  def toXML(value : T) : Elem
  override def compareTo(x : Any) : Int = x match {
    case ft : StyleFieldType[_] => pos.compareTo(ft.pos)
    case _ => -1
  }
}

sealed abstract class StyleValFieldType[T](name : String,pos : Int)(implicit ct : ClassTag[T]) 
  extends StyleFieldType[T](name,pos)(ct) {  
  override def toXML(value : T) : Elem =
    (<w:elem w:val={value.toString} />
			).copy(label = name)
}

sealed abstract class StyleStringFieldType(name : String,pos : Int) 
  extends StyleValFieldType[String](name,pos) 

case object S_BasedOn extends StyleStringFieldType("basedOn",3)
case object S_Next extends StyleStringFieldType("next",4)
case object S_Link extends StyleStringFieldType("link",5)


abstract sealed class StyleIntFieldType(name : String, pos : Int)
  extends StyleValFieldType[Int](name,pos)
  
case object S_UIPriority extends StyleIntFieldType("uiPriority",8)  

sealed abstract class StyleFlagType(name : String, pos : Int) 
  extends StyleFieldType[Unit](name,pos) {  
  def toXML(value : Unit) = (<w:x/>).copy(label = name)
}

case object S_AutoRedefine extends StyleFlagType("autoRedefine",6)
case object S_Hidden extends StyleFlagType("hidden",7)
case object S_QFormat extends StyleFlagType("qFormat",11)
case object S_SemiHidden extends StyleFlagType("semiHidden",9)
case object S_UnhideWhenUsed extends StyleFlagType("unhideWhenUsed",10)
case object S_Locked extends StyleFlagType("locked",12)



sealed abstract class StyleType(val value : String) extends Product

case object ST_Paragraph extends StyleType("paragraph")
case object ST_Character extends StyleType("character")
case object ST_Table extends StyleType("table")
case object ST_Numbering extends StyleType("numbering")

final case class Style(
    `type` : StyleType,
    id : String,
    aliases : Set[String] = Set(),
    name : Option[String] = None,
    fields : Map[StyleFieldType[_],StyleOptField[T] forSome { type T }] = Map(
        S_QFormat -> StyleOptField(S_QFormat,())),    
    customStyle : Boolean = false,
    default : Boolean = false,
    pPr : Option[PPr] = None,
    rPr : Option[RPr] = None
) extends XmlComponent {
  
  def field[Q](ft : StyleFieldType[Q])
    (implicit ctv : ClassTag[Q]) : Option[Q] =    
    fields.get(ft).map(_.value).collect {
      case ctv(x : Q) => x
    }
  
  def setField[Q](ft : StyleFieldType[Q]) = (v : Q) =>
    copy (fields = fields + (ft -> StyleOptField(ft,v)))
    
  def clearField[Q](ft : StyleFieldType[Q]) =
    copy (fields = fields - ft)
    
  def flag(st : StyleFlagType) = fields.contains(st)
  def setFlag(st : StyleFlagType) = (v : Boolean) =>
    if(v) { setField(st)(()) } else { clearField(st) } 
      
      
  def setName(n : String) = copy(name=Some(n))
  
  def basedOn = field(S_BasedOn)
  
  val setBasedOn = setField(S_BasedOn)
  
  def link = field(S_Link)
  
  val setLink = setField(S_Link)
  
  def next = field(S_Next)
  
  val setNext = setField(S_Next)
      
  def autoRedefine = flag(S_AutoRedefine)
  
  val setAutoRedefine = setFlag(S_AutoRedefine)
  
  def hidden = flag(S_Hidden)
  
  val setHidden = setFlag(S_Hidden)
  
  def qFormat = flag(S_QFormat)
  
  val setQFormat = setFlag(S_QFormat)
  
  def uiPriority = field(S_UIPriority)
  
  def setUIPriority = setField(S_UIPriority)
  
  def unhideWhenUsed = flag(S_UnhideWhenUsed)
  def setUnhideWhenUsed = setFlag(S_UnhideWhenUsed)
  import StyleOptField.sofOrdering
  def asXML = (      
      <w:style w:styleId={id} w:type={`type`.value}
      	w:default={default.onTrueOrNull("true")}
      	w:customStyle={customStyle.onTrueOrNull("true")}>
			{ name.elem(n =>
			    <w:name w:val={n}/>)
			}
			{
			  if(aliases.isEmpty) { NodeSeq.Empty } else {			
			    <w:aliases w:val={aliases.mkString(",")}/>
			  }
			}			
			{ fields.values.to(IndexedSeq).sortBy(_.ft.pos).map(_.asXML) }
			{ pPr.onSome(_.asXML) }
			{ rPr.onSome(_.asXML) }
			</w:style>
      )        
}
  
final case class LatentStyles(
    count : Option[Int] = None,
    defLockedState : Option[Boolean] = None,
    defQFormat : Option[Boolean] = None,
    defSemiHidden : Option[Boolean] = None,
    defUIPriority : Option[Int] = None,
    defUnhideWhenUsed : Option[Boolean] = None,
    lsdExceptions : Seq[LsdException] = Seq()) extends XmlComponent {
  def asXML = (
      <w:latentStyles>
			</w:latentStyles>
      )
}
    
final case class LsdException(
    name : String,
    locked : Option[Boolean] = None,
    qFormat : Option[Boolean] = None,
    semiHidden : Option[Boolean] = None,
    uiPriority : Option[Int] = None,
    unhideWhenUsed : Option[Boolean] = None) extends XmlComponent {
  def asXML = (
      <w:lsdException w:name={name} 					
					w:locked={locked.optAttr}
					w:qFormat={qFormat.optAttr}
					w:semiHidden={semiHidden.optAttr}
					w:uiPriority={uiPriority.optAttr}
					w:unhideWhenUsed={unhideWhenUsed.optAttr} />
      )
}

final case class DocDefaults(
    pPr : Option[PPr] = None,
    rPr : Option[RPr] = None) extends XmlComponent {
  def asXML = (
      <w:docDefaults>			
			{rPr.onSome(x => <w:rPrDefault>{x.asXML}</w:rPrDefault>)}
			{pPr.onSome(x => <w:pPrDefault>{x.asXML}</w:pPrDefault>)}
			</w:docDefaults>
      )
}

final case class Styles(
    docDefaults : Seq[DocDefaults] = Seq(),
    latentStyles : Option[LatentStyles] = None,
    styles : Seq[Style] = Seq()) extends XmlComponent {
  def asXML = (
      <w:styles xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" mc:Ignorable="w14">
			{docDefaults.map(_.asXML)}
			{latentStyles.onSome(_.asXML)}
			{styles.map(_.asXML)}
			</w:styles>
      )
  def completeWith(base : Styles) : Styles = {
    val alreadyDefined = styles.flatMap(_.name).to(Set)
    val missing = base.styles.filter(s => s.name.isDefined && !alreadyDefined.contains(s.name.get))
    copy(styles = styles ++ missing)
  }
    
}

object DefaultStyles {
  
  private val sb = Seq.newBuilder[Style]
  
  def makePPrStyle(id : String, name : String, link : String = null,basedOn : String = null,
      default : Boolean = false)(pPr : PPr) = {
    val st = Style(id = id, `type` = ST_Paragraph)
      .setName(name)
    val st1 = if(link != null) { st.setLink(link) } else { st }
    val basedOn2 = Option(basedOn).orElse(pPr.pStyle)
    val st2 = basedOn2.map { st1.setBasedOn(_) }.getOrElse{ st1 }
    val st3 = st2      
      .setUIPriority(1)      
      .copy(          
          pPr = Some(pPr.copy(pStyle=None)),
          default = default
      )
    sb += st3
    st3
  }
  
  def makeRPrStyle(id : String, name : String, link : String = null,basedOn : String = null,
      default : Boolean = false)(rPr : RPr) = {
    val st = Style(id = id, `type` = ST_Character)
      .setName(name)
    val st1 = if(link != null) { st.setLink(link) } else { st }
    val basedOn2 = Option(basedOn).orElse(rPr.rStyle)
    val st2 = basedOn2.map { st1.setBasedOn(_) }.getOrElse{ st1 }
    val st3 = st2      
      .setUIPriority(1)      
      .copy(          
          rPr = Some(rPr.copy(rStyle=None)),
          default = default
      )
    sb += st3
    st3
  }
  
  val omissisTabs = Seq(
      Tab( pos = Pts20(72*5.9), tabType = TST_End, leader = TL_Dot ) 
  )
  
  val font = Fonts(
      ascii = Some("Arial"),
      cs = Some("Arial"),
      hAnsi = Some("Arial"))
    
  

  val defaultRPr = RPr(
      fonts = Some(font),
      sz = Some(20),
      szCs = Some(20)
      )
  
  val defInd = Ind(firstLine=Pts20(708.0/20.0))    
      
  val defaultPPr = PPr(      
      ind = Some(defInd)
      ) ;
              
  val docDefault = DocDefaults(
      rPr = Some(defaultRPr),
      pPr = Some(defaultPPr))
  
  val defaultParStyle =
        makePPrStyle("DefaultParagraph","Default Paragraph",
            link = "DefaultCharacter",default=true)(PPr(
                      spacing = Some(Spacing(after=Some(Pts20(6)))),                      
                      tabs = omissisTabs,
                      jc = Some(JC_Both),
                      ind = Some(defInd)
                      ))   
  
  val indentAlteracao1 = Ind(start = Pts20(1134.0/20), firstLine = Pts20(284.0/20))
  val spacingAlteracao1 = Spacing(line = Some(Left(Pts20(12.0))), lineRule=Some(SLR_Auto))

      
                      
  val defaultCharStyle =
    makeRPrStyle("DefaultCharacter","Default Character",
            link = "DefaultParagraph",default=true)(defaultRPr)        
  
  def rPrRef(id : String) = RPr(rStyle = Some(id))
  def pPrRef(id : String) = PPr(pStyle = Some(id))
            
  //Any 
  val pprAny = PPr(
      pStyle = Some(defaultParStyle.id)
  )
  
  //Epígrafe
  val epigrafePPrStyle = makePPrStyle(
      "EpigrafeParagrafo","Epígrafe (parágrafo)",
      link = "EpigrafeCaracter",basedOn = defaultParStyle.id)(
          PPr(        
              jc = Some(JC_Center),
              spacing = Some(
                Spacing(
                  before = Some(Pts20(12)),
                  after = Some(Pts20(12))
                )               
              ),
              ind = Some(Ind(firstLine=Pts20(0)))
           )       
         )
          
  val epigrafeRPrStyle = makeRPrStyle(
        "EpigrafeCaracter", "Epígrafe (caractere)",
        link = "EpigrafeParagrafo",
        basedOn = defaultCharStyle.id)(
            RPr(                              
              sz = Some(24),
              szCs = Some(24),
              capsMode =  Some(CM_Caps),
              bold = Some(true),
              boldCs = Some(true)
           )       
         )      
      
  val pprEpigrafe = pPrRef(epigrafePPrStyle.id)    
    
  val rprEpigrafe = rPrRef(epigrafeRPrStyle.id)
                  
  //Ementa
  
  val ementaPPrStyle = makePPrStyle(
      "EmentaParafrafo","Ementa (parágrafo)",
      link = "EmentaCaracter")(  
    PPr(       
        pStyle = Some(defaultParStyle.id),
        ind = Some(Ind(start=Pts20(198.45),firstLine=Pts20(0))), 
        spacing = Some(Spacing(
            after = Some(Pts20(12)),
            line = Some(Left(Pts20(12))),
            lineRule = Some(SLR_AtLeast)))        
    ))
  
  val ementaRPrStyle = makeRPrStyle(
      "EmentaCaracter","Ementa (caractere)",
      link="EmentaParagrafo")(RPr(
        rStyle = Some(defaultCharStyle.id),
        sz = Some(18),
        szCs = Some(18),
        bold = Some(true),
        italics = Some(true)
    ))
    
  val pprEmenta = pPrRef(ementaPPrStyle.id)
    
  val rprEmenta = rPrRef(ementaRPrStyle.id)
  
  //Preambulo
  val preambuloPPrStyle = makePPrStyle(
      "PreambuloParagrafo","Preambulo (parágrafo)",
      link="PreambuloCaracter")(
    PPr(  
         pStyle = Some(defaultParStyle.id),
         spacing = Some(Spacing(
             before = Some(Pts20(0)),
             after=Some(Pts20(6.0)),
             line=Some(Left(Pts20(12.0))),
             lineRule=Some(SLR_AtLeast)
             )
         )
    ))
  
  val preambuloRPrStyle = makeRPrStyle(
      "PreambuloCaracter","Preambulo (caractere)",
      link="PreambuloParagrafo")(
      RPr(
        rStyle = Some(defaultCharStyle.id)        
    ))
    
  val pprPreambulo = pPrRef(preambuloPPrStyle.id)
  
  val rprPreambulo = rPrRef(preambuloRPrStyle.id)
        
  //Omissis
  val pprOmissis = pPrRef(defaultParStyle.id)
    
    
  //Nome Agrupador
  
  val nomeAgrupadorPPrStyle = makePPrStyle(
      "NomeAgrupadorParagrafo","Nome de Agrupador (parágrafo)",
      link="NomeAgrupadorCaracter")(PPr(
          pStyle = Some(defaultParStyle.id),
          jc = Some(JC_Center),
          ind = Some(Ind(firstLine=Pts20(0)))
      ))
   
  
  val nomeAgrupadorRPrStyle = makeRPrStyle(
      "NomeAgrupadorCaracter","Nome de Agrupador (caracter)",
      link="NomeAgrupadorParagrafo")(RPr(
          rStyle = Some(defaultCharStyle.id),
          italics = Some(true), 
          capsMode = Some(CM_Caps),
          sz = Some(22),
          szCs = Some(22)
      ))
      
  val rprNomeAgrupador = rPrRef(nomeAgrupadorRPrStyle.id)
  
  val pprNomeAgrupador = pPrRef(nomeAgrupadorPPrStyle.id)
  
  //Rotulo Agrupador
  
  val rotuloAgrupadorPPrStyle = makePPrStyle(
      "RotuloAgrupadorParagrafo","Rótulo de Agrupador (parágrafo)",
      link="RotuloAgrupadorCaracter")(PPr(
          pStyle = Some(defaultParStyle.id),
          jc = Some(JC_Center),
          ind = Some(Ind(firstLine=Pts20(0)))
      ))
      
  val rotuloAgrupadorRPrStyle = makeRPrStyle(
      "RotuloAgrupadorCaracter","Rótulo de Agrupador (caracter)",
      link="RotuloAgrupadorParagrafo")(RPr(
          rStyle = Some(defaultCharStyle.id),
          bold = Some(true), 
          capsMode = Some(CM_Caps),
          sz = Some(24),
          szCs = Some(24)
      ))
  
  val pprRotuloAgrupador = pPrRef(rotuloAgrupadorPPrStyle.id)
      
  val rprRotuloAgrupador = rPrRef(rotuloAgrupadorRPrStyle.id) 
      
  
  //Secao e subsecao
  
  val secaoSubsecaoRotuloRPrStyle = makeRPrStyle(
      "SecaoSubsecaoRotuloCaracter","Rótulo de Seção e Subseção (caractere)")(RPr( 
      rStyle = Some(rotuloAgrupadorRPrStyle.id),      
      capsMode = Some(CM_Normal)
  ))
  
  val secaoSubsecaoRPrStyle = makeRPrStyle(
      "SecaoSubsecaoCaracter","Seção e Subseção (caractere)")(RPr( 
      rStyle = Some(defaultCharStyle.id),
      italics = Some(true),
      sz = Some(22),
      szCs = Some(22)
  ))
  
  val rprSecaoSubsecao = rPrRef(secaoSubsecaoRPrStyle.id)
  val rprRotuloSecaoSubsecao = rPrRef(secaoSubsecaoRotuloRPrStyle.id)
  
  //Dispositivos
  
  val conteudoDispositivoPPrStyle = makePPrStyle(
     "ConteudoDispositivoParagrafo","Conteúdo de Dispositivo (caractere)")(
      PPr(
        pStyle = Some(defaultParStyle.id),
        ind = Some(defInd)
      ))
  
  val pprConteudoDispositivo = pPrRef(conteudoDispositivoPPrStyle.id) 
  
  //Artigos
  
  val tituloArtigoRPrStyle = makeRPrStyle(
      "TituloArtigoCaracter","Título de Artigo (caractere)")(
      RPr(
        rStyle = Some(defaultCharStyle.id),
        bold = Some(true),
        boldCs = Some(true)
        ))
  
  val rprTituloArtigo = rPrRef(tituloArtigoRPrStyle.id)
  
  val pprTituloDispositivo = pPrRef("TituloDispositivoParagrafo")
  
  //Remissoes
  
  val linkRemissaoRPrStyle = makeRPrStyle(
      "RemissaoCaracter","Remissão (caractere)")(RPr(
       rStyle = Some(DefaultStyles.defaultCharStyle.id),
       color = Some(RGB(0.2,0.2,0.2)
   )))
  
  val rprLinkRemissao = RPr(       
       color = Some(RGB(0.2,0.2,0.2)
   ))
        
  val styles = Styles(
      docDefaults = Seq(docDefault),
      styles=sb.result
  )
  
  //LocalDataFecho
  
  val localDataFechoPPrStyle = makePPrStyle(
      "LocalDataFechoParagrafo","LocalDataFecho (Paragrafo)",
      link="LocalDataFechoCaracter")(
    PPr(  
         pStyle = Some(defaultParStyle.id),
         spacing = Some(Spacing(
             before = Some(Pts20(0)),
             after=Some(Pts20(6.0)),
             line=Some(Left(Pts20(12.0))),
             lineRule=Some(SLR_AtLeast)
             )
         )
    ))
  
  val localDataFechoRPrStyle = makeRPrStyle(
      "LocalDataFechoCaracter","LocalDataFecho (Caracter)",
      link="LocalDataFechoParagrafo")(
      RPr(
        rStyle = Some(defaultCharStyle.id)        
    ))
    
  val pprLocalDataFecho = pPrRef(localDataFechoPPrStyle.id)
  
  val rprLocalDataFecho = rPrRef(localDataFechoRPrStyle.id)
  
  //AssinaturaTexto
  
  val assinaturaTextoPPrStyle = makePPrStyle(
      "AssinaturaTextoParagrafo","AssinaturaTexto (Paragrafo)",
      link="AssinaturaTextoCaracter")(
    PPr(  
         pStyle = Some(defaultParStyle.id),
         spacing = Some(Spacing(
             before = Some(Pts20(0)),
             after=Some(Pts20(6.0)),
             line=Some(Left(Pts20(12.0))),
             lineRule=Some(SLR_AtLeast)
             )
         )
    ))
  
  val assinaturaTextoRPrStyle = makeRPrStyle(
      "AssinaturaTextoCaracter","AssinaturaTexto (Caracter)",
      link="AssinaturaTextoParagrafo")(
      RPr(
        rStyle = Some(defaultCharStyle.id)        
    ))
    
  val pprAssinaturaTexto = pPrRef(assinaturaTextoPPrStyle.id)
  
  val rprAssinaturaTexto = rPrRef(assinaturaTextoRPrStyle.id)
  
  //FormulaPromulgacao
  
  val formulaPromulgacaoPPrStyle = makePPrStyle(
      "FormulaPromulgacaoParagrafo","FormulaPromulgacao (Paragrafo)",
      link="FormulaPromulgacaoCaracter")(
    PPr(  
         pStyle = Some(defaultParStyle.id),
         spacing = Some(Spacing(
             before = Some(Pts20(0)),
             after=Some(Pts20(6.0)),
             line=Some(Left(Pts20(12.0))),
             lineRule=Some(SLR_AtLeast)
             )
         )
    ))
  
  val formulaPromulgacaoRPrStyle = makeRPrStyle(
      "FormulaPromulgacaoCaracter","FormulaPromulgacao (Caracter)",
      link="FormulaPromulgacaoParagrafo")(
      RPr(
        rStyle = Some(defaultCharStyle.id)        
    ))
    
  val pprFormulaPromulgacao = pPrRef(formulaPromulgacaoPPrStyle.id)
  
  val rprFormulaPromulgacao = rPrRef(formulaPromulgacaoRPrStyle.id)
  
  val rprNotaReferenciada = rPrRef("NotaReferenciada")
}

