package br.gov.lexml.renderer.docx.docxmodel

import scala.annotation.unused
import scala.xml.*
import scala.reflect.ClassTag


final case class StyleOptField[T](
    ft : StyleFieldType[T],value : T)
     extends XmlComponent with Comparable[Any]:
  this.ensuring(value!=null)
  def asXML: Elem = ft.toXML(value)
  override def compareTo(x : Any) : Int = x match {
    case f : StyleOptField[_] => ft.compareTo(f.ft)
    case _ => -1
  }

object StyleOptField:
  given sofOrdering[T] : Ordering[StyleOptField[T]] =
    (x: StyleOptField[T], y: StyleOptField[T]) => x.ft.compareTo(y.ft)

sealed abstract class StyleFieldType[T](
    val name : String,
    val pos : Int)
  (using ct : ClassTag[T]) extends Comparable[Any]:
  def toXML(value : T) : Elem
  override def compareTo(x : Any) : Int = x match {
    case ft : StyleFieldType[_] => pos.compareTo(ft.pos)
    case _ => -1
  }

sealed abstract class StyleValFieldType[T]
    (name : String,pos : Int)
    (using ct : ClassTag[T])
  extends StyleFieldType[T](name,pos):
  override def toXML(value : T) : Elem =
    <w:elem w:val={value.toString} />.copy(label = name)


enum SSFT(name : String, pos : Int) extends StyleValFieldType[String](name,pos):
  case S_BasedOn extends SSFT("basedOn", 3)
  case S_Next extends SSFT("next", 4)
  case S_Link extends SSFT("link", 5)

type StyleStringFieldType = SSFT

enum SIFT(name : String, pos : Int)
  extends StyleValFieldType[Int](name,pos):
  case S_UIPriority extends SIFT("uiPriority",8)

type StyleIntFieldType = SIFT

enum SFT(name : String, pos : Int)
  extends StyleFieldType[Unit](name,pos):
  case S_AutoRedefine extends SFT("autoRedefine", 6)

  case S_Hidden extends SFT("hidden", 7)

  case S_QFormat extends SFT("qFormat", 11)

  case S_SemiHidden extends SFT("semiHidden", 9)

  case S_UnhideWhenUsed extends SFT("unhideWhenUsed", 10)

  case S_Locked extends SFT("locked", 12)
  def toXML(value : Unit) : Elem = (<w:x/>).copy(label = name)


type StyleFlagType = SFT

enum ST(val value : String):
  case Paragraph extends ST("paragraph")
  case Character extends ST("character")
  case Table extends ST("table")
  case Numbering extends ST("numbering")

type StyleType = ST

final case class Style(
    `type` : ST,
    id : String,
    aliases : Set[String] = Set(),
    name : Option[String] = None,
    fields : Map[StyleFieldType[_],StyleOptField[_]] = Map(
        SFT.S_QFormat -> StyleOptField(SFT.S_QFormat,())),
    customStyle : Boolean = false,
    default : Boolean = false,
    pPr : Option[PPr] = None,
    rPr : Option[RPr] = None
) extends XmlComponent:
  def field[Q](ft : StyleFieldType[Q])
    (implicit ctv : ClassTag[Q]) : Option[Q] =    
    fields.get(ft).map(_.value).collect {
      case ctv(x : Q) => x
    }
  
  def setField[Q](ft : StyleFieldType[Q])(v : Q) : Style =
    copy(fields = fields + (ft -> StyleOptField(ft,v)))
    
  def clearField[Q](ft : StyleFieldType[Q]): Style =
    copy (fields = fields - ft)
    
  def flag(st : StyleFlagType): Boolean = fields.contains(st)
  def setFlag(st : StyleFlagType): Boolean => Style = (v : Boolean) =>
    if(v) { setField(st)(()) } else { clearField(st) } 
      
      
  def setName(n : String): Style = copy(name=Some(n))

  @unused
  def basedOn: Option[String] = field(SSFT.S_BasedOn)

  val setBasedOn: String => Style = setField(SSFT.S_BasedOn)
  
  @unused
  def link: Option[String] = field(SSFT.S_Link)
  
  val setLink: String => Style = setField(SSFT.S_Link)
  
  @unused
  def next: Option[String] = field(SSFT.S_Next)
  
  @unused
  val setNext: String => Style = setField(SSFT.S_Next)
      
  @unused
  def autoRedefine: Boolean = flag(SFT.S_AutoRedefine)
  
  @unused
  val setAutoRedefine: Boolean => Style = setFlag(SFT.S_AutoRedefine)
  
  @unused
  def hidden: Boolean = flag(SFT.S_Hidden)
  
  @unused
  val setHidden: Boolean => Style = setFlag(SFT.S_Hidden)
  
  @unused
  def qFormat: Boolean = flag(SFT.S_QFormat)
  
  @unused
  val setQFormat: Boolean => Style = setFlag(SFT.S_QFormat)
  
  @unused
  def uiPriority: Option[Int] = field(SIFT.S_UIPriority)
  
  def setUIPriority(n : Int) : Style = setField(SIFT.S_UIPriority)(n)
  
  @unused
  def unhideWhenUsed: Boolean = flag(SFT.S_UnhideWhenUsed)
  @unused
  def setUnhideWhenUsed(v : Boolean) : Style = setFlag(SFT.S_UnhideWhenUsed)(v)

  import StyleOptField.sofOrdering
  def asXML : Elem =
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
end Style
  
final case class LatentStyles(
    count : Option[Int] = None,
    defLockedState : Option[Boolean] = None,
    defQFormat : Option[Boolean] = None,
    defSemiHidden : Option[Boolean] = None,
    defUIPriority : Option[Int] = None,
    defUnhideWhenUsed : Option[Boolean] = None,
    lsdExceptions : Seq[LsdException] = Seq()) extends XmlComponent:
  def asXML : Elem = <w:latentStyles></w:latentStyles>

    
final case class LsdException(
    name : String,
    locked : Option[Boolean] = None,
    qFormat : Option[Boolean] = None,
    semiHidden : Option[Boolean] = None,
    uiPriority : Option[Int] = None,
    unhideWhenUsed : Option[Boolean] = None) extends XmlComponent:
  def asXML : Elem =
      <w:lsdException w:name={name}                
               w:locked={locked.optAttr}
               w:qFormat={qFormat.optAttr}
               w:semiHidden={semiHidden.optAttr}
               w:uiPriority={uiPriority.optAttr}
               w:unhideWhenUsed={unhideWhenUsed.optAttr} />

final case class DocDefaults(
    pPr : Option[PPr] = None,
    rPr : Option[RPr] = None) extends XmlComponent:
  def asXML : Elem =
      <w:docDefaults>         
         {rPr.onSome(x => <w:rPrDefault>{x.asXML}</w:rPrDefault>)}
         {pPr.onSome(x => <w:pPrDefault>{x.asXML}</w:pPrDefault>)}
         </w:docDefaults>

final case class Styles(
    docDefaults : Seq[DocDefaults] = Seq(),
    latentStyles : Option[LatentStyles] = None,
    styles : Seq[Style] = Seq()) extends XmlComponent:
  def asXML  : Elem =
      <w:styles xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" mc:Ignorable="w14">
         {docDefaults.map(_.asXML)}
         {latentStyles.onSome(_.asXML)}
         {styles.map(_.asXML)}
         </w:styles>
  def completeWith(base : Styles) : Styles =
    val alreadyDefined = styles.flatMap(_.name).to(Set)
    val missing = base.styles.filter(s => s.name.isDefined && !alreadyDefined.contains(s.name.get))
    copy(styles = styles ++ missing)

object DefaultStyles:
  
  private val sb = Seq.newBuilder[Style]
  
  def makePPrStyle(id : String, name : String, link : String = null,basedOn : String = null,
      default : Boolean = false)(pPr : PPr) : Style =
    val st = Style(id = id, `type` = ST.Paragraph)
      .setName(name)
    val st1 = if(link != null) { st.setLink(link) } else { st }
    val basedOn2 = Option(basedOn).orElse(pPr.pStyle)
    val st2 : Style = basedOn2.map { st1.setBasedOn(_) }.getOrElse{ st1 }
    val st3 = st2.setUIPriority(1).copy(
          pPr = Some(pPr.copy(pStyle=None)),
          default = default
      )
    sb += st3
    st3
  
  def makeRPrStyle(id : String, name : String, link : String = null,basedOn : String = null,
      default : Boolean = false)(rPr : RPr) : Style =
    val st = Style(id = id, `type` = ST.Character)
      .setName(name)
    val st1 = if(link != null) { st.setLink(link) } else { st }
    val basedOn2 = Option(basedOn).orElse(rPr.rStyle)
    val st2 = basedOn2.map { st1.setBasedOn(_) }.getOrElse{ st1 }
    val st3 = st2.setUIPriority(1).copy(
          rPr = Some(rPr.copy(rStyle=None)),
          default = default
      )
    sb += st3
    st3

  val omissisTabs: Seq[TabElem] = Seq(
    TabElem(pos = Pts20(72 * 5.9), tabType = TST.End, leader = TL.Dot)
  )

  val font: Fonts = Fonts(
    ascii = Some("Arial"),
    cs = Some("Arial"),
    hAnsi = Some("Arial"))

  val defaultRPr: RPr = RPr(
    fonts = Some(font),
    sz = Some(20),
    szCs = Some(20)
  )
  
  val defInd : Ind = Ind(firstLine=Pts20(708.0/20.0))
      
  val defaultPPr : PPr = PPr(
      ind = Some(defInd)
      ) ;

  val docDefault: DocDefaults = DocDefaults(
    rPr = Some(defaultRPr),
    pPr = Some(defaultPPr))
  
  val defaultParStyle : Style =
        makePPrStyle("DefaultParagraph","Default Paragraph",
            link = "DefaultCharacter",default=true)(PPr(
                      spacing = Some(Spacing(after=Some(Pts20(6)))),                      
                      tabs = omissisTabs,
                      jc = Some(ST_Jc.JC_Both),
                      ind = Some(defInd)
                      ))   
  
  val indentAlteracao1 : Ind = Ind(start = Pts20(1134.0/20), firstLine = Pts20(284.0/20))
  val spacingAlteracao1 : Spacing = Spacing(line = Some(Left(Pts20(12.0))), lineRule=Some(SLR.Auto))

  val defaultCharStyle : Style =
    makeRPrStyle("DefaultCharacter","Default Character",
            link = "DefaultParagraph",default=true)(defaultRPr)        
  
  def rPrRef(id : String) : RPr = RPr(rStyle = Some(id))
  def pPrRef(id : String) : PPr = PPr(pStyle = Some(id))
            
  //Any 
  val pprAny : PPr = PPr(
      pStyle = Some(defaultParStyle.id)
  )
  
  //Epígrafe
  val epigrafePPrStyle : Style = makePPrStyle(
      "EpigrafeParagrafo","Epígrafe (parágrafo)",
      link = "EpigrafeCaracter",basedOn = defaultParStyle.id)(
          PPr(        
              jc = Some(ST_Jc.JC_Center),
              spacing = Some(
                Spacing(
                  before = Some(Pts20(12)),
                  after = Some(Pts20(12))
                )               
              ),
              ind = Some(Ind(firstLine=Pts20(0)))
           )       
         )
          
  val epigrafeRPrStyle : Style = makeRPrStyle(
        "EpigrafeCaracter", "Epígrafe (caractere)",
        link = "EpigrafeParagrafo",
        basedOn = defaultCharStyle.id)(
            RPr(                              
              sz = Some(24),
              szCs = Some(24),
              capsMode =  Some(CM.Caps),
              bold = Some(true),
              boldCs = Some(true)
           )       
         )      
      
  val pprEpigrafe : PPr = pPrRef(epigrafePPrStyle.id)
    
  val rprEpigrafe : RPr = rPrRef(epigrafeRPrStyle.id)
                  
  //Ementa
  
  val ementaPPrStyle : Style = makePPrStyle(
      "EmentaParafrafo","Ementa (parágrafo)",
      link = "EmentaCaracter")(  
    PPr(       
        pStyle = Some(defaultParStyle.id),
        ind = Some(Ind(start=Pts20(198.45),firstLine=Pts20(0))), 
        spacing = Some(Spacing(
            after = Some(Pts20(12)),
            line = Some(Left(Pts20(12))),
            lineRule = Some(SLR.AtLeast)))
    ))
  
  val ementaRPrStyle : Style = makeRPrStyle(
      "EmentaCaracter","Ementa (caractere)",
      link="EmentaParagrafo")(RPr(
        rStyle = Some(defaultCharStyle.id),
        sz = Some(18),
        szCs = Some(18),
        bold = Some(true),
        italics = Some(true)
    ))
    
  val pprEmenta: PPr = pPrRef(ementaPPrStyle.id)
    
  val rprEmenta: RPr = rPrRef(ementaRPrStyle.id)
  
  //Preambulo
  val preambuloPPrStyle: Style = makePPrStyle(
      "PreambuloParagrafo","Preambulo (parágrafo)",
      link="PreambuloCaracter")(
    PPr(  
         pStyle = Some(defaultParStyle.id),
         spacing = Some(Spacing(
             before = Some(Pts20(0)),
             after=Some(Pts20(6.0)),
             line=Some(Left(Pts20(12.0))),
             lineRule=Some(SLR.AtLeast)
             )
         )
    ))
  
  val preambuloRPrStyle: Style = makeRPrStyle(
      "PreambuloCaracter","Preambulo (caractere)",
      link="PreambuloParagrafo")(
      RPr(
        rStyle = Some(defaultCharStyle.id)        
    ))
    
  val pprPreambulo: PPr = pPrRef(preambuloPPrStyle.id)
  
  val rprPreambulo: RPr = rPrRef(preambuloRPrStyle.id)
        
  //Omissis
  val pprOmissis: PPr = pPrRef(defaultParStyle.id)
    
    
  //Nome Agrupador
  
  val nomeAgrupadorPPrStyle: Style = makePPrStyle(
      "NomeAgrupadorParagrafo","Nome de Agrupador (parágrafo)",
      link="NomeAgrupadorCaracter")(PPr(
          pStyle = Some(defaultParStyle.id),
          jc = Some(ST_Jc.JC_Center),
          ind = Some(Ind(firstLine=Pts20(0)))
      ))
   
  
  val nomeAgrupadorRPrStyle: Style = makeRPrStyle(
      "NomeAgrupadorCaracter","Nome de Agrupador (caracter)",
      link="NomeAgrupadorParagrafo")(RPr(
          rStyle = Some(defaultCharStyle.id),
          italics = Some(true), 
          capsMode = Some(CM.Caps),
          sz = Some(22),
          szCs = Some(22)
      ))
      
  val rprNomeAgrupador: RPr = rPrRef(nomeAgrupadorRPrStyle.id)
  
  val pprNomeAgrupador: PPr = pPrRef(nomeAgrupadorPPrStyle.id)
  
  //Rotulo Agrupador
  
  val rotuloAgrupadorPPrStyle: Style = makePPrStyle(
      "RotuloAgrupadorParagrafo","Rótulo de Agrupador (parágrafo)",
      link="RotuloAgrupadorCaracter")(PPr(
          pStyle = Some(defaultParStyle.id),
          jc = Some(ST_Jc.JC_Center),
          ind = Some(Ind(firstLine=Pts20(0)))
      ))
      
  val rotuloAgrupadorRPrStyle: Style = makeRPrStyle(
      "RotuloAgrupadorCaracter","Rótulo de Agrupador (caracter)",
      link="RotuloAgrupadorParagrafo")(RPr(
          rStyle = Some(defaultCharStyle.id),
          bold = Some(true), 
          capsMode = Some(CM.Caps),
          sz = Some(24),
          szCs = Some(24)
      ))
  
  val pprRotuloAgrupador: PPr = pPrRef(rotuloAgrupadorPPrStyle.id)
      
  val rprRotuloAgrupador: RPr = rPrRef(rotuloAgrupadorRPrStyle.id)
      
  
  //Secao e subsecao
  
  val secaoSubsecaoRotuloRPrStyle: Style = makeRPrStyle(
      "SecaoSubsecaoRotuloCaracter","Rótulo de Seção e Subseção (caractere)")(RPr( 
      rStyle = Some(rotuloAgrupadorRPrStyle.id),      
      capsMode = Some(CM.Normal)
  ))
  
  val secaoSubsecaoRPrStyle: Style = makeRPrStyle(
      "SecaoSubsecaoCaracter","Seção e Subseção (caractere)")(RPr( 
      rStyle = Some(defaultCharStyle.id),
      italics = Some(true),
      sz = Some(22),
      szCs = Some(22)
  ))
  
  val rprSecaoSubsecao: RPr = rPrRef(secaoSubsecaoRPrStyle.id)
  val rprRotuloSecaoSubsecao: RPr = rPrRef(secaoSubsecaoRotuloRPrStyle.id)
  
  //Dispositivos
  
  val conteudoDispositivoPPrStyle: Style = makePPrStyle(
     "ConteudoDispositivoParagrafo","Conteúdo de Dispositivo (caractere)")(
      PPr(
        pStyle = Some(defaultParStyle.id),
        ind = Some(defInd)
      ))
  
  val pprConteudoDispositivo: PPr = pPrRef(conteudoDispositivoPPrStyle.id)
  
  //Artigos
  
  val tituloArtigoRPrStyle: Style = makeRPrStyle(
      "TituloArtigoCaracter","Título de Artigo (caractere)")(
      RPr(
        rStyle = Some(defaultCharStyle.id),
        bold = Some(true),
        boldCs = Some(true)
        ))
  
  val rprTituloArtigo: RPr = rPrRef(tituloArtigoRPrStyle.id)
  
  val pprTituloDispositivo: PPr = pPrRef("TituloDispositivoParagrafo")
  
  //Remissoes
  
  val linkRemissaoRPrStyle: Style = makeRPrStyle(
      "RemissaoCaracter","Remissão (caractere)")(
        RPr(rStyle = Some(DefaultStyles.defaultCharStyle.id),
            color = Some(RGB(0,0,0.8)),
            underline = Some(UO.Underline(Some(RGB(0,0,0.8))))
        )
  )
  
  val rprLinkRemissao: RPr = RPr(
    color = Some(RGB(0, 0, 0.8)),
    underline = Some(UO.Underline())
  )
        
  val styles: Styles = Styles(
      docDefaults = Seq(docDefault),
      styles=sb.result()
  )
  
  //LocalDataFecho
  
  val localDataFechoPPrStyle: Style = makePPrStyle(
      "LocalDataFechoParagrafo","LocalDataFecho (Paragrafo)",
      link="LocalDataFechoCaracter")(
    PPr(  
         pStyle = Some(defaultParStyle.id),
         spacing = Some(Spacing(
             before = Some(Pts20(0)),
             after=Some(Pts20(6.0)),
             line=Some(Left(Pts20(12.0))),
             lineRule=Some(SLR.AtLeast)
             )
         )
    ))
  
  val localDataFechoRPrStyle: Style = makeRPrStyle(
      "LocalDataFechoCaracter","LocalDataFecho (Caracter)",
      link="LocalDataFechoParagrafo")(
      RPr(
        rStyle = Some(defaultCharStyle.id)        
    ))
    
  val pprLocalDataFecho: PPr = pPrRef(localDataFechoPPrStyle.id)
  
  val rprLocalDataFecho: RPr = rPrRef(localDataFechoRPrStyle.id)
  
  //AssinaturaTexto
  
  val assinaturaTextoPPrStyle: Style = makePPrStyle(
      "AssinaturaTextoParagrafo","AssinaturaTexto (Paragrafo)",
      link="AssinaturaTextoCaracter")(
    PPr(  
         pStyle = Some(defaultParStyle.id),
         spacing = Some(Spacing(
             before = Some(Pts20(0)),
             after=Some(Pts20(6.0)),
             line=Some(Left(Pts20(12.0))),
             lineRule=Some(SLR.AtLeast)
             )
         )
    ))
  
  val assinaturaTextoRPrStyle: Style = makeRPrStyle(
      "AssinaturaTextoCaracter","AssinaturaTexto (Caracter)",
      link="AssinaturaTextoParagrafo")(
      RPr(
        rStyle = Some(defaultCharStyle.id)        
    ))
    
  val pprAssinaturaTexto: PPr = pPrRef(assinaturaTextoPPrStyle.id)
  
  val rprAssinaturaTexto: RPr = rPrRef(assinaturaTextoRPrStyle.id)
  
  //FormulaPromulgacao
  
  val formulaPromulgacaoPPrStyle: Style = makePPrStyle(
      "FormulaPromulgacaoParagrafo","FormulaPromulgacao (Paragrafo)",
      link="FormulaPromulgacaoCaracter")(
    PPr(  
         pStyle = Some(defaultParStyle.id),
         spacing = Some(Spacing(
             before = Some(Pts20(0)),
             after=Some(Pts20(6.0)),
             line=Some(Left(Pts20(12.0))),
             lineRule=Some(SLR.AtLeast)
             )
         )
    ))
  
  val formulaPromulgacaoRPrStyle: Style = makeRPrStyle(
      "FormulaPromulgacaoCaracter","FormulaPromulgacao (Caracter)",
      link="FormulaPromulgacaoParagrafo")(
      RPr(
        rStyle = Some(defaultCharStyle.id)        
    ))
    
  val pprFormulaPromulgacao: PPr = pPrRef(formulaPromulgacaoPPrStyle.id)
  
  val rprFormulaPromulgacao: RPr = rPrRef(formulaPromulgacaoRPrStyle.id)
  
  val rprNotaReferenciada: RPr = rPrRef("NotaReferenciada")
end DefaultStyles


