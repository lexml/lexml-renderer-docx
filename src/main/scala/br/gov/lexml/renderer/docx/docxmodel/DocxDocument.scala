package br.gov.lexml.renderer.docx.docxmodel

import scala.xml.*
import java.io.*

import scala.annotation.targetName

object XmlUtils:
  def xmlToByteArray(e : Elem): Array[Byte] =
    val w = StringWriter()
    XML.write(w,e,"utf-8",true,null,MinimizeMode.Always)
    w.close()
    w.toString.getBytes("utf-8")

final case class HrefData(href : java.net.URI, id : String, tooltip : Option[String] = None, anchor : Option[String] = None,
    rPr : Option[RPr] = None):
  def toRelationship: Relationship = Relationship(id = id, target = href, targetMode = TM.External, typ = RT.Hyperlink)

object RelationshipType:
  def apply(value : String) : RelationshipType = value match {
    case RT.Hyperlink.value => RT.Hyperlink
    case _ => RT.Other(value)
  }

type RelationshipType = RT
enum RT(val value : String):
  case Hyperlink extends RelationshipType("http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink")
  case Footer extends RelationshipType("http://purl.oclc.org/ooxml/officeDocument/relationships/footer") // footer1.xml
  case Settings extends RelationshipType("http://purl.oclc.org/ooxml/officeDocument/relationships/settings") // settings.xml
  case Header extends RelationshipType("http://purl.oclc.org/ooxml/officeDocument/relationships/header") // header1.xml
  case Styles extends RelationshipType("http://purl.oclc.org/ooxml/officeDocument/relationships/styles") // styles.xml
  case Numbering extends RelationshipType("http://purl.oclc.org/ooxml/officeDocument/relationships/numbering") // numbering.xml
  case Endnotes extends RelationshipType("http://purl.oclc.org/ooxml/officeDocument/relationships/endnotes") // endnotes.xml
  case Footnotes extends RelationshipType("http://purl.oclc.org/ooxml/officeDocument/relationships/footnotes") // footnotes.xml
  case Theme extends RelationshipType("http://purl.oclc.org/ooxml/officeDocument/relationships/theme") // theme/theme1.xml
  case WebSettings extends RelationshipType("http://purl.oclc.org/ooxml/officeDocument/relationships/webSettings") // webSettings.xml
  case FontTable extends RelationshipType("http://purl.oclc.org/ooxml/officeDocument/relationships/fontTable") // fontTable.xml
  case Other(override val value : String) extends RelationshipType(value)


object TargetMode:
  def apply(value : Option[String]) : TargetMode = value match {
    case None => TM.Implicit
    case Some(TM.External.value) => TM.External
    case Some(TM.Implicit.value) => TM.Implicit
    case Some(x) => TM.Other(x)
  }

type TargetMode = TM
enum TM(val isDefault : Boolean = false, val value : String):
  lazy val nonDefaultValue : String = if isDefault then null else value
  case External extends TargetMode(false,"External")
  case Implicit extends TargetMode(true,"Implicit")
  case Other(override val value : String) extends TM(value == Implicit.value,value)
    
final case class Relationship(id : String, typ : RelationshipType, target : java.net.URI, targetMode : TargetMode) extends XmlComponent:
  lazy val asXML : Elem =
      <Relationship xmlns="http://schemas.openxmlformats.org/package/2006/relationships" 
          Id={id} Type={typ.value} Target={target.toString} TargetMode={targetMode.nonDefaultValue}/>

final case class Relationships(relationships : Seq[Relationship] = Seq()) extends XmlComponent:
  lazy val asXML : Elem =
    <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
      {NodeSeq.fromSeq(relationships.map(_.asXML))}
    </Relationships>

object Relationships:
  def fromByteArray(data : Array[Byte]) : Relationships =
    def rel(id : String, targetStr : String, typStr : String, targetModeStr : Option[String]) : Relationship =
      Relationship(id = id,
          target = java.net.URI(targetStr),
          typ = RelationshipType(typStr),
          targetMode = TargetMode(targetModeStr))
    val root = XML.load(java.io.ByteArrayInputStream(data))
    
    val rels = (root \\ "Relationship").to(Seq).collect {
      case e : Elem =>        
        rel(e.attribute("Id").get.head.text,
         e.attribute("Target").get.head.text,
         e.attribute("Type").get.head.text,
         e.attribute("TargetMode").flatMap(_.headOption.map(_.text)))
    }
    Relationships(relationships = rels)

final case class Docx(
    mainDoc : DocxMainDocument = DocxMainDocument(),
    baseRelationships : Relationships = Relationships(),    
    hyperlinks : Seq[HrefData] = Seq(),
    endnotes : Seq[(String,Seq[DocxTextComponent])]= Seq(),
    footnotes : Seq[(String,Seq[DocxTextComponent])] = Seq()):
  def mainDocFile: Array[Byte] = XmlUtils.xmlToByteArray(mainDoc.asXML)

  def relationshipsFile: Array[Byte] = XmlUtils.xmlToByteArray(relationships.asXML)
  
  def files : Map[String,Array[Byte]] = Map(
    "word/document.xml" -> mainDocFile,
    "word/_rels/document.xml.rels" -> relationshipsFile,
    "word/footnotes.xml" -> makeNotesFile("footnotes","footnote",footnotes),
    "word/endnotes.xml" -> makeNotesFile("endnotes","endnote",endnotes)
  )
    
  def makeNotesFile(rootLabel : String, childLabel : String, notes : Seq[(String,Seq[DocxTextComponent])]) : Array[Byte] =
    val innerElems1 = Seq(
      <w:endnote w:type="separator" w:id="-1">
        <w:p>
          <w:r>
            <w:separator/>
          </w:r>
         </w:p>
      </w:endnote>,
      <w:endnote w:type="continuationSeparator" w:id="0">
        <w:p>
           <w:r>
            <w:continuationSeparator/>
          </w:r>
        </w:p>
      </w:endnote>
    )
    val refHead = Seq(
        R(rPr = Some(RPr(rStyle = Some("Refdenotadefim"))),Seq(T("("),EndnoteRef,T(")"))),
        R(rPr = None,Seq(T(" ",true)))
        )
        
    val innerElems2 = notes.map { (id,contents) =>
      val first = contents.head match {
        case p : P =>
          p.replaceParElements(refHead ++ p.parElements)
        case _ : Table => sys.error("Table not supported!")
      }
      val contents2 = first +: contents.tail
      
      <w:note w:id={id}>          
          {NodeSeq.fromSeq(contents2.map(_.asXML))}
      </w:note> 
    }
    val innerElems = (innerElems1 ++ innerElems2).map(_.copy(label = childLabel))    
    val rootElem =
      <w:root xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">
      { NodeSeq.fromSeq(innerElems) }
      </w:root>.copy(label = rootLabel)
    XmlUtils.xmlToByteArray(rootElem)
  end makeNotesFile

  def relationships : Relationships =
    val rels = baseRelationships.relationships ++ hyperlinks.map(_.toRelationship)        
    baseRelationships.copy(relationships = rels)
end Docx

final case class DocxMainDocument(contents : Seq[DocxTextComponent] = Seq())
    extends XmlComponent with DocxTextComponentContainer[DocxMainDocument]:
  override def asXML : Elem =
      <w:document xmlns:o="urn:schemas-microsoft-com:office:office"
                  xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                  xmlns:v="urn:schemas-microsoft-com:vml"
                  xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                  xmlns:w10="urn:schemas-microsoft-com:office:word"
                  xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                  xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape"
                  xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup"
                  xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
                  xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"
                  xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"
                  mc:Ignorable="w14 wp14">
        <w:body>
        {NodeSeq.fromSeq(contents.map(_.asXML))}
        <w:sectPr>
          <w:endnotePr>
            <w:numFmt w:val="chicago"/>
          </w:endnotePr>
          <w:pgSz w:w="11906" w:h="16838"/>
          <w:pgMar w:left="1701" w:right="1701" w:header="0" w:top="1417" w:footer="0" w:bottom="1417" w:gutter="0"/>
          <w:cols w:space="720"/>
          <w:formProt w:val="0"/>
          <w:docGrid w:linePitch="360" w:charSpace="4096"/>
        </w:sectPr>
      </w:body>
    </w:document>

  def flatMap(f : DocxTextComponent => Seq[DocxTextComponent]) : DocxMainDocument =
     DocxMainDocument(contents = contents.flatMap(f))
end DocxMainDocument

sealed trait DocxTextComponent extends Product with XmlComponent:
  def isEmpty : Boolean

trait DocxTextComponentContainer[T]:
  def flatMap(f : DocxTextComponent => Seq[DocxTextComponent]) : T

final case class P(runs : Seq[ParElement] = Seq(),pPr : Option[PPr] = None) extends DocxTextComponent with ParElementContainer[P]:
  lazy val asXML: Elem =
    <w:p>
      {pPr.elem(_.asXML)}
      {runs.eachElem(_.asXML)}
    </w:p>

  def insertFirst(els : ParElement*): P =
    copy(runs = els ++ runs)
  def insertLast(els : ParElement*): P =
    copy(runs = runs ++ els)
  def isEmpty: Boolean = runs.forall(_.isEmpty)
  
  def replaceParElements(pl : Seq[ParElement]): P = copy (runs = pl)

  override val parElements: Seq[ParElement] = runs

final case class TblPr() extends XmlComponent:
  lazy val asXML : Elem = ???

final case class GridCol() extends XmlComponent:
  lazy val asXML : Elem = ???

final case class TblGrid(gridCols : Seq[GridCol] = Seq()) extends XmlComponent:
  lazy val asXML : Elem = ???

final case class TR(trPr : Option[TrPr] = None,
                    cols : Seq[TC] = Seq()) extends XmlComponent:
  lazy val asXML : Elem = ???

final case class TrPr() extends XmlComponent:
  lazy val asXML : Elem = ???

final case class TC(tcPr : Option[TcPr] = None, contents : Seq[DocxTextComponent]) extends XmlComponent:
  lazy val asXML : Elem = ???

final case class TcPr() extends XmlComponent:
  lazy val asXML : Elem = ???

final case class Table(tblPr : Option[TblPr] = None,
                       tblGrid : Option[TblGrid] = None,
                       rows : Seq[TR] = Seq()) extends DocxTextComponent with XmlComponent:
  lazy val asXML: Elem =
    <w:tbl>
      {tblPr.elem(_.asXML)}
      {tblGrid.elem(_.asXML)}
      {rows.eachElem(_.asXML)}
    </w:tbl>
  override def isEmpty: Boolean = rows.isEmpty


sealed trait ParElement extends Product with XmlComponent:
  def isEmpty : Boolean = false

sealed trait ParElementContainer[T <: ParElementContainer[T]]:
  final def flatMap(f : (Option[ParElement],ParElement,Option[ParElement]) => Seq[ParElement]) : T =
    val t = parElements.zipAll(None +: parElements.init.map(Some(_)), null, None).
            zipAll(parElements.tail.map(Some(_)), null, None).collect {
      case ((a,b),c) => (b,a,c)
    }
    replaceParElements(t.flatMap(f.tupled))
  def replaceParElements(pl : Seq[ParElement]) : T
  def parElements : Seq[ParElement]

type CapsMode = CM
enum CM(val ns : NodeSeq):
  def asXML: NodeSeq = ns
  case Caps extends CM(<w:caps w:val="true"/>)
  case SmallCaps extends CM(<w:smallCaps w:val="true"/>)
  case Normal extends CM(NodeSeq.fromSeq(Seq(
      <w:caps w:val="false"/>,
      <w:smallCaps w:val="false"/>
  )))


final case class RGB(red : Double, green : Double, blue : Double):
  lazy val value = f"$r%02X$g%02X$b%02X"
  private val r = RGB.norm(red)
  private val g = RGB.norm(green)
  private val b = RGB.norm(blue)

object RGB:
  private def norm(v : Double) = math.floor(255.0*v).toInt

final case class Fonts(
    ascii : Option[String] = None,
    cs : Option[String] = None,
    hAnsi : Option[String] = None) extends XmlComponent:
  lazy val asXML: Elem =
    <w:rFonts w:ascii={ascii.orNull} w:cs={cs.orNull}
              w:hAnsi={ascii.orNull}/>

type UnderlineOption = UO
enum UO(val elem : Elem) extends XmlComponent:
  override def asXML: Elem = elem
  case NoUnderline extends UO(<w:u w:val="false"/>)
  case Underline(color : Option[RGB] = None) extends UO(<w:u w:val="single" w:color={color.map(_.value).orNull}/>)

type UnderlineStyle = US
enum US(val value : String):
  case Dash extends US("dash")
  case DashDotDotHeavy extends US("dashDotDotHeavy")
  case DashDotHeavy extends US("dashDotHeavy")
  case DashedHeavy extends US("dashedHeavy")
  case DashLong extends US("dashLong")
  case DashLongHeavy extends US("dashLongHeavy")
  case DotDash extends US("dotDash")
  case DotDotDash extends US("dotDotDash")
  case Dotted extends US("dotted")
  case DottedHeavy extends US("dottedHeavy")
  case Double extends US("double")
  case None extends US("none")
  case Single extends US("single")
  case Thick extends US("thick")
  case Wave extends US("wave")
  case WavyDouble extends US("wavyDouble")
  case WavyHeavy extends US("wavyHeavy")
  case Words extends US("words")

type VertAlignment = VA
enum VA(val value : String):
  case Superscript extends VA("superscript")
  case Subscript extends VA("subscript")
  case Baseline extends VA("baseline")

final case class RPr(
    bold : Option[Boolean] = None,
    boldCs : Option[Boolean] = None,
    italics : Option[Boolean] = None,
    italicsCs : Option[Boolean] = None,
    capsMode : Option[CapsMode] = None,
    color : Option[RGB] = None,
    lang : Option[String] = None,
    fonts : Option[Fonts] = None,
    rStyle : Option[String] = None,
    strike : Option[Boolean] = None,
    sz : Option[Int] = None,
    kern : Option[Int] = None,
    szCs : Option[Int] = None,
    underline : Option[UnderlineOption] = None,
    vertAlign : Option[VertAlignment] = None
    ) extends XmlComponent:
  lazy val asXML: Elem =
    <w:rPr>
      {bold.elem(x => <w:b w:val={x.toString}/>)}
      {italics.elem(x => <w:i w:val={x.toString}/>)}
      {boldCs.elem(x => <w:bCs w:val={x.toString}/>)}
      {italicsCs.elem(x => <w:iCs w:val={x.toString}/>)}
      {capsMode.map(_.asXML).getOrElse(NodeSeq.Empty)}
      {color.elem(v => <w:color w:val={v.value}/>)}
      {lang.elem(x => <w:lang w:val={x.toString}/>)}
      {fonts.elem(_.asXML)}
      {rStyle.elem(x => <w:rStyle w:val={x}/>)}
      {strike.elem(x => <w:strike w:val={x.toString}/>)}
      {sz.elem(x => <w:sz w:val={x.toString}/>)}
      {szCs.elem(x => <w:szCs w:val={x.toString}/>)}
      {kern.elem(x => <w:kern w:val={x.toString}/>)}
      {vertAlign.elem(x => <w:vertAlign w:val={x.value}/>)}
      {underline.elem(_.asXML)}
    </w:rPr>

  @targetName("plus")
  def +(x : RPr): RPr = RPr(
      bold = x.bold orElse bold,
      italics = x.italics orElse italics,
      boldCs = x.boldCs orElse boldCs,
      italicsCs = x.italicsCs orElse italicsCs,
      capsMode = x.capsMode orElse capsMode,
      color = x.color orElse color,
      lang = x.lang orElse lang,
      fonts = x.fonts orElse fonts,
      rStyle = x.rStyle orElse rStyle,
      strike = x.strike orElse strike,
      sz = x.sz orElse sz,
      szCs = x.szCs orElse szCs,
      underline = x.underline orElse underline,
      vertAlign = x.vertAlign orElse vertAlign)
end RPr

final case class R(
    rPr : Option[RPr] = None,
    contents : Seq[RunContent] = Seq()) extends ParElement with RunContentContainer[R]:
  lazy val asXML: Elem =
      <w:r>
      {rPr.onSome(_.asXML)}
      {contents.map(_.asXML)}
      </w:r>

  def insertFirst(els : RunContent*): R = copy(contents = els ++ contents)
  def insertLast(els : RunContent*): R = copy(contents = contents ++ els)
  override def isEmpty: Boolean = contents.forall(_.isEmpty)
  def flatMap(f : RunContent => Seq[RunContent]) : R = 
    copy(contents = contents.flatMap(f))


abstract sealed class RunContent extends Product with XmlComponent:
  def isEmpty : Boolean = false

trait RunContentContainer[+T]:
  def flatMap(f : RunContent => Seq[RunContent]) : T
  def contents : Seq[RunContent]

case object Br extends RunContent:
  val asXML: Elem = <w:br/>

final case class DelText(text : String, preserveSpace : Boolean = false) extends RunContent:
  lazy val asXML: Elem =
      <w:delText xml:space={if(preserveSpace) { "preserve" } else {  null }}>{text}</w:delText>

final case class Del(id : String, content : Seq[ParElement] = Seq(), author : Option[String] = None, date : Option[java.time.ZonedDateTime] = None
    ) extends RunContent with ParElementContainer[Del]:
  lazy val asXML : Elem =
      <w:del w:id={id} w:author={author.orNull} date={date.map(_.toString).orNull}>
    {NodeSeq.fromSeq(content.map(_.asXML))}
    </w:del>

  def replaceParElements(pl : Seq[ParElement]): Del = copy (content = pl)
   
  override val parElements: Seq[ParElement] = content


final case class Ins(id : String, content : Seq[ParElement] = Seq(), author : Option[String] = None, date : Option[java.time.ZonedDateTime] = None
    ) extends RunContent with ParElementContainer[Ins]:
  lazy val asXML: Elem =
      <w:ins w:id={id} w:author={author.orNull} date={date.map(_.toString).orNull}>
        {NodeSeq.fromSeq(content.map(_.asXML))}
      </w:ins>

  def replaceParElements(pl : Seq[ParElement]): Ins = copy (content = pl)
  override val parElements: Seq[ParElement] = content

final case class Hyperlink(
    content : Seq[ParElement] = Seq(), 
    anchor : Option[String] = None, 
    id : Option[String] = None, 
    tooltip : Option[String] = None) extends ParElement 
  with ParElementContainer[Hyperlink]:
  lazy val asXML: Elem =
    <w:hyperlink r:id={id.orNull} w:anchor={anchor.orNull} w:tooltip={tooltip.orNull}>
      {NodeSeq.fromSeq(content.map(_.asXML))}
    </w:hyperlink>
  def replaceParElements(pl : Seq[ParElement]): Hyperlink = copy (content = pl)
  override val parElements: Seq[ParElement] = content

case object NoBreakHyphen extends RunContent:
  val asXML: Elem = <w:noBreakHyphen/>

final case class Sym(font : String, char : String) extends RunContent:
  val asXML: Elem = <w:sym w:font={font} w:char={char}/>

final case class T(text : String, preserveSpace : Boolean = false) extends RunContent:
  lazy val asXML: Elem = <w:t xml:space={if preserveSpace then "preserve" else  null}>{text}</w:t>
  override val isEmpty: Boolean = text.isBlank


case object EndnoteRef extends RunContent:
  val asXML: Elem = <w:endnoteRef/>

case object FootnoteRef extends RunContent:
  val asXML: Elem = <w:footnoteRef/>

type PTabAlignment = PTA
enum PTA(val value : String):
  case Left extends PTA("left")
  case Center extends PTA("center")
  case Right extends PTA("right")

enum PTabBase(val value : String):
  case Indent extends PTabBase("indent")
  case Margin extends PTabBase("margin")

final case class PTab(
    alignment : PTabAlignment,
    leader : Option[TabLeader] = None,
    relativeTo : PTabBase) extends RunContent:
  val asXML: Elem = <w:ptab w:alignment={alignment.value}
                            w:leader={leader.map(_.value).optAttr}
                            w:relativeTo={relativeTo.value}/>

/**
 *
 * <Relationships ... >
...
<Relationship Id="rId8" TargetMode="Internal"
Type="http://purl.oclc.org/ooxml/officeDocument/relationships/customXml"
Target="math1.xml" />
...
</Relationships>
 *
 */
final case class ContentPart(id : String) extends RunContent:
  val asXML: Elem = <w:contentPart r:id={id}/>

final case class Ind(
      start : Pts20 = Pts20(0), 
      end : Pts20 = Pts20(0),
      hanging : Pts20 = Pts20(0),
      firstLine : Pts20 = Pts20(0)//twentieths of a point
      ) extends XmlComponent:
  lazy val asXML: Elem =
    <w:ind w:left={start.nzValue}
      w:right={end.nzValue}
      w:hanging={hanging.nzValue}
          w:firstLine={firstLine.nzValue}/>

enum ST_Jc(val v : String):
  case JC_Both extends ST_Jc("both")
  case JC_Center extends ST_Jc("center")
  case JC_Distribute extends ST_Jc("distribute")
  case JC_End extends ST_Jc("end")
  case JC_NumTab extends ST_Jc("numTab")
  case JC_Start extends ST_Jc("start")

type TabLeader = TL
enum TL(val value : String):
  case  Dot extends TL("dot")
  case  Heavy extends TL("heavy")
  case  Hyphen extends TL("hyphen")
  case  MiddleDot extends TL("middleDot")
  case  None extends TL("none")
  case  Underscore extends TL("underscore")

type TabStopStyle = TST
enum TST(val value : String):
  /**
   * Specifies that the current tab stop shall result in a
  location in the document where all following text is
  aligned to its trailing edge (i.e. all text runs following
  this tab stop and preceding the next tab stop shall be
  aligned against the trailing edge with respect to the
  tab stop location). [Example: In an RTL paragraph, the
  trailing edge is the left edge, so text aligns to that
  edge, extending to the right. end example]
   */
  case End extends TST("end")

  /*
   * Specifies that the current tab stop shall result in a
  location in the document where all following text is
  aligned to its leading edge (i.e. all text runs following
  this tab stop and preceding the next tab stop shall be
  aligned against the leading edge with respect to the
  tab stop location).
   */
  case Start extends TST("start")

final case class TabElem(
    pos : Pts20 = Pts20(0),
    tabType : TST = TST.Start,
    leader : TabLeader = TL.None
    ):
  lazy val asXML : Elem =
    <w:tab w:leader={leader.value} w:pos={pos.value} w:val={tabType.value}/>

case object TAB extends RunContent:
  def asXML: Elem = <w:tab/>

final case class FootnoteReference(id : String) extends RunContent:
  def asXML: Elem = <w:foornoteReference w:id={id}/>

final case class EndnoteReference(id : String) extends RunContent:
  def asXML: Elem = <w:endnoteReference w:id={id}/>

type SpacingLineRule = SLR
enum SLR(val value : String):
  case AtLeast extends SLR("atLeast")
  case Exactly extends SLR("exactly")
  case Auto extends SLR("auto")

abstract sealed class Measure[T <: Measure[T]](ratio : Double) extends Product:
  val v : Double  
  val rounded: Int = math.round(ratio*v).toInt
  val value: String = rounded.toString
  val nzValue: String = if (rounded > 0) { value } else { null }

abstract sealed class Pts[T <: Pts[T]](ratio : Double) extends Measure[T](ratio)

final case class Pts20(v : Double) extends Pts[Pts20](20) 

abstract sealed class Lines[T <: Lines[T]](ratio : Double) extends Measure[T](ratio)

final case class Lines100(v : Double) extends Lines[Lines100](100)

final case class Lines240(v : Double) extends Lines[Lines240](240)

final case class Spacing(
    after : Option[Pts20] = None, // 1/20 pt
    afterLines : Option[Lines100] = None, // 1/100 line
    before : Option[Pts20] = None, // 1/20 pt
    beforeLines : Option[Lines100] = None, // 1/100 line
    line : Option[Either[Pts20,Lines240]] = None, // 1/20 pt (for lineRule = atLeast or exactly) or 1/240 line if (auto)
    lineRule : Option[SpacingLineRule] = None
    ) extends XmlComponent:
  def r(x : Option[Measure[_]]) : String = x.map(_.value).orNull
  def asXML: Elem =
      <w:spacing
        w:after={r(after)}
        w:afterLines={r(afterLines)}
        w:before={r(before)}
        w:beforeLines={r(beforeLines)}
        w:line={r(line.map(_.fold(x => x,x => x)))}
        w:lineRule={lineRule.map(_.value).optAttr}/>

final case class PPr(
    divId : Option[String] = None,
    ind : Option[Ind] = None,
    jc : Option[ST_Jc] = None,    
    tabs : Seq[TabElem] = Seq(),
    spacing : Option[Spacing] = None,
    widowControl : Boolean = true,
    qFormat : Boolean = true,
    pStyle : Option[String] = None
    ) extends XmlComponent:
  lazy val asXML : Elem =
    <w:pPr>
      {pStyle.elem(n => <w:pStyle w:val={n}/>)}
      {if(widowControl) { <w:widowControl/> } else { NodeSeq.Empty }}
      {cond(tabs.nonEmpty)(<w:tabs>{tabs.eachElem(_.asXML)}</w:tabs>)}
      {spacing.elem(_.asXML)}
      {ind.elem(_.asXML)}
      {jc.elem(x => <w:jc w:val={x.v}/>)}
      {divId.elem(n => <w:divId w:val={n}/>)}
    </w:pPr>