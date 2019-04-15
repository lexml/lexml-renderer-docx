package br.gov.lexml.renderer.docx.docxmodel

import scala.xml._
import java.io._
import org.apache.commons.io.IOUtils

object XmlUtils {
  def xmlToByteArray(e : Elem) = {
    val w = new StringWriter()
    XML.write(w,e,"utf-8",true,null,MinimizeMode.Always)
    w.close()
    w.toString().getBytes("utf-8")
  }
}


final case class DocxMainDocument(contents : Seq[DocxTextComponent] = Seq())
    extends XmlComponent with DocxTextComponentContainer[DocxMainDocument] {
  override def asXML : Elem = (
<w:document xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" mc:Ignorable="w14 wp14">
  <w:body>
    {NodeSeq.fromSeq(contents.map(_.asXML))}
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
</w:document>
   )
   def flatMap(f : DocxTextComponent => Seq[DocxTextComponent]) : DocxMainDocument = 
     DocxMainDocument(contents = contents.flatMap(f))
}

sealed trait DocxTextComponent extends Product with XmlComponent {
  def isEmpty : Boolean
}

trait DocxTextComponentContainer[T] {
  def flatMap(f : DocxTextComponent => Seq[DocxTextComponent]) : T
}

final case class P(runs : Seq[ParElement] = Seq(),pPr : Option[PPr] = None) extends DocxTextComponent with ParElementContainer[P] {
  lazy val asXML = (
<w:p>
  {pPr.elem(_.asXML)}
  {runs.eachElem(_.asXML)}
</w:p>
  )
  def insertFirst(els : ParElement*) =
    copy(runs = els ++ runs)
  def insertLast(els : ParElement*) =
    copy(runs = runs ++ els)
  def isEmpty = runs.forall(_.isEmpty)
  
  def flatMap(f : ParElement => Seq[ParElement]) : P =
    copy(runs = runs.flatMap(f))
}

sealed trait ParElement extends Product with XmlComponent {
  def isEmpty : Boolean = false
}

trait ParElementContainer[+T] {
  def flatMap(f : ParElement => Seq[ParElement]) : T
}

abstract sealed class CapsMode extends Product {
  def asXML : NodeSeq
}

case object CM_Caps extends CapsMode {
  def asXML = (<w:caps w:val="true"/>)
}
case object CM_SmallCaps extends CapsMode {
  def asXML = (<w:smallCaps w:val="true"/>)
}
case object CM_Normal extends CapsMode {
  def asXML = NodeSeq.fromSeq(Seq(
      <w:caps w:val="false"/>,
      <w:smallCaps w:val="false"/>
      ))
}

final case class RGB(red : Double, green : Double, blue : Double) {
  lazy val value = f"${r}%02X${g}%02X${b}%02X"
  private val r = norm(red)
  private val g = norm(green)
  private val b = norm(blue)
  private def norm(v : Double) = math.floor(255.0*v).toInt
}

final case class Fonts(
    ascii : Option[String] = None,
    cs : Option[String] = None,
    hAnsi : Option[String] = None) extends XmlComponent {
  lazy val asXML = (
<w:rFonts w:ascii={ascii.getOrElse(null)} w:cs={cs.getOrElse(null)}
       w:hAnsi={ascii.getOrElse(null)}/>
      )
}

abstract sealed class UnderlineOption extends Product with XmlComponent

case object UO_NoUnderline extends UnderlineOption {
  val asXML = <w:u w:val="false"/>
}

abstract sealed class UnderlineStyle(val value : String) extends Product

case object US_Dash extends UnderlineStyle("dash")
case object US_DashDotDotHeavy extends UnderlineStyle("dashDotDotHeavy")
case object US_DashDotHeavy extends UnderlineStyle("dashDotHeavy")
case object US_DashedHeavy extends UnderlineStyle("dashedHeavy")
case object US_DashLong extends UnderlineStyle("dashLong")
case object US_DashLongHeavy extends UnderlineStyle("dashLongHeavy")
case object US_DotDash extends UnderlineStyle("dotDash")
case object US_DotDotDash extends UnderlineStyle("dotDotDash")
case object US_Dotted extends UnderlineStyle("dotted")
case object US_DottedHeavy extends UnderlineStyle("dottedHeavy")
case object US_Double extends UnderlineStyle("double")
case object US_None extends UnderlineStyle("none")
case object US_Single extends UnderlineStyle("single")
case object US_Thick extends UnderlineStyle("thick")
case object US_Wave extends UnderlineStyle("wave")
case object US_WavyDouble extends UnderlineStyle("wavyDouble")
case object US_WavyHeavy extends UnderlineStyle("wavyHeavy")
case object US_Words extends UnderlineStyle("words")

final case class UO_Underline(color : Option[RGB] = None) extends UnderlineOption {
  val asXML = (<w:u w:val="true" w:color={color.map(_.value).getOrElse(null)}/>)
}

abstract sealed class VertAlignment(val value : String) extends Product

case object VA_Superscript extends VertAlignment("superscript")
case object VA_Subscript extends VertAlignment("subscript")
case object VA_Baseline extends VertAlignment("baseline")

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
    ) extends XmlComponent {
  lazy val asXML = (
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
{underline.elem(_.asXML)}
</w:rPr>
      )
  def +(x : RPr) = RPr(
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

}

final case class R(
    rPr : Option[RPr] = None,
    contents : Seq[RunContent] = Seq()) extends ParElement with RunContentContainer[R] {
  lazy val asXML = (
<w:r>
{rPr.onSome(_.asXML)}
{contents.map(_.asXML)}
</w:r>
      )
  def insertFirst(els : RunContent*) = copy(contents = els ++ contents)
  def insertLast(els : RunContent*) = copy(contents = contents ++ els)
  override def isEmpty = contents.forall(_.isEmpty)
  def flatMap(f : RunContent => Seq[RunContent]) : R = 
    copy(contents = contents.flatMap(f))
}

abstract sealed class RunContent extends Product with XmlComponent {
  def isEmpty : Boolean = false
}

trait RunContentContainer[+T] {
  def flatMap(f : RunContent => Seq[RunContent]) : T
}

case object Br extends RunContent {
  val asXML = <w:br/>
}

final case class DelText(text : String, preserveSpace : Boolean = false) extends RunContent {
  lazy val asXML = (
      <w:delText xml:space={if(preserveSpace) { "preserve" } else {  null }}>{text}</w:delText>
      )
}

final case class Del(id : String, content : Seq[ParElement] = Seq(), author : Option[String] = None, date : Option[java.time.ZonedDateTime] = None
    ) extends RunContent with ParElementContainer[Del] {
  lazy val asXML = (
      <w:del w:id={id} w:author={author.getOrElse(null)} date={date.map(_.toString).getOrElse(null)}>
    {NodeSeq.fromSeq(content.map(_.asXML))}
    </w:del>
      )
   def flatMap(f : ParElement => Seq[ParElement]) : Del =
    copy(content = content.flatMap(f))
}

final case class Ins(id : String, content : Seq[ParElement] = Seq(), author : Option[String] = None, date : Option[java.time.ZonedDateTime] = None
    ) extends RunContent with ParElementContainer[Ins] {
  lazy val asXML = (
      <w:ins w:id={id} w:author={author.getOrElse(null)} date={date.map(_.toString).getOrElse(null)}>
    {NodeSeq.fromSeq(content.map(_.asXML))}
    </w:ins>
      )
  def flatMap(f : ParElement => Seq[ParElement]) : Ins =
    copy(content = content.flatMap(f))
}

final case class Hyperlink(
    content : Seq[ParElement] = Seq(), 
    anchor : Option[String] = None, 
    id : Option[String] = None, 
    tooltip : Option[String] = None) extends ParElement 
  with ParElementContainer[Hyperlink] {
  lazy val asXML = (
   <w:hyperlink r:id={id.getOrElse(null)} w:anchor={anchor.getOrElse(null)} w:tooltip={tooltip.getOrElse(null)}>
   {NodeSeq.fromSeq(content.map(_.asXML))}
  </w:hyperlink>
      )
  def flatMap(f : ParElement => Seq[ParElement]) : Hyperlink =
    copy(content = content.flatMap(f))
}

case object NoBreakHyphen extends RunContent {
  val asXML = <w:noBreakHyphen/>
}

case class Sym(font : String, char : String) extends RunContent {
  val asXML = <w:sym w:font={font} w:char={char}/>
}

case class T(text : String, preserveSpace : Boolean = false) extends RunContent {
  lazy val asXML = <w:t xml:space={if(preserveSpace) { "preserve" } else {  null }}>{text}</w:t>
  override val isEmpty = text.trim.size == 0
}

abstract sealed class PTabAlignment(val value : String) extends Product 

case object PTA_Left extends PTabAlignment("left")
case object PTA_Center extends PTabAlignment("center")
case object PTA_Right extends PTabAlignment("right")

abstract sealed class PTabBase(val value : String) extends Product

case object PTB_Indent extends PTabBase("indent")
case object PTB_Margin extends PTabBase("margin")

case class PTab(
    alignment : PTabAlignment,
    leader : Option[TabLeader] = None,
    relativeTo : PTabBase) extends RunContent {
  val asXML = <w:ptab w:alignment={alignment.value}
                   w:leader={leader.map(_.value).optAttr}
                   w:relativeTo={relativeTo.value}/>
}

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
final case class ContentPart(id : String) extends RunContent {
  val asXML = <w:contentPart r:id={id}/>
}

final case class Ind(
      start : Pts20 = Pts20(0), 
      end : Pts20 = Pts20(0),
      hanging : Pts20 = Pts20(0),
      firstLine : Pts20 = Pts20(0)//twentieths of a point
      ) extends XmlComponent {
  lazy val asXML = (
<w:ind w:start={start.value} 
	w:end={end.value} 
	w:hanging={hanging.value}
      w:firstLine={firstLine.value}/>
    )
}

abstract sealed class ST_Jc(val v : String) extends Product

case object JC_Both extends ST_Jc("both")
case object JC_Center extends ST_Jc("center")
case object JC_Distribute extends ST_Jc("distribute")
case object JC_End extends ST_Jc("end")
case object JC_NumTab extends ST_Jc("numTab")
case object JC_Start extends ST_Jc("start")

abstract sealed class TabLeader(val value : String) extends Product

case object TL_Dot extends TabLeader("dot")
case object TL_Heavy extends TabLeader("heavy")
case object TL_Hyphen extends TabLeader("hyphen")
case object TL_MiddleDot extends TabLeader("middleDot")
case object TL_None extends TabLeader("none")
case object TL_Underscore extends TabLeader("underscore")

abstract sealed class TabStopType(val value : String) extends Product

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
case object TST_End extends TabStopType("end")

/*
 * Specifies that the current tab stop shall result in a
location in the document where all following text is
aligned to its leading edge (i.e. all text runs following
this tab stop and preceding the next tab stop shall be
aligned against the leading edge with respect to the
tab stop location).
 */
case object TST_Start extends TabStopType("start")

final case class Tab(
    pos : Pts20 = Pts20(0),
    tabType : TabStopType = TST_Start,
    leader : TabLeader = TL_None
    ) extends Product {
  lazy val asXML = (
<w:tab w:leader={leader.value} w:pos={pos.value} w:val={tabType.value}/>
      )
}

final case object TAB extends RunContent {
  def asXML = <w:tab/>
}

abstract sealed class SpacingLineRule(val value : String) extends Product

case object SLR_AtLeast extends SpacingLineRule("atLeast")
case object SLR_Exactly extends SpacingLineRule("exactly")
case object SLR_Auto extends SpacingLineRule("auto")

abstract sealed class Measure[T <: Measure[T]](ratio : Double) extends Product {
  val v : Double  
  val value = math.round(ratio*v).toInt.toString   
}

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
    ) extends XmlComponent {
  def r(x : Option[Measure[_]]) : String = x.map(_.value).getOrElse(null) 
  def asXML = (
      <w:spacing
        w:after={r(after)}
        w:afterLines={r(afterLines)}
        w:before={r(before)}
        w:beforeLines={r(beforeLines)}
        w:line={r(line.map(_.fold(x => x,x => x)))}
        w:lineRune={lineRule.map(_.value).optAttr}/>
      )
}

final case class PPr(
    divId : Option[String] = None,
    ind : Option[Ind] = None,
    jc : Option[ST_Jc] = None,
    style : Option[String] = None, //styleId
    tabs : Seq[Tab] = Seq(),
    spacing : Option[Spacing] = None,
    widowControl : Boolean = true,
    qFormat : Boolean = true,
    pStyle : Option[String] = None
    ) extends XmlComponent {
  lazy val asXML = (
<w:pPr>
  {divId.elem(n => <w:divId w:val={n}/>)}
  {ind.elem(_.asXML)}
  {jc.elem(x => <w:jc w:val={x.v}/>)}
  {style.elem(x => <w:pStyle w:val={x}/>)}
  {cond(!tabs.isEmpty)(<w:tabs>{tabs.eachElem(_.asXML)}</w:tabs>)}
  {spacing.elem(_.asXML)}
  {if(widowControl) { <w:widowControl/> } else { NodeSeq.Empty }}
  {if(qFormat) { <w:qFormat/> } else { NodeSeq.Empty }}
  {pStyle.elem(n => <w:pStyle w:val={n}/>)}
</w:pPr>
  )
}

