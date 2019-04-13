package br.gov.lexml.renderer.docx.docxmodel

import scala.xml._

import scala.reflect.runtime.universe._

import scala.reflect.ClassTag

import br.gov.lexml.renderer.docx.docxmodel.builders.implicits.RichOption1


final case class StyleOptField[T](
    val ft : StyleFieldType[T],val value : T)
     extends XmlComponent {
  def asXML = ft.toXML(value)  
}

sealed abstract class StyleFieldType[T](val name : String)
  (implicit ct : ClassTag[T]){
  def toXML(value : T) : Elem
}

sealed abstract class StyleValFieldType[T](name : String)(implicit ct : ClassTag[T]) 
  extends StyleFieldType[T](name)(ct) {
  override def toXML(value : T) : Elem =
    (<w:elem w:val={value.toString} />
			).copy(label = name)
}

sealed abstract class StyleStringFieldType(name : String) 
  extends StyleValFieldType[String](name) 

case object S_Name extends StyleStringFieldType("name")
case object S_BasedOn extends StyleStringFieldType("basedOn")
case object S_Link extends StyleStringFieldType("link")
case object S_Next extends StyleStringFieldType("next")

abstract sealed class StyleIntFieldType(name : String)
  extends StyleValFieldType[Int](name)
  
case object S_UIPriority extends StyleIntFieldType("uiPriority")  

sealed abstract class StyleFlagType(name : String) 
  extends StyleFieldType[Unit](name) {  
  def toXML(value : Unit) = (<w:x/>).copy(label = name)
}

case object S_AutoRedefine extends StyleFlagType("autoRedefine")
case object S_Hidden extends StyleFlagType("hidden")
case object S_QFormat extends StyleFlagType("qFormat")
case object S_SemiHidden extends StyleFlagType("semiHiddem")
case object S_UnhideWhenUsed extends StyleFlagType("unhideWhenUsed")
case object S_Locked extends StyleFlagType("unhideWhenUsed")



sealed abstract class StyleType(val value : String) extends Product

case object ST_Paragraph extends StyleType("paragraph")
case object ST_Character extends StyleType("character")
case object ST_Table extends StyleType("table")
case object ST_Numbering extends StyleType("numbering")

final case class Style(
    `type` : StyleType,
    id : String,
    aliases : Set[String] = Set(),
    fields : Map[StyleFieldType[_],StyleOptField[T] forSome { type T }] = Map(),    
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
      
  
  def name = field(S_Name)
  
  val setName = setField(S_Name)
  
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
  
  def asXML = (      
      <w:style w:styleId={id} w:type={`type`.value}
      	w:default={default.onTrueOrNull("true")}
      	w:customStyle={customStyle.onTrueOrNull("true")}>
			{
			  if(aliases.isEmpty) { NodeSeq.Empty } else {			
			    <w:aliases w:val={aliases.mkString(",")}/>
			  }
			}			
			{ fields.values.to[Seq].map(_.asXML) }
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
			{pPr.onSome(x => <w:pPrDefault>{x.asXML}</w:pPrDefault>)}
			{rPr.onSome(x => <w:rPrDefault>{x.asXML}</w:rPrDefault>)}
			</w:docDefaults>
      )
}

final case class Styles(
    docDefaults : Seq[DocDefaults] = Seq(),
    latentStyles : Option[LatentStyles] = None,
    styles : Seq[Style] = Seq()) extends XmlComponent {
  def asXML = (
      <w:styles xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" mc:Ignorable="w14">
			{latentStyles.onSome(_.asXML)}
			{styles.map(_.asXML)}
			</w:styles>
      )
}
