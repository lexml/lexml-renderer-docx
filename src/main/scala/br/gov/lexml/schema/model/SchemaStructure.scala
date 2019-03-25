package br.gov.lexml.schema.model

import scala.xml._

final case class Bounds(min : Int, max : Option[Int])

object Bounds {  
  def apply(e : Elem) : Bounds = {
    
    Bounds(
        (e \ "@minOccurs").headOption.map(_.text.toInt).getOrElse(1),
        (e \ "@maxOccurs").headOption.map(_.text).filterNot(_ == "unbounded").map(_.toInt)
        )
  }
}

object ElemSeq {
  def apply(e : Elem) : Seq[Ref] = {
    val els = e.child.to[Seq].collect {
      case e : Elem if e.label == "element" || e.label == "group" => e
    }
    els.map{Ref(_)}
  }
}

final case class Ref(refName : String, refType : RefType, bounds : Bounds)

object Ref  {
  val specialElements : Map[String,RefType] =
    Map(
        "mathml:math" -> RT_Math
        )
  def apply(e : Elem) : Ref = {
    val refName = (e \ "@ref").text
    val refType = e.label match {      
      case "element" =>        
        specialElements.getOrElse(refName,RT_Element)        
      case "group" => RT_Group
      case "any" => RT_Any((e \ "@namespace").headOption.map(_.text))
      case _ => throw new RuntimeException("unexpected: " + e)
    }
    val bounds = Bounds(e)
    Ref(refName,refType,bounds)
  }
}

abstract sealed trait RefType extends Product 

case object RT_Element extends RefType

case object RT_Group extends RefType

final case class RT_Any(ns : Option[String]) extends RefType

case object RT_Math extends RefType


sealed abstract trait AttrOrGroup extends Product 

object AttrOrGroup {
  def select : PartialFunction[Node,AttrOrGroup] = {
    case e : Elem if e.label == "attribute" && ((e \ "@name").text != "") => Attribute(e)
    case e : Elem if e.label == "attribute" => AttrRef(e)
    case e : Elem if e.label == "attributeGroup" => AttrGroupRef(e)
  }  
  def apply(e : Elem) : AttrOrGroup = select(e)
}

final case class Attribute(name : String, `type`: String, required : Boolean = false) extends AttrOrGroup {
  if (name == "") { throw new RuntimeException("empty attribute name") }
}

object Attribute {
  def apply(e : Elem) : Attribute =
    Attribute((e \ "@name").text,(e \ "@type").text, (e \ "@required").text == "true")
}

final case class AttrGroupRef(refName : String) extends AttrOrGroup

object AttrGroupRef {
  def apply(e : Elem) : AttrGroupRef = AttrGroupRef((e \ "@ref").text)
}

abstract sealed class AttrRef extends AttrOrGroup

object AttrRef {
  def apply(e : Elem) : AttrRef = {
    val refName = (e \ "@ref").text
    refName match {
      case "xml:lang" => AR_Lang
      case _ => AR_Named(refName)
    }    
  }
}

case object AR_Lang extends AttrRef

final case class AR_Named(refName : String) extends AttrRef



sealed abstract class RootElement {
  def name : String
}

object RootElement {
  def select1 : PartialFunction[Node,Elem] = {
    case e : Elem if e.label == "element" => e      
  }  
  
  def select  : PartialFunction[Node,RootElement] = 
    select1 andThen (RootElementComplex.select orElse RootElementSimple.select)    
  def apply( e : Elem) : RootElement = select(e)
}

final case class RootElementComplex(
    name : String,
    elemFields: Seq[Ref],
    attrs : Seq[AttrOrGroup],
    mixed : Boolean) extends RootElement

object RootElementComplex {
  def select : PartialFunction[Node,RootElementComplex] = {
    case e: Elem if e.label == "element" &&
          (e.child.collect({case x : Elem => x}).headOption.map(_.label) == Some("complexType")) => RootElementComplex(e)     
  }
  def apply(e : Elem) : RootElementComplex = {
    val ct = (e \ "complexType").head.asInstanceOf[Elem]
    val ct_childs = ct.child.collect{case e : Elem => e}.to[Seq]
    val seqsOrGroup = 
      ct_childs.filter(_.label == "sequence").flatMap(_.child.collect { case x : Elem => Ref(x) }) ++
        ct_childs.filter(_.label == "group").map{Ref(_)}
    if ((ct \ "attribute" \ "@name").to[Seq].map(_.text).exists(_ == "")) {
      throw new RuntimeException("empty attribute name at " + e)
    }
    println("at " + e)
    val attrs = 
        ct_childs.filter(x => x.label == "attribute" && x.attributes.get("ref").isEmpty).map(Attribute(_)) ++
        ct_childs.filter(x => x.label == "attribute" && x.attributes.get("name").isEmpty).map(AttrRef(_)) ++        
        ct_childs.filter(_.label == "attributeGroup").map(AttrGroupRef(_))
    RootElementComplex((e \ "@name").text,seqsOrGroup,attrs,
        (ct \ "@mixed").text == "true")
  }
}

abstract sealed class NamedType(val name : String) extends Product

object NamedType {
  val named_types = Map(
      ST_String.name -> ST_String,
      ST_Date.name -> ST_Date,
      NT_AnyWithSource.name -> NT_AnyWithSource
      )  
  def apply(x : String) : NamedType = named_types(x)
}

abstract sealed class SimpleType(name : String) extends NamedType(name)

case object ST_String extends SimpleType("xsd:string")

case object ST_Date extends SimpleType("xsd:date")

case object NT_AnyWithSource extends NamedType("anyWithSource")

final case class RootElementSimple(
    name : String,
    `type` : NamedType) extends RootElement

object RootElementSimple {
  def select : PartialFunction[Node,RootElementSimple] = {
    case e: Elem if e.label == "element" &&
          (e.child.collect({case x : Elem => x}).isEmpty) => RootElementSimple(e)
  }
  def apply(e : Elem) : RootElementSimple = {
    RootElementSimple((e \ "@name").text,NamedType((e \ "@type").text))
  }
}

final case class AttributeGroup(name : String, fields : Seq[AttrOrGroup])

object AttributeGroup {
  def select : PartialFunction[Node,AttributeGroup] = {
    case e : Elem if e.label == "attributeGroup" =>
      AttributeGroup((e \ "@name").text, (e.child.collect({case x : Elem => x}).map{AttrOrGroup(_)}))
  }
  def apply(e : Elem) : AttributeGroup = select(e)
}

final case class Group(name : String,elems : Seq[Ref])

object Group {
  def select : PartialFunction[Node,Group] = {
    case e : Elem if e.label == "group" =>
      val choice = e.child.collect { case x : Elem if x.label == "choice" => x}.head
      val els = choice.child.to[Seq].collect { case x : Elem  => Ref(x)}
      Group((e \ "@name").text, els)
  }
  def apply(e : Elem) : Group = select(e)
}

final case class Schema(
    rootElements : Map[String,RootElement],
    attributeGroups : Map[String,AttributeGroup],
    groups : Map[String,Group]) {
  def error[T](msg : String) : T = { throw new RuntimeException("Validation: " + msg) }
  def validate() {
    rootElements.values.collect { 
      case RootElementComplex(name,elemFields,attrs,mixed) =>
        elemFields.foreach { r => validateRef(name,r) }        
        attrs.foreach { a => validateAttrOrGroup(name,a) }
        if(mixed && elemFields.size != 1) { 
          error("{" + name + "} is mixed its subelement count is not 1")
        }
      case _ =>
    }
    attributeGroups.values.foreach {   
      case AttributeGroup(name,fields) =>
        fields.foreach { a => validateAttrOrGroup(name,a) }
    }
    groups.values.foreach {      
      case Group(name,elems) =>
        elems.foreach { r => validateRef(name,r) }
    }
  }
  
  
  
  def validateRef(elemName : String, ref : Ref) {
    ref.refType match {
      case RT_Element => rootElements.contains(ref.refName) || error(s"invalid ref to element '${ref.refName}' in ${elemName}")
      case RT_Group => groups.contains(ref.refName) || error(s"invalid ref to group '${ref.refName}' in ${elemName}")
      case _ =>
    }
  }
  
  def validateAttrOrGroup(elemName : String, a : AttrOrGroup) {
    a match {
      case x : AttrGroupRef => attributeGroups.contains(x.refName) || error(s"invalid ref to attribute group '${x.refName}' in ${elemName}")
      case AR_Named(name) => println(s"Warning: reference to attribute = ${name} in ${elemName}")
      case _ => ()
    }
  }
    
}

object Schema {
  def apply(e : Elem) : Schema = {
    val rootElements = 
      e.child.collect(RootElement.select).map{x => (x.name,x)}.toMap
    val attributeGroups = 
      e.child.collect(AttributeGroup.select).map{x => (x.name,x)}.toMap
    val groups = 
      e.child.collect(Group.select).map{x => (x.name,x)}.toMap
    Schema(rootElements,attributeGroups,groups)
  }
}