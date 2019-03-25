package br.gov.lexml.schema.model

import scala.xml._


object SchemaXmlValidation {
  
  def validate(schema : Elem) = {
    val rootElms = schema.child.collect { case e : Elem if e.label != "import" => e }
    val elements = rootElms.collect {
      case e : Elem if e.label == "element" => xsd_element(e)     
    }
  }
  
  def xsd_element(e : Elem) {
    val subels = e.child.collect({case x : Elem => x })
    val name = (e \ "@name").text
    if(subels.length > 2 || subels.headOption.map(_.label).getOrElse("complexType") != "complexType") {
       println("No complexType under (" + name + ") " + e)
    } else {
      subels.foreach(complex_type(name))
    }
  }
  
  def complex_type(name : String)(e : Elem) {
    val subels = e.child.collect({case x : Elem => x })
    val seqs = subels.takeWhile(x => x.label == "sequence" || x.label == "group")
    val seqs_rest = subels.drop(seqs.size)
    val seqs_rest_labels = seqs_rest.map(_.label).to[Set]
    if (!seqs_rest_labels.subsetOf(Set("attribute","attributeGroup"))) {
       println("Unexpected element under root element (" + name + "): " + seqs_rest_labels + " on: " + e)
    } else {
  
      if(seqs.length > 1) { 
        println("More than one seq or group in (" + name + "): " + e) 
      } 
      seqs.headOption.collect({
         case x : Elem if e.label == "sequence" => seq_element(name)(x)
         case x => seq_group(name)(x)
      })
      seqs_rest.foreach(elem_attr_element(name))
   }
  }
          
  def seq_element(name : String)(e : Elem) {
    val subels = e.child.collect({case x : Elem => x }).to[Seq]
    if(!subels.forall(x => x.label == "element" || x.label == "group")) {
      println("Unexpected element inside sequence (" + name + "): " + e)
    } else {
      subels.foreach {
        case x : Elem if x.label == "element" && ((x \ "@ref").text).length == 0 => println("No ref in element(" + name + "): " + x + ", parent: " + e)
        case x : Elem if x.label == "group" && ((x \ "@ref").text).length == 0 => println("No ref in group(" + name + "): " + x + ", parent: " + e)
        case _ =>
      }
    } 
  }
  
  def elem_attr_element(name : String)(e : Elem) { }
          
  def seq_group(name : String)(e : Elem) { }
 
}