import java.io.File
import scala.xml.{ Attribute => XmlAttribute, _}
import br.gov.lexml.schema.model._

sealed trait OccurPos extends Product {
  def +(p : OccurPos) : OP_Indirect = OP_Indirect(p, Seq(this))
  val sortKey : String
}

final case class OP_Complex_Root_Element(name : String) extends OccurPos {
  val sortKey = "CRE:" + name
}

final case class OP_AttributeGroup(name : String) extends OccurPos {
  val sortKey = "AG:" + name
}

final case class OP_Group(name : String) extends OccurPos {
  val sortKey = "G:" + name
}

final case class OP_Indirect(in : OccurPos, through : Seq[OccurPos]) extends OccurPos {
  override def +(p : OccurPos) = OP_Indirect(p, in +: through)
  val sortKey = in.sortKey + "_" + through.map(_.sortKey).mkString("","_","")
}

sealed trait Occurrence extends Product {
  val sortKey : String
}

final case class O_ElementRef(name : String) extends Occurrence {
  val sortKey = "E: " + name
}

final case class O_GroupRef(name : String) extends Occurrence {
  val sortKey = "G:" + name
}

final case class O_AttributeRef(name : String) extends Occurrence {
  if(name == "") { throw new RuntimeException("empty attribute name") }
  val sortKey = "A:" + name
}

final case class O_AttributeGroupRef(name : String) extends Occurrence {
  val sortKey = "AG:" + name
}
    
object TestSchema {
    
  def main(args : Array[String]) {
    val schemaXml = XML.loadFile(new File("/home/joao/git/senado/lexml-xml-schemas/src/main/resources/xsd/lexml-flexivel-1.1.xsd"))
    
    SchemaXmlValidation.validate(schemaXml)
    
    
    val schema = Schema(schemaXml)
    
    schema.validate()
    
    val om1  : Seq[(Occurrence,OccurPos)] = schema.rootElements.values.to[Seq].collect {
        case RootElementComplex(name, elemFields, attrs, _) => 
          val l1 : Seq[Occurrence] = elemFields collect { 
            case Ref(refName, RT_Element, _) => O_ElementRef(refName)
            case Ref(refName, RT_Group, _)  => O_GroupRef(refName)            
          }
          val l2 : Seq[Occurrence] = attrs collect {
            case Attribute(name, _,_) => O_AttributeRef(name) 
            case AttrGroupRef(name) => O_AttributeGroupRef(name)
            case AR_Named(name) => O_AttributeRef(name)
          }
          (l1 ++ l2).map { x => (x,OP_Complex_Root_Element(name)) }         
    }.flatten
    
    val om2  : Seq[(Occurrence,OccurPos)] = schema.groups.values.to[Seq].flatMap { g =>
      val l1 : Seq[Occurrence] = g.elems collect {        
        case Ref(refName, RT_Element, _) => O_ElementRef(refName)
        case Ref(refName, RT_Group, _)  => O_GroupRef(refName)            
      }
      l1.map { x => (x,OP_Group(g.name)) }
    }
    
    val om3 :  Seq[(Occurrence,OccurPos)] = schema.attributeGroups.values.to[Seq].flatMap { g =>
      val l1 : Seq[Occurrence] = g.fields collect {        
        case Attribute(name, _,_) => O_AttributeRef(name) 
        case AttrGroupRef(name) => O_AttributeGroupRef(name)
        case AR_Named(name) => O_AttributeRef(name)            
      }
      l1.map { x => (x,OP_AttributeGroup(g.name)) }
    } 
    
    val ol = om1 ++ om2 ++ om3
    
    def mapSet[K,V](ol : Seq[(K,V)]) = {
      val m0 : Map[K,Set[V]] = Map.empty.withDefaultValue(Set[V]())    
      ol.foldLeft(m0) { case (m,(oc,op)) =>  m + (oc -> (m.getOrElse(oc,Set()) + op)) }
    }
    
    
    
    val m1 = mapSet(ol)
    
    
    val m2 = mapSet(ol.map{case (x,y) => (y,x)})
        
    def explode : (Occurrence,Set[OccurPos]) => Seq[(Occurrence,OccurPos)] = {
      case (O_GroupRef(refName),ops) => 
        for { 
          op <- ops.to[Seq] ;
          oc <-  m2(OP_Group(refName))  
        } yield (oc,op)
      case (O_AttributeGroupRef(refName),ops) => 
        for { 
          op <- ops.to[Seq] ;
          oc <-  m2(OP_AttributeGroup(refName))  
        } yield (oc,op)
      case (O_ElementRef(refName),ops) => 
        for { 
          op <- ops.to[Seq] ;
          oc <-  m2(OP_Complex_Root_Element(refName)).to[Seq]   
        } yield (oc,op)
      case (x,y) => Seq()
    }
    
    def explode1 : ((Boolean,Seq[(Occurrence,OccurPos)]),(Occurrence,Set[OccurPos])) => (Boolean,Seq[(Occurrence,OccurPos)]) = {
      case ((exploded,sofar),r) =>
        val d = explode.tupled(r)
        val d1 = if (d.isEmpty) { r._2.map { y => (r._1,y) } } else { d }
        (exploded || !d.isEmpty, sofar ++ d1)
    }
    
    def explode2(l : Map[Occurrence,Set[OccurPos]]) : Map[Occurrence,Set[OccurPos]] = {
      val s0 = (false,Seq[(Occurrence,OccurPos)]())
      val (exploded,l1) = l.to[Seq].foldLeft(s0)(explode1(_,_))
      if(exploded) { mapSet(l1) } else { l }
    }
    
    val m1_1 = explode2(m1)
    
    val ll1 = m1_1.to[Seq].sortBy{case x : ((Occurrence,Set[OccurPos])) => (1000 - x._2.size,x._1.sortKey)}
    ll1.foreach { case (k,v) =>      
      val v1 = v.to[Seq].collect {
        case x : OP_Complex_Root_Element => x.name
      }
      if(v1.size > 1) {
        println(s"${k}[${v1.size}]: ${v1.sorted.mkString("",", ","")}")
      }
    }
  }
}