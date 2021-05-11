package br.gov.lexml.renderer.docx.docxmodel

import br.gov.lexml.renderer.docx.docxmodel._
import org.apache.commons.lang.StringEscapeUtils
import org.bitbucket.inkytonik.kiama._
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.bitbucket.inkytonik.kiama.attribution._
import org.bitbucket.inkytonik.kiama.attribution.UncachedAttribution._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayStack
import scala.util.matching.Regex

package object proc {



val pecTexts : ParElementContainer[_] => Seq[T] =  attr { _.parElements.flatMap(peTexts) }  

val rccTexts : RunContentContainer[_] => Seq[T] = attr { _.contents.flatMap(rcTexts) }  

val rcTexts : RunContent => Seq[T] = attr {
  case t : T => Seq(t)
  
  case pec : ParElementContainer[_] => pecTexts(pec)
    
  case rcc : RunContentContainer[_] => rccTexts(rcc)

  case x => Seq()  
}

val peTexts : ParElement => Seq[T] = attr {
  case t : T => Seq(t)
  
  case pec : ParElementContainer[_] => pecTexts(pec)
  
  case rcc : RunContentContainer[_] => rccTexts(rcc)

  case x => Seq()  
}

private val trimRe = """(^\s+)|(\s+$)|((?<=\S\s)\s+(?=\S))|(\s+(?=[,;.!?]))|((?<=\()\s+|\s+(?=\)))|((?<=“)\s+)|(\s+(?=”))""".r

  

protected[proc] class WrapEq[A](x : A) {
  private val _hashCode = System.identityHashCode(x)
  override def hashCode() = _hashCode

  override def equals(obj: Any): Boolean = obj match {
    case o: WrapEq[_] => o._hashCode == _hashCode
  }
  override def toString() = s"#${_hashCode}{${x.toString}}"
}

def wrapEq[T](x : T) : WrapEq[T] = new WrapEq(x)

def escape(raw: String): String = {
  import scala.reflect.runtime.universe._
  Literal(Constant(raw)).toString
}

private def S(x : String) = {
  if(x == null) { "(null)" } else {
    "\"" + StringEscapeUtils.escapeJava(x) + "\"" + s" [${x.length}]"
  }
}

def trimP(p : P) : P = {
  val texts = pecTexts(p)
  val sb = new StringBuilder()
  import scala.collection.immutable.SortedMap
  val (_,rangeMap) = texts.foldLeft((0,SortedMap[Int,T]())) {
    case ((pos,m),t) =>
      val npos = pos + t.text.length
      val nm = m + (pos -> t)
      sb.append(t.text)
      (npos,nm)
  }
  val text = sb.toString
  import scala.collection.mutable.Stack
  val matches: Stack[(Int,Int)] =
    trimRe.findAllMatchIn(text).to(Vector).
      map(m => (m.start,m.end - m.start))
  val cb = Vector.newBuilder[(WrapEq[T],Int,Int)]
  cb.sizeHint(matches.length)
  while(!matches.isEmpty) {
    val (start,length) = matches.pop()
    val (p0,t) = rangeMap.rangeTo(start).last
    val p1 = start - p0
    val l1 = math.min(length,t.text.length-p1)
    val l2 = length - l1
    if(l2 > 0) {
      matches.push((l1,l2))
    }
    cb += ((wrapEq(t),p1,l1))
  }
  val cutsL = cb.result()
  val cuts = cutsL.groupBy(_._1).view.mapValues(
    _.map(x => (x._2,x._3)))
    .toMap

  val rewriteRule = rule[Any] {
    case t : T if cuts.contains(new WrapEq(t)) =>
      t.copy(text = cutString(t.text,cuts(new WrapEq(t)) : _*))
    case x => x    
  }

  val strategy = bottomup(rewriteRule)
  rewrite(strategy)(p)  
}

def cutString(txt : String, cuts : (Int,Int)*) : String = {
  val txt1=txt.replaceAll("\\s"," ")
  cuts.to(IndexedSeq).sortBy(_._1).reverse.foldLeft(txt1) { case (t,(pos,len)) =>
    t.substring(0,pos) + t.substring(pos + len)
  }
}

}
