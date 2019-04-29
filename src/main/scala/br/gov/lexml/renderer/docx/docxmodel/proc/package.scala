package br.gov.lexml.renderer.docx.docxmodel

import br.gov.lexml.renderer.docx.docxmodel._
import org.bitbucket.inkytonik.kiama._
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.bitbucket.inkytonik.kiama.attribution._
import org.bitbucket.inkytonik.kiama.attribution.UncachedAttribution._

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayStack
import scala.util.matching.Regex

package object proc {



val pecTexts : ParElementContainer[_] => Seq[T] =  attr { p : ParElementContainer[_] =>
  val res = p.parElements.flatMap(peTexts)
  println("pecTexts: p = " + p + ", res: " + res)
  res
}

val rccTexts : RunContentContainer[_] => Seq[T] = attr { c : RunContentContainer[_] =>
  val res = c.contents.flatMap(rcTexts)
  println("rccTexts:  c = " + c + ", res = " + res)
  res
}

val rcTexts : RunContent => Seq[T] = attr {
  case t : T => {
    println("Found a T: " + t)
    Seq(t)
  }
  case pec : ParElementContainer[_] => {
    println("Found a container: " + pec.parElements)
    val res = pecTexts(pec)
    println("   res = " + res)
    res
  }
  case rcc : RunContentContainer[_] => {
    println("Found a run container: " + rcc.contents)
    val res = rccTexts(rcc)
    res
  }
  case x => {
    println("Found something else: " + x)
    Seq()
  }
}

val peTexts : ParElement => Seq[T] = attr {
  case t : T => {
    println("Found a T: " + t)
    Seq(t)
  }
  case pec : ParElementContainer[_] => {
    println("Found a container: " + pec.parElements)
    val res = pecTexts(pec)
    println("   res = " + res)
    res
  }
  case rcc : RunContentContainer[_] => {
    println("Found a run container: " + rcc.contents)
    val res = rccTexts(rcc)
    res
  }
  case x => {
    println("Found something else: " + x)
    Seq()
  }
}

private val trimRe = """(^\s+)|(\s+$)|((?<=\S\s)\s+(?=\S))""".r

protected[proc] class WrapEq[A](x : A) {
  private val _hashCode = System.identityHashCode(x)
  override def hashCode() = _hashCode

  override def equals(obj: Any): Boolean = obj match {
    case o: WrapEq[_] => o._hashCode == _hashCode
  }
}

def escape(raw: String): String = {
  import scala.reflect.runtime.universe._
  Literal(Constant(raw)).toString
}

def trimP(p : P) : P = {
  println("Trimming: " + p)
  val texts = pecTexts(p)
  val sb = new StringBuilder()
  import scala.collection.SortedMap
  val (_,rangeMap) = texts.foldLeft((0,SortedMap[Int,T]())) {
    case ((pos,m),t) =>
      val npos = pos + t.text.length
      val nm = m + (pos -> t)
      sb.append(t.text)
      (npos,nm)
  }
  val text = sb.toString
  println("text: " + escape(text))
  val matches: ArrayStack[(Int,Int)] =
    trimRe.findAllMatchIn(text).to[ArrayStack].
      map(m => (m.start,m.end - m.start))
  val cb = Seq.newBuilder[(T,Int,Int)]
  while(!matches.isEmpty) {
    val (start,length) = matches.pop()
    val (p0,t) = rangeMap.to(start).last
    val p1 = start - p0
    val l1 = math.min(length,t.text.length-p1)
    val l2 = length - l1
    if(l2 > 0) {
      matches.push((l1,l2))
    }
    cb += ((t,p1,l1))
  }
  val cuts = cb.result().groupBy(_._1).mapValues(_.map(x => (x._2,x._3)))
    .map { case (k,v) => (new WrapEq(k),v)}
  println("cuts: " + cuts)
  val rewriteRule = rule[Any] {
    case t : T if cuts.contains(new WrapEq(t)) =>
      println("Cutting: " + escape(t.text))
      val r = T(text = cutString(t.text,cuts(new WrapEq(t)) : _*))
      println("Result: " + escape(r.text))
      r
    case x => {
      println("rewriteRule: ignoring: " + x)
      x
    }
  }

  val strategy = bottomup(rewriteRule)
  val res = rewrite(strategy)(p)
  println("   Result: " + res)
  res
}

def cutString(txt : String, cuts : (Int,Int)*) : String = {
  val txt1=txt.replaceAll("\\s"," ")
  cuts.to[IndexedSeq].sortBy(_._1).reverse.foldLeft(txt1) { case (t,(pos,len)) =>
    t.substring(0,pos) + t.substring(pos + len)
  }
}

}
