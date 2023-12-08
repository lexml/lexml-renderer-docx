package br.gov.lexml.renderer.docx.docxmodel

import br.gov.lexml.renderer.docx.docxmodel.*
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.*
import org.bitbucket.inkytonik.kiama.attribution.UncachedAttribution.*

package object proc:

  lazy val pecTexts : ParElementContainer[_] => Seq[T] =  attr { _.parElements.flatMap(peTexts) }

  lazy val rccTexts : RunContentContainer[_] => Seq[T] = attr { _.contents.flatMap(rcTexts) }

  lazy val rcTexts : RunContent => Seq[T] = attr[RunContent,Seq[T]] {
    case t : T => Seq(t)
    case pec : ParElementContainer[_] => pecTexts(pec)
    case _ => Seq()
  }

  lazy val peTexts : ParElement => Seq[T] = attr[ParElement,Seq[T]]:
    case pec : ParElementContainer[_] => pecTexts(pec)
    case rcc : RunContentContainer[_] => rccTexts(rcc)
    case null => Seq()

  private val trimRe = """(^\s+)|(\s+$)|((?<=\S\s)\s+(?=\S))|(\s+(?=[,;.!?]))|((?<=\()\s+|\s+(?=\)))|((?<=“)\s+)|(\s+(?=”))""".r

  protected[proc] class WrapEq[A](val x : A):
    private val _hashCode = System.identityHashCode(x)
    override def hashCode: Int = _hashCode

    override def equals(obj: Any): Boolean = obj match {
      case o: WrapEq[_] => o._hashCode == _hashCode
    }
    override def toString: String = s"#${_hashCode}{${x.toString}}"

  def wrapEq[T](x : T) : WrapEq[T] = new WrapEq(x)

  def trimP(p : P) : P =
    val texts = pecTexts(p)
    val sb = StringBuilder()
    import scala.collection.immutable.SortedMap
    val (_, rangeMap) = texts.foldLeft((0, SortedMap[Int, T]())) {
      case ((pos, m), t) =>
        val npos = pos + t.text.length
        val nm = m + (pos -> t)
        sb.append(t.text)
        (npos, nm)
    }
    val text = sb.toString
    import scala.collection.mutable
    val matches: mutable.Stack[(Int, Int)] =
      trimRe.findAllMatchIn(text).
        map(m => (m.start, m.end - m.start)).to(mutable.Stack)
    val cb = Vector.newBuilder[(WrapEq[T], Int, Int)]
    cb.sizeHint(matches.length)
    while matches.nonEmpty do
      val (start, length) = matches.pop()
      val (p0, t) = rangeMap.rangeTo(start).last
      val p1 = start - p0
      val l1 = math.min(length, t.text.length - p1)
      val l2 = length - l1
      if l2 > 0 then
        matches.push((l2, l1))
      cb += ((wrapEq(t), p1, l1))
    val cutsL = cb.result()
    val cuts = cutsL.groupBy(_._1).view.mapValues(
        _.map(x => (x._2, x._3)))
      .toMap

    val rewriteRule = rule[Any] {
      case t: T if cuts.contains(WrapEq(t)) =>
        t.copy(text = cutString(t.text, cuts(WrapEq(t))*))
      case x => x
    }

    val strategy = bottomup(rewriteRule)
    rewrite(strategy)(p)
  end trimP

  def cutString(txt : String, cuts : (Int,Int)*) : String =
    val txt1=txt.replaceAll("\\s"," ")
    cuts.to(IndexedSeq).sortBy(_._1).reverse.foldLeft(txt1) { case (t,(pos,len)) =>
      t.substring(0,pos) + t.substring(pos + len)
    }
end proc