package br.gov.lexml.renderer.docx.docxmodel.builders

import cats.{implicits => catImplicits, _}
import cats.data._
import catImplicits._
import br.gov.lexml.renderer.docx.docxmodel._

  
final case class RunBuilderState[Q](
    contents : Seq[RunContent] = Seq(),
    value : Q) extends Modifiable[Q,RunBuilderState[Q]] {
  def setValue(x : Q) = copy(value = x)
}

trait RunBuilderOps[T] {  
  import implicits._    
  
  final type RST = RunBuilderState[T]
  
  final type RB[A] = RunBuilderMonad[T,A]
  
  final def subRun[A](rPr : Option[RPr] = None)(rb : RB[A])(implicit mo : Mergeable[T]) : RB[(R,A)] = State {
    st =>
      val (st1,v) = rb.run(st.copy(contents = Seq())).value
      val runElems = st1.contents
      val st2 = st1.copy(contents = Seq())
      (st.modify(x => mo.merge(x,st2.value)),(R(rPr = rPr,contents = runElems),v))        
  }
  
  final def putRC(rc : RunContent*) : RB[Unit] = {
    State.modify((x : RST) => x.copy(contents = x.contents ++ rc))
  }
  
  final def br = putRC(Br)
  
  final def delText(text : String, preserveSpace : Boolean = false) =
    putRC(DelText(text, preserveSpace))
  
  final def enclosingRun2(rb : RB[Unit],rPr : Option[RPr] = None)(f : R => RunContent)(implicit mo : Mergeable[T]) =
    subRun(rPr)(rb).flatMap { case (run,_) => putRC(f(run)) }
  
  final def enclosingRun[T1](
      pb : ParBuilderMonad[T1,Unit])(f : Seq[ParElement] => RunContent)(implicit mo : Mergeable2[T,T1]) = 
    State { st0 : RST =>
      val pst0 = ParBuilderState[T1](value = mo.extract(st0.value))
      val (pst1,_) = pb.run(pst0).value
      val rc = f(pst1.contents)
      val st1 = st0.copy(contents = st0.contents :+ rc, value = mo.merge(st0.value,pst1.value))
      (st1,())    
  }
  
  final def del[T1](id : String, author : Option[String] = None, 
      date : Option[java.time.ZonedDateTime] = None)
      (pb : ParBuilderMonadStmt[T1])(implicit mo : Mergeable2[T,T1]) : RB[Unit] =
      enclosingRun(pb)(Del(id,_,author,date))
      
      
  final def ins[T1](id : String, author : Option[String] = None, 
      date : Option[java.time.ZonedDateTime] = None)
      (pb : ParBuilderMonadStmt[T1])(implicit mo : Mergeable2[T,T1]) : RB[Unit] =
      enclosingRun(pb)(Ins(id,_,author,date))


  final def noBreakHyphen = putRC(NoBreakHyphen)
  
  final def sym(font : String, char : String) = putRC(Sym(font, char))
  
  final def text(text : String, preserveSpace : Boolean = true) = {
    putRC(T(text, preserveSpace))
  }
  
  final def tab = putRC(TAB)
  
  final def ptab(alignment : PTabAlignment,leader : Option[TabLeader] = None,
            relativeTo : PTabBase) =
     putRC(PTab(alignment,leader,relativeTo))
  
  final def contentPart(id : String) = putRC(ContentPart(id))   
  
  final def footnoteReference(id : String) = putRC(FootnoteReference(id))
  
  final def endnoteReference(id : String) = putRC(EndnoteReference(id))
  
  final def getRState : RB[T] = State.inspect(_.value)
  
  final def setRState(v : T) : RB[Unit] = State.modify(_.copy(value = v))
  
  final def mergeRState(v : T)(implicit mo : Mergeable[T]) : RB[Unit] = 
    State.modify(x => x.copy(value = mo.merge(x.value,v)))
  
  final def modifyRState(f : T => T) : RB[Unit] = 
    State.modify(x => x.copy(value = f(x.value)))
  
  final def modifyRStateV[A](f : T => (T,A)) : RB[A] = State { st =>
    val (v1,res) = f(st.value)
    (st.copy(value = v1),res)
  }    
    
  final def inspectRState[A](f : T => A) : RB[A] =
      State.inspect(x => f(x.value))  
  
}
  
 
