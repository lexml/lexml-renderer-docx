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
  
  final def putRC(rc : RunContent*) = 
    State.modify((x : RST) => x.copy(contents = x.contents ++ rc)) 
  
  final def br = putRC(Br)
  
  final def delText(text : String, preserveSpace : Boolean = false) =
    putRC(DelText(text, preserveSpace))
  
  final def enclosingRun(rb : RB[Unit],rPr : Option[RPr] = None)(f : R => RunContent)(implicit mo : Mergeable[T]) =
    subRun(rPr)(rb).flatMap { case (run,_) => putRC(f(run)) }
    
  final def del(id : String, author : Option[String] = None, 
      date : Option[java.time.ZonedDateTime] = None,
      rPr : Option[RPr] = None)(rb : RB[Unit])(implicit mo : Mergeable[T]) =
      enclosingRun(rb,rPr)(Del(id,_,author,date))
      
      
  final def ins(id : String, author : Option[String] = None, 
      date : Option[java.time.ZonedDateTime] = None,
      rPr : Option[RPr] = None)(rb : RB[Unit])(implicit mo : Mergeable[T]) =
      enclosingRun(rb,rPr)(Ins(id,_,author,date))

  final def hyperlink(anchor : Option[String] = None, 
        id : Option[String] = None, 
        tooltip : Option[String] = None,
      rPr : Option[RPr] = None)(rb : RB[Unit])(implicit mo : Mergeable[T])  =
      enclosingRun(rb,rPr)(Hyperlink(_,anchor, id, tooltip))
  
  final def noBreakHyphen = putRC(NoBreakHyphen)
  
  final def sym(font : String, char : String) = putRC(Sym(font, char))
  
  final def text(text : String, preserveSpace : Boolean = true) =
      putRC(T(text, preserveSpace))
  
  final def tab(pos : Int, tabType : TabStopType, leader : TabLeader = TL_None) =
    putRC(Tab(pos,tabType,leader))
  
  final def ptab(alignment : PTabAlignment,leader : Option[TabLeader] = None,
            relativeTo : PTabBase) =
     putRC(PTab(alignment,leader,relativeTo))
  
  final def contentPart(id : String) = putRC(ContentPart(id))   
  
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
  
 
