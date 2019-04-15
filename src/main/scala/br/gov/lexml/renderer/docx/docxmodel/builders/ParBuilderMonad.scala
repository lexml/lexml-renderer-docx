package br.gov.lexml.renderer.docx.docxmodel.builders

import cats.{implicits => catImplicits, _}
import cats.data._
import catImplicits._
import br.gov.lexml.renderer.docx.docxmodel._
  
final case class ParBuilderState[Q](
    contents : Seq[ParElement] = Seq(),
    value : Q) extends Modifiable[Q,ParBuilderState[Q]] {
  def setValue(x : Q) = copy(value = x)
}

trait ParBuilderOps[T] {  
  import implicits._
          
  final type PST = ParBuilderState[T]
  
  final type PB[A] = ParBuilderMonad[T,A]
      
  final def putPE(pe : ParElement*) : PB[Unit] = 
    State.modify((x : PST) => x.copy(contents = x.contents ++ pe)) 
      
  final def getPState : PB[T] = State.inspect(_.value)
  
  final def setPState(v : T) : PB[Unit] = State.modify(_.copy(value = v))
  
  final def mergePState[Q](v : Q)(implicit mo : Mergeable2[T,Q]) : PB[Unit] = 
    State.modify(x => x.copy(value = mo.merge(x.value,v)))
  
  final def modifyPState(f : T => T) : PB[Unit] = 
    State.modify(x => x.copy(value = f(x.value)))
    
  final def inspectPState[A](f : T => A) : PB[A] =
      State.inspect(x => f(x.value))  
  
  final def modifyPStateV[A](f : T => (T,A)) : PB[A] = State { st =>
    val (v1,res) = f(st.value)
    (st.copy(value = v1),res)
  }    
      
  final def runM[Q : Mergeable](rPr : Option[RPr] = None)(r : RunBuilderMonad[Q,Unit])(implicit mo : Mergeable2[T,Q]) 
          : PB[Unit] = { 
    for {
      v0 <- getPState
      vv0 = mo.extract(v0)
      (run,v1) = r.makeRun(vv0,rPr).value
      _ <- putPE(run)
      _ <- mergePState(v1)
    } yield (())
  }
  
  
}
  
 
