package br.gov.lexml.renderer.docx.docxmodel.builders


 

import cats.{implicits => catImplicits, _}
import cats.data._
import catImplicits._
import br.gov.lexml.renderer.docx.docxmodel._
  
final case class MainDocBuilderState[Q](
    contents : Seq[DocxTextComponent] = Seq(),
    value : Q) extends Modifiable[Q,MainDocBuilderState[Q]] {
  def setValue(x : Q) = copy(value = x)
}

trait MainDocBuilderOps[T] {  
  import implicits._        
  
  final type MST = MainDocBuilderState[T]
  
  final type MB[A] = MainDocBuilderMonad[T,A]
      
  final def putDTC(pe : DocxTextComponent*) : MB[Unit] = 
    State.modify((x : MST) => x.copy(contents = x.contents ++ pe)) 
  
  final def par(p : P) = putDTC(p)
  
  final def getMDState : MB[T] = State.inspect(_.value)
  
  final def setMDState(v : T) : MB[Unit] = 
    State.modify(_.copy(value = v))
       
  final def mergeMDState[Q](v : Q)(implicit mo : Mergeable2[T,Q]) : 
    MainDocBuilderMonad[T,Unit] = 
    State.modify(x => x.copy(value = mo.merge(x.value,v)))

  final def modifyMDState(f : T => T) : MB[Unit] = 
    State.modify(x => x.copy(value = f(x.value)))
    
  final def modifyMDStateV[E](f : T => (T,E)) : MB[E] = 
    State { x =>
      val (v,r) = f(x.value)
      (x.copy(value = v),r)  
    }
    
  final def inspectMDState[A](f : T => A) : MB[A] =
      State.inspect(x => f(x.value))
    
  final def parM[Q : Mergeable](pPr : Option[PPr] = None)(p : ParBuilderMonad[Q,Unit])(implicit mo : Mergeable2[T,Q]) 
          : MB[Unit] = { 
    for {
      v0 <- getMDState
      val vv0 = mo.extract(v0)
      val (par,v1) = p.makePar(vv0,pPr).value
      _ <- putDTC(par)
      _ <- mergeMDState(v1)
    } yield (())
  }
  
  final def setDocxTextComponents(elems : Seq[DocxTextComponent]) : MB[Unit] =
    State.modify(_.copy(contents = elems)) 
  
  final def docxTextComponents : MB[Seq[DocxTextComponent]] =
    State.inspect(_.contents) 
  
}
  
 
  
