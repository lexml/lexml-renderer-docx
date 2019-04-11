package br.gov.lexml.renderer.docx.docxmodel.builders

import cats._
import cats.data._
import cats.implicits._
import br.gov.lexml.renderer.docx.docxmodel._

object implicits {
  implicit class RunBuilderMonadStmtOps[T](r : RunBuilderMonadStmt[T]) {
    def makeRun(v0 : T,rPr : Option[RPr] = None) : Eval[(R,T)] = {
      r.run(RunBuilderState(value = v0)) map { case (rs,_) =>
          (R(rPr = rPr, contents = rs.contents),rs.value)          
      }
    }
  }
  
  implicit class ParBuilderMonadStmtOps[T](r : ParBuilderMonadStmt[T]) {
    def makePar(v0 : T,pPr : Option[PPr] = None) : Eval[(P,T)] = {
      r.run(ParBuilderState(value = v0)) map { case (rs,_) =>
          (P(runs = rs.contents, pPr = pPr),rs.value)          
      }
    }
  }
  
  implicit class MainDocBuilderMonadStmtOps[T](d : MainDocBuilderMonadStmt[T]) {
    def makeMainDoc(v0 : T) : Eval[(DocxMainDocument,T)] = {
      d.run(MainDocBuilderState(value = v0)) map { case (rs,_) =>
          (DocxMainDocument(contents = rs.contents),rs.value)          
      }
    }
  }
  
  implicit class MyRichState[S,A](x : State[S,A]) {
    def >>[B](y : State[S,B]) = x.flatMap(_ => y)
    def >>=[B](y : A => State[S,B]) = x.flatMap(y)
  }
  
   implicit class RichOption1[T](v : Option[T]) {
      def ifDef[A](f : T => State[A,Q] forSome { type Q }) : State[A,Unit] = v match {
        case None => State { (x : A) => (x,()) }
        case Some(x) => State { st =>
          val (st1,_) = f(x).run(st).value
          (st1,())
        }
      }
    }
   
}
