package br.gov.lexml.renderer.docx.docxmodel.builders

import cats.*
import cats.data.*
import cats.implicits.*
import br.gov.lexml.renderer.docx.docxmodel.*

import scala.annotation.{targetName, unused}

object implicits:
  extension[T](r : RunBuilderMonadStmt[T]) def makeRun(v0 : T,rPr : Option[RPr] = None) : Eval[(R,T)] =
      r.run(RunBuilderState(value = v0)) map { (rs,_) =>
          (R(rPr = rPr, contents = rs.contents),rs.value)          
      }
  
  extension[T](r : ParBuilderMonadStmt[T]) def makePar(v0 : T,pPr : Option[PPr] = None) : Eval[(P,T)] =
      r.run(ParBuilderState(value = v0)) map { (rs,_) =>
          (P(runs = rs.contents, pPr = pPr),rs.value)          
      }

  import br.gov.lexml.renderer.docx.docxmodel.proc.trimP
  extension[T](d : DocxCompSeqBuilderMonadStmt[T])
    def makeMainDoc(v0 : T) : Eval[(DocxMainDocument,T)] =
      makeDocxCompSeq(v0).map { case (contents,v) => (DocxMainDocument(contents = contents),v) }

    def makeDocxCompSeq(v0 : T) : Eval[(Seq[DocxTextComponent],T)] =
      d.run(DocxCompSeqBuilderState(value = v0)) map { (rs,_) =>
        val contents = rs.contents
        val contents1 =  contents.collect {
          case p : P => trimP(p)
          case x => x
        }
        (contents1,rs.value)
      }
  
  extension[S,A](x : State[S,A])
    @targetName("seq")
    def >>[B](y : State[S,B]): IndexedStateT[Eval, S, S, B] = x.flatMap(_ => y)
    @targetName("flatMapOp")
    @unused
    def >>=[B](y : A => State[S,B]): IndexedStateT[Eval, S, S, B] = x.flatMap(y)

  extension[T](v : Option[T])
      def ifDef[A](f : T => State[A,_]) : State[A,Unit] = v match {
        case None => State { (x : A) => (x,()) }
        case Some(x) => State { st =>
          val (st1,_) = f(x).run(st).value
          (st1 : A,())
        }
      }
end implicits

