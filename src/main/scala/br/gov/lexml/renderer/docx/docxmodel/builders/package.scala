package br.gov.lexml.renderer.docx.docxmodel

import cats.data._

package object builders {

type RunBuilderMonad[T,A] = State[RunBuilderState[T],A]

type RunBuilderMonadStmt[T] = RunBuilderMonad[T,Unit]  
  
type ParBuilderMonad[T,A] = State[ParBuilderState[T],A]

type ParBuilderMonadStmt[T] = ParBuilderMonad[T,Unit]  

type MainDocBuilderMonad[T,A] = State[MainDocBuilderState[T],A]

type MainDocBuilderMonadStmt[T] = MainDocBuilderMonad[T,Unit]  

 def mapM_[T,S](x : Seq[T])(f : T => State[S,_]) : State[S,Unit] = {
      if(x.isEmpty) { State.pure(()) } else {
        val (h,t) = (x.head,x.tail)
        for {
          _ <- f(h)
          _ <- mapM_(t)(f)
        } yield (())            
      }
    }

type Mergeable[T] = Mergeable2[T,T]
}


