package br.gov.lexml.renderer.docx.docxmodel.builders

trait Modifiable[Q,T]:
  self : Modifiable[Q,T] =>
  def value : Q
  def setValue(x : Q) : T
  final def modify(f : Q => Q): T = setValue(f(value))

trait Mergeable2[T,Q]:
  def extract(x : T) : Q
  def merge(x : T, y : Q) : T