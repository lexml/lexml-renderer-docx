package br.gov.lexml.renderer.docx.tool

import br.gov.lexml.renderer.docx.{LexmlToDocx, LexmlToDocxConfig}
import org.apache.commons.io.FileUtils

import java.io.File

object RendererTool:
  def main(args : Array[String]) : Unit =
    val srcFile = File(args(0))
    val destFile = File(args(1))
    val lexmlToDocxConfig = LexmlToDocxConfig()
    val lexmlToDocx = new LexmlToDocx(lexmlToDocxConfig)
    val src = FileUtils.readFileToByteArray(srcFile)
    val res = lexmlToDocx.convert(src)
    FileUtils.writeByteArrayToFile(destFile, res)