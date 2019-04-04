package br.gov.lexml.renderer.docx.test

import br.gov.lexml.renderer._

import br.gov.lexml.schema.scala._
import org.apache.commons.io.FileUtils
import com.typesafe.config._


object TestDocxRenderer extends App {
  val config = ConfigFactory.load()
  println(config)
  val renderer = RendererSettigs.renderer("docx-renderer")
  println("renderer = " + renderer)
  import java.io.File
  import scala.collection.JavaConverters._
  val sampleDir = new File("/home/joao/workspace_oxygen2/lexml-schema-scala/src/test/samples")
  println(s"${sampleDir.isDirectory()}, ${sampleDir.exists()}")
  val files1 = Option(sampleDir.listFiles()).getOrElse(Array()).filterNot(_ == null)  
  def file_filter(f : File) = 
      f != null && f.getName().endsWith(".xml")
  println("files1: " + files1.take(2))
  val files = files1.filter(file_filter).to[Seq]
  println(s"files.size = ${files.size}")
  val samples = files.flatMap { f => 
    try { 
      println("Parsing xml: " + f)
      Some((f,LexmlSchema(f))) 
    } catch { 
      case ex : Exception =>         
        None 
    } }
  println(s"samples.size = ${samples.size}")
  val destDir = new File("/tmp/results")
  try { destDir.mkdirs() } catch { case _ : Exception => }
  samples.take(1) foreach { case (f,doc) =>
    println("Rendering: " + f)
    val rendered = renderer.render(doc)    
    val dst = new File(destDir,f.getName().replaceAll("\\.xml",".docx"))
    println("Writing to " + dst.getPath)
    FileUtils.writeByteArrayToFile(dst,rendered.data)    
  }     
}