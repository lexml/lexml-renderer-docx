package br.gov.lexml.renderer.docx.test


import br.gov.lexml.renderer.docx.docxmodel._
import br.gov.lexml.renderer.docx._

import br.gov.lexml.schema.scala._
import org.apache.commons.io.FileUtils
import br.gov.lexml.doc.xml.XmlConverter
import org.apache.commons.io.IOUtils
import br.gov.lexml.doc._
import br.gov.lexml.renderer.docx.renderers.PackageRenderer

object TestRendererV4  extends App {  
  import java.io.File
  import scala.collection.JavaConverters._
  
  val lexmlToDocxConfig = LexmlToDocxConfig()

  val lexmlToDocx = new LexmlToDocx(lexmlToDocxConfig)
        
  val sampleDir = new File("./src/test/samples")
  println(s"${sampleDir.isDirectory()}, ${sampleDir.exists()}")
  
  val files1 = Option(sampleDir.listFiles()).getOrElse(Array()).filterNot(_ == null)  
  def file_filter(f : File) = 
      f != null && f.getName().endsWith(".xml")
      
  val limit = 5000
  val files = files1.filter(file_filter).to[Seq].take(limit)
  
      
  println(s"files.size = ${files.size}")
 
  val destDir = new File("/tmp/results")
  try { destDir.mkdirs() } catch { case _ : Exception => }
  
  var count : Int = 1
  
  def addOriginal(old : Option[Array[Byte]]) : Option[Array[Byte]] = {
    import scala.xml._
    val oldXml = old.map(x => XML.loadString(new String(x,"utf-8"))).
      getOrElse(<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"/>)
    val newXml = oldXml.copy(
        child = oldXml.child :+ (<Relationship Id="OriginalLexMLDocument" Type="http://www.lexml.gov.br/doc"
                              Target="original.xml"/>)
                              )
    Some(PackageRenderer.xmlToByteArray(newXml))
  }
  
  files.foreach({ 
    case f =>      
      val fname = f.getName.substring(0,f.getName.length()-4)      
      println(s"(${count}): Rendering: " + f + " to " + fname)
      count = count + 1
      val dst = new File(destDir,fname + ".docx")
      val dst2 = new File(destDir,fname + ".xml")
      FileUtils.copyFile(f, dst2) 
      val src = FileUtils.readFileToByteArray(f)
      val res = lexmlToDocx.convert(src,
          Seq(
           ("original.xml" -> ((_ : Option[Array[Byte]]) => Some(src))),
           ("_rels/.rels" -> addOriginal)
              ))
      FileUtils.writeByteArrayToFile(dst,res)      
    })
}    
        
