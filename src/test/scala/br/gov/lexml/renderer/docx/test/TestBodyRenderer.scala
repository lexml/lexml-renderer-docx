package br.gov.lexml.renderer.docx.test

import br.gov.lexml.renderer.docx.ver3.{DocxBodyRenderer, DocxBodyRenderResult}
import br.gov.lexml.schema.scala.LexmlSchema
import br.gov.lexml.doc.xml.XmlConverter
import org.apache.commons.io.FileUtils
import br.gov.lexml.renderer.docx.ver3.DocxMainPartRenderer
import org.apache.commons.compress.utils.SeekableInMemoryByteChannel
import org.apache.commons.compress.archivers.zip._
import scala.xml.XML
import br.gov.lexml.renderer.docx.ver3.ParagraphType
import java.io.File

object TestBodyRenderer extends App {
  ParagraphType.types.foreach(println)
  import java.io.File
  import scala.collection.JavaConverters._
  
  
  val referenceDocx = FileUtils.readFileToByteArray(new File("src/main/resources/docx/reference.docx"))

  val referenceDocxZip = {
    val channel = new SeekableInMemoryByteChannel(referenceDocx)
    new ZipFile(channel)
  }
  
  def writeReplace(f : File, data : (String,Array[Byte])*) {
    val m : Map[String,Array[Byte]] = data.toMap
    import java.io._                   
    val zos = new ZipArchiveOutputStream(new FileOutputStream(f))        
    referenceDocxZip.copyRawEntries(zos, new  ZipArchiveEntryPredicate {    
      def test(ze : ZipArchiveEntry) : Boolean = {
        println(s"Testing ${ze.getName}, given = ${m.keySet}")      
        !m.contains(ze.getName)
      }
    })
    m.foreach { case (name,data) =>
      zos.putArchiveEntry(new ZipArchiveEntry(name))
      zos.write(data)
      zos.closeArchiveEntry()
    }
    try {
      zos.close()      
    } catch { case _ : Exception => }    
  }
  
  val sampleDir = new File("../lexml-schema-scala/src/test/samples")
  println(s"${sampleDir.isDirectory()}, ${sampleDir.exists()}")
  
  val files1 = Option(sampleDir.listFiles()).getOrElse(Array()).filterNot(_ == null)  
  def file_filter(f : File) = 
      f != null && f.getName().endsWith(".xml")
      
  val limit = 50
  val files = files1.filter(file_filter).to[Seq].take(limit)
  
      
  println(s"files.size = ${files.size}")
  val samples = files.flatMap { f => 
    try { 
      println("Parsing xml: " + f)
      val xml = LexmlSchema(f)
      val doc = XmlConverter.scalaxbToModel(xml)
      Some((f,doc)) 
    } catch { 
      case ex : Exception =>         
        None 
    } }
  println(s"samples.size = ${samples.size}")
  val destDir = new File("/tmp/results")
  try { destDir.mkdirs() } catch { case _ : Exception => }
  def xmlToByteArray(e : scala.xml.Elem) = {
    import java.io._
    import scala.xml._
    val w = new StringWriter()
    XML.write(w,e,"utf-8",true,null,MinimizeMode.Always)
    w.close()
    w.toString().getBytes("utf-8")
  }
  var count : Int = 1
  samples.foreach({ 
    case (f,doc) =>
      val fname = s"result${count}"      
      println(s"(${count}): Rendering: " + f + " to " + fname)
      count = count + 1
      val dst2 = new File(destDir,fname + ".xml")
      FileUtils.copyFile(f, dst2) 
      try {
        
        val (mainDoc,styles) = DocxMainPartRenderer.render(doc)    
        val dst = new File(destDir,fname+".docx")    
        println("Writing to " + dst.getPath)
        writeReplace(dst,"word/document.xml" -> xmlToByteArray(mainDoc), "word/styles.xml" -> xmlToByteArray(styles) )

      } catch {
        case ex : Exception => 
          println("Ooops:")
          ex.printStackTrace()
      }
    })
}    
        
