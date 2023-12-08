package br.gov.lexml.renderer.docx.test


import br.gov.lexml.renderer.docx._

import org.apache.commons.io.FileUtils

object TestRendererV4  extends App:
  import java.io.File

  val lexmlToDocxConfig = LexmlToDocxConfig()

  val lexmlToDocx = LexmlToDocx(lexmlToDocxConfig)
        
  val sampleDir = File("/tmp/test/samples")
  
  val files1 = Option(sampleDir.listFiles()).getOrElse(Array[File]()).filterNot(_ == null)
  def file_filter(f : File) = 
      f != null && f.getName.endsWith(".xml")
      
  val limit = 5000
  val files = files1.filter(file_filter).to(Seq).take(limit)         
 
  val destDir = new File("/tmp/test/results")
  try { destDir.mkdirs() } catch { case _ : Exception => }
  
  var count : Int = 1    
  
  files.foreach { f =>
     val fname = f.getName.substring(0,f.getName.length()-4)
     println(s"(${count}): Rendering: " + f + " to " + fname)
     count = count + 1
     val dst = new File(destDir,fname + ".docx")
     val dst2 = new File(destDir,fname + ".xml")
     FileUtils.copyFile(f, dst2)
     val src = FileUtils.readFileToByteArray(f)
     val res = lexmlToDocx.convert(src)
     FileUtils.writeByteArrayToFile(dst,res)
  }
end TestRendererV4

        
