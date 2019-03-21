import br.gov.lexml.renderer.docx._
import scala.xml._

import br.goc.lemxl.renderer.docx.PP
import org.apache.commons.io.FileUtils
import java.io.File

object TestLexmlDocument extends App {

  val src_dir = new File("/tmp/lexml_test")
  val srcs = src_dir.listFiles().filter(_.getName.endsWith(".xml")).filterNot(_.getName.endsWith(".gen.xml"))
  def process(i : Int) {
    val xml = XML.loadFile(srcs(i))
    
    val doc = LexmlDocument(xml)
    
    val dstFile = new File(srcs(i).getParentFile,
        srcs(i).getName.replaceAll(".xml$", ".gen.xml"))
    if(dstFile.getCanonicalPath == srcs(i).getCanonicalPath) {
      throw new RuntimeException("ops!")
    }    
    FileUtils.writeStringToFile(dstFile, 
        doc.xml().toString(),"utf-8")
    val pb = new ProcessBuilder() 
      
  }
  val threads = (0 to 7).map { n => new Thread("Runner" + n) {
    var i = n
    while(i < srcs.length) {
      process(i)
      i = i + 8
    }
  }
  
}