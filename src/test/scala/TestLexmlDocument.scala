import br.gov.lexml.renderer.docx._
import scala.xml._

import br.goc.lemxl.renderer.docx.PP
import org.apache.commons.io.FileUtils
import java.io.File

object TestLexmlDocument extends App {

  val src_dir = new File("/tmp/lexml_test")
  val srcs = src_dir.list().filter(_.endsWith(".xml")).filterNot(_.endsWith(".gen.xml"))
  def process(i : int) {
    
  }
  val threads = (1 to 8).map { n => new Thread("Runner" + n) {
    var i = n
    while(i < srcs.length) {
      process(i)
      i = i + 8
    }
  }
  val xml = XML.loadFile("/tmp/t.xml")
  
  val doc = LexmlDocument(xml)
  
  FileUtils.writeStringToFile(new java.io.File("/tmp/result.txt"), PP.prettyPrint(doc),"utf-8")
  FileUtils.writeStringToFile(new java.io.File("/tmp/result.xml"), 
      doc.xml().toString(),"utf-8")
  
}