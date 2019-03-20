import br.gov.lexml.renderer.docx._
import scala.xml._

import br.goc.lemxl.renderer.docx.PP
import org.apache.commons.io.FileUtils

object TestLexmlDocument extends App {

  val xml = XML.loadFile("/home/joao/git/senado/lexml-xml-samples/LexML/ADC1988--20140805.LexML.xml")
  
  val doc = LexmlDocument(xml)
  
  FileUtils.writeStringToFile(new java.io.File("/tmp/result.txt"), PP.prettyPrint(doc),"utf-8")
  
}