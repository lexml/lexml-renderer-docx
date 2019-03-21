import br.gov.lexml.renderer.docx._
import scala.xml._

import br.goc.lemxl.renderer.docx.PP
import org.apache.commons.io.FileUtils
import java.io.File

object TestLexmlDocument extends App {

  val src_dir = new File("/tmp/lexml_test")
  case class Src(val f : File) {
    val dstFile = new File(f.getParentFile,
        f.getName.replaceAll(".xml$", ".gen.xml"))
    if(dstFile.getCanonicalPath == f.getCanonicalPath) {
      throw new RuntimeException("ops! :" + f)
    }  
    val dataFile = new File(f.getParentFile,
        f.getName.replaceAll(".xml$", ".data"))
    val logFile = new File(f.getParentFile,
        f.getName.replaceAll(".xml$", ".log"))
    var result : Seq[String] = Seq()
  }
  val srcs = src_dir.listFiles().filter(_.getName.endsWith(".xml")).filterNot(_.getName.endsWith(".gen.xml")).map{Src(_)}
  def process(i : Int) {
    val xml = XML.loadFile(srcs(i).f)
    println(s"Processing ${srcs(i).f} [${i}]")
    val doc = LexmlDocument(xml)
    FileUtils.writeStringToFile(srcs(i).dstFile, 
        doc.xml().toString(),"utf-8")
    FileUtils.writeStringToFile(srcs(i).dataFile, 
        PP.prettyPrint(doc),"utf-8")
    val pb = new ProcessBuilder() 
    pb.command("/usr/bin/xmldiff","-c",srcs(i).f.getPath(),srcs(i).dstFile.getPath())
    pb.redirectError(new File("/dev/null"))    
    pb.redirectOutput(srcs(i).logFile)
    val p = pb.start()
    p.waitFor()
    if(srcs(i).logFile.length() == 0) {
      srcs(i).logFile.renameTo(new File(srcs(i).logFile.getPath.replaceAll(".log$",".ok")))
    }
  }
  val tg = new ThreadGroup("Runners")
  val threads = (0 to 7).map { n => new Thread(tg,"Runner" + n) {
    var i = n
    while(i < srcs.length) {
      process(i)
      i = i + 8
    }
  } }
  threads.foreach(_.start())
  threads.foreach(_.join())
  
  
}