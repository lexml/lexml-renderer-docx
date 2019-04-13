package br.gov.lexml.renderer.docx.test


import br.gov.lexml.renderer.docx.ver4.Constants
import br.gov.lexml.renderer.docx.ver4.MainDocRenderer
import br.gov.lexml.renderer.docx.docxmodel._

import br.gov.lexml.schema.scala._
import org.apache.commons.io.FileUtils
import com.typesafe.config._
import br.gov.lexml.doc.xml.XmlConverter
import org.apache.commons.io.IOUtils
import br.gov.lexml.renderer.docx.ver3.DocxMainPartRenderer
import br.gov.lexml.doc._



object TestRendererV4  extends App {  
  import java.io.File
  import scala.collection.JavaConverters._
  
  
  val consts = Constants(
   formulaPromulgacaoParStyle = None, // Option[PPr]
   epigrafeParStyle = Some(PPr(
       jc = Some(JC_Center),
       spacing = Some(Spacing(beforeLines = Some(2.0),afterLines=Some(2.0)))
       )), // Option[PPr] 
   ementaParStyle = Some(PPr(       
       ind = Some(Ind(start=2500, firstLine=400)),
       spacing = Some(Spacing(beforeLines = Some(2.0),afterLines=Some(2.0))),
       jc = Some(JC_Both)
       )), 
   preambuloParStyle = Some(PPr(              
       spacing = Some(Spacing(beforeLines = Some(2.0),afterLines=Some(2.0)))
       )), // Option[PPr]
   hyperlinkRPr = None, // Option[RPr]   
   omissisParStyle = None, // Option[PPr]           
   textoAlteracaoRPrStyle = None, // Option[RPr] 
   indentAlteracao = Ind(start=100,hanging=100),
   nomeAgrupadorPredefPPrStyles = Map[TipoAgrupadorPredef,PPr]( // Map[TipoAgrupadorPredef,PPr]
       ).withDefaultValue(PPr(jc = Some(JC_Center))),
   nomeAgrupadorPredefRPrStyles = Map[TipoAgrupadorPredef,RPr]( // Map[TipoAgrupadorPredef,RPr]
       TAP_Secao -> RPr(italics = Some(true)),
       TAP_Subsecao -> RPr(italics = Some(true)) 
       ).withDefaultValue(RPr(italics = Some(true), capsMode = Some(CM_Caps))),  
   rotuloAgrupadorPredefPPrStyles = Map[TipoAgrupadorPredef,PPr]( // Map[TipoAgrupadorPredef,PPr]
       ).withDefaultValue(PPr(jc = Some(JC_Center))),
   rotuloAgrupadorPredefRPrStyles = Map[TipoAgrupadorPredef,RPr]( // Map[TipoAgrupadorPredef,RPr]
       ).withDefaultValue(RPr(bold = Some(true), capsMode = Some(CM_Caps))),    
   rotuloStyleRPrForDispositivos = Map( // Map[TipoDispositivo,RPr],
       ).withDefaultValue(RPr()),
   contentStyleRPrForDispositivos = Map( // Map[TipoDispositivo,RPr]
       ),
   contentStylePPrForDispositivos  = Map( // Map[TipoDispositivo,PPr] 
       )
  )
  println("consts = " + consts)
  
  val referenceDocx = FileUtils.readFileToByteArray(new File("src/main/resources/docx/reference.docx"))
  val referenceEntries = {
    import java.io._
    import java.util.zip._
    val zis = new ZipInputStream(new ByteArrayInputStream(referenceDocx))
    val b = Map.newBuilder[String,Array[Byte]]
    var ze : ZipEntry = zis.getNextEntry()
    while(ze != null) {
      val data = IOUtils.toByteArray(zis)
      b += (ze.getName -> data)
      zis.closeEntry()
      ze = zis.getNextEntry()
    }
    zis.close()
    b.result()
  }
     
  def writeReplace(f : File, data : (String,Array[Byte])*) {
    val m : Map[String,Array[Byte]] = data.toMap
    import java.io._
    import java.util.zip._    
    val zos = new ZipOutputStream(new FileOutputStream(f))
    val m1 = referenceEntries ++ m
    m1.foreach { case (name,data) =>
      val ze = new ZipEntry(name)
      ze.setSize(data.length)
      zos.putNextEntry(ze)
      zos.write(data)
      zos.closeEntry()
    }
    zos.close()        
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
  val styles = Styles(
        styles = Seq(
            Style(id = "DefaultParagraph",`type` = ST_Paragraph)
              .setName("Default Paragraph")
              .setLink("DefaultCharacter")
              .setUIPriority(1)
              .copy(default = true,
                  pPr = Some(PPr(
                      spacing = Some(Spacing(line=Some(1.5)))
                      ))),
            Style(id = "DefaultCharacter",`type` = ST_Character)
              .setName("Default Character")
              .setLink("DefaultParagraph")
              .setUIPriority(1)
              .copy(default = true)))    
  val stylesElem = styles.asXML  //  DocxMainPartRenderer.stylesElem            
  samples.foreach({ 
    case (f,doc) =>
      val fname = s"result${count}"      
      println(s"(${count}): Rendering: " + f + " to " + fname)
      count = count + 1
      val dst2 = new File(destDir,fname + ".xml")
      FileUtils.copyFile(f, dst2) 
      try {
        
        val renderer = new MainDocRenderer(consts)
        val res = renderer.render(doc)
        println("HREFs:")
        res.idToHref.foreach{ case (id,href) => println(s"${id} -> ${href}") }
        res.unsupportedCases.foreach { case (msg,x) =>
          println(msg)
          println(x.toString)
        }        
        val mainDoc = res.doc.asXML
        val dst = new File(destDir,fname+".docx")    
        println("Writing to " + dst.getPath)
        writeReplace(dst,
                  "word/document.xml" -> xmlToByteArray(mainDoc),
                  "word/styles.xml" -> xmlToByteArray(stylesElem) )

      } catch {
        case ex : Exception => 
          println("Ooops:")
          ex.printStackTrace()
      }
    })
}    
        
