package br.gov.lexml.renderer.docx

import br.gov.lexml.renderer.docx.renderers.PackageRenderer
import org.apache.commons.io.FileUtils
import org.apache.commons.io.IOUtils
import br.gov.lexml.renderer.docx.renderers.Constants
import java.io.File
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import br.gov.lexml.doc.xml.XmlConverter
import br.gov.lexml.schema.scala.LexmlSchema
import org.apache.commons.codec.digest.DigestUtils

final case class LexmlToDocxConfig(
    val referenceDocx : Array[Byte],
    val constants : Constants
    )    

    
object LexmlToDocxConfig {
  protected [LexmlToDocxConfig] val logger = LoggerFactory.getLogger(classOf[LexmlToDocxConfig])
  lazy val defaultReferenceDocx : Option[Array[Byte]] = {
    val path = "docx/reference.docx"
    try {      
      val is = this.getClass.getClassLoader.getResourceAsStream(path)
      Some(IOUtils.toByteArray(is))
    } catch {
      case ex : Exception =>
        logger.warn(s"Não foi possível obter documento de referência default em ${path}",ex) 
        None
    }
  }
  
  def apply() : LexmlToDocxConfig = LexmlToDocxConfig(
      referenceDocx = defaultReferenceDocx.getOrElse(sys.error("É preciso informar um documento de referencia")),
      constants = Constants.default)
}

object LexmlToDocx {
  protected [LexmlToDocx] val logger = LoggerFactory.getLogger(classOf[LexmlToDocx])
}

class LexmlToDocx(config : LexmlToDocxConfig) {
  import LexmlToDocx.logger
  
  private lazy val consts = Constants.default
  private lazy val renderer = new PackageRenderer(config.referenceDocx)
  
  
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
    
  def convert(source : Array[Byte], extraReplace : Seq[(String,PackageRenderer.ReplaceFunc)]) : Array[Byte] = {
    val sourceMD5 = DigestUtils.md5Hex(source)
    logger.info(s"Parse do documento LexML: md5 = ${sourceMD5}")
    val xml = LexmlSchema(source)
    logger.info("Conversão para objetos de domínio")
    val doc = XmlConverter.scalaxbToModel(xml)
    logger.info("Renderização para DOCX")
    val extraReplace1 = Seq[(String,PackageRenderer.ReplaceFunc)](
           ("original.xml" -> ((_ : Option[Array[Byte]]) => Some(source))),
           ("_rels/.rels" -> addOriginal)
              )  ++ extraReplace
    val res = renderer.render(doc,extraReplace1)
    val resMD5 = DigestUtils.md5Hex(res)
    logger.info(s"Resultado len=${res.length}, md5 = ${resMD5}")
    res  
  }
  
  def convert(source : Array[Byte]) : Array[Byte] = 
    convert(source = source, extraReplace = Seq())
  
  
}