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

class LexmlToDocxConfig(
    val referenceDocx : Array[Byte] = LexmlToDocxConfig.defaultReferenceDocx.getOrElse(sys.error("É preciso informar um documento de referencia")),
    val constants : Constants = Constants.default
    )

object LexmlToDocxConfig {
  protected [LexmlToDocxConfig] val logger = LoggerFactory.getLogger(classOf[LexmlToDocxConfig])
  private lazy val defaultReferenceDocx : Option[Array[Byte]] = {
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
}

object LexmlToDocx {
  protected [LexmlToDocx] val logger = LoggerFactory.getLogger(classOf[LexmlToDocx])
}

class LexmlToDocx(config : LexmlToDocxConfig) {
  import LexmlToDocx.logger
  
  private lazy val consts = Constants.default
  private lazy val renderer = new PackageRenderer(config.referenceDocx)
  
  def convert(source : Array[Byte]) : Array[Byte] = {
    val sourceMD5 = DigestUtils.md5Hex(source)
    logger.info(s"Parse do documento LexML: md5 = ${sourceMD5}")
    val xml = LexmlSchema(source)
    logger.info("Conversão para objetos de domínio")
    val doc = XmlConverter.scalaxbToModel(xml)
    logger.info("Renderização para DOCX")
    val res = renderer.render(doc)
    val resMD5 = DigestUtils.md5Hex(res)
    logger.info(s"Resultado len=${res.length}, md5 = ${resMD5}")
    res  
  }
}