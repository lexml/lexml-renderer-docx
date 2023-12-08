package br.gov.lexml.renderer.docx

import br.gov.lexml.renderer.docx.renderers.PackageRenderer
import org.apache.commons.io.IOUtils
import br.gov.lexml.renderer.docx.renderers.Constants
import org.slf4j.LoggerFactory
import br.gov.lexml.doc.xml.XmlConverter
import br.gov.lexml.schema.scala.LexmlSchema

final case class LexmlToDocxConfig(
    referenceDocx : Array[Byte],
    constants : Constants,
    linkTemplate : Option[String] = None
    ):
  val getLinkTemplate : String =
    linkTemplate.orElse(
      sys.env.get("br.gov.lexml.renderer.docx.link-template")
    ).getOrElse("https://normas.leg.br/?urn=%s")
    
object LexmlToDocxConfig:
  private [LexmlToDocxConfig] val logger = LoggerFactory.getLogger(classOf[LexmlToDocxConfig])
  lazy val defaultReferenceDocx : Option[Array[Byte]] =
    val path = "docx/reference.docx"
    try {      
      val is = this.getClass.getClassLoader.getResourceAsStream(path)
      Some(IOUtils.toByteArray(is))
    } catch {
      case ex : Exception =>
        logger.warn(s"Não foi possível obter documento de referência default em $path",ex)
        None
    }
  
  def apply() : LexmlToDocxConfig = LexmlToDocxConfig(
      referenceDocx = defaultReferenceDocx.getOrElse(sys.error("É preciso informar um documento de referencia")),
      constants = Constants.default)

object LexmlToDocx:
  private [LexmlToDocx] val logger = LoggerFactory.getLogger(classOf[LexmlToDocx])

class LexmlToDocx(config : LexmlToDocxConfig):
  import LexmlToDocx.logger
  
  private lazy val consts = Constants.default
  private lazy val renderer = PackageRenderer(config)
  
  def addOriginal(old : Option[Array[Byte]]) : Option[Array[Byte]] =
    import scala.xml._
    val oldXml = old.map(x => XML.loadString(String(x,"utf-8"))).
      getOrElse(<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"/>)
    val newXml = oldXml.copy(
        child = oldXml.child :+ <Relationship Id="OriginalLexMLDocument" Type="http://www.lexml.gov.br/doc"
                              Target="original.xml"/>
                              )
    Some(PackageRenderer.xmlToByteArray(newXml))
    
  def convert(source : Array[Byte], extraReplace : Seq[(String,PackageRenderer.ReplaceFunc)]) : Array[Byte] =
    val xml = LexmlSchema(source)
    logger.info("Conversão para objetos de domínio")
    val doc = XmlConverter.scalaxbToModel(xml)
    logger.info("Renderização para DOCX")
    val extraReplace1 = Seq[(String,PackageRenderer.ReplaceFunc)](
           "original.xml" -> ((_ : Option[Array[Byte]]) => Some(source)),
           "_rels/.rels" -> addOriginal
              )  ++ extraReplace
    renderer.render(doc,extraReplace1)
  
  def convert(source : Array[Byte]) : Array[Byte] = 
    convert(source = source, extraReplace = Seq())
end LexmlToDocx