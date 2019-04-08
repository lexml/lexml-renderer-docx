package br.gov.lexml.renderer.docx

import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.EnumerationReader._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase
import scala.xml._
import net.ceedubs.ficus.readers.ValueReader
import com.typesafe.config.Config



object Elemento extends Enumeration {
  val formulaPromulgacao = Value("formulaPromulgacao")
  val epigrafe = Value("epigrafe")
  val ementa = Value("ementa")
  val preambulo = Value("preambulo")      
  val livro = Value("livro")
  val parte = Value("parte")
  val capitulo = Value("capitulo")
  val secao = Value("secao")
  val subsecao = Value("subsecao")      
  val artigo = Value("artigo")
  val caput = Value("caput")
  val paragrafo = Value("paragrafo")
  val inciso = Value("inciso")
  val alinea = Value("alinea")
  val item = Value("item")
  val pena = Value("pena")      
}


final case class ElementoConfig(
    templateRotulo : Option[Template] = None,
    templateNomeAgrupador : Option[Template] = None,
    templateTitulo : Option[Template] = None,
    templateConteudoXml : Option[Template] = None,
    templateElemento : Option[Template] = None)  

final case class DocxRendererConfig(
    elementos : Map[Elemento.Value,ElementoConfig] = Map(),
    indentacaoAlteracao : Option[Int] = None,
    estrangeirismos : Set[String] = Set(),
    templateGeral : Option[Template] = None)

object DocxRendererConfig {
  
  implicit val templateReader: ValueReader[Template] = new ValueReader[Template] {
    def read(config: Config, path: String): Template = {
      Template(config.getString(path))      
    }
  }  
  
  def fromConfig(config : Config) = config.atKey("root").as[DocxRendererConfig]("root")
}

