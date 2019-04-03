package br.gov.lexml.renderer.docx

import org.docx4j.openpackaging.packages.WordprocessingMLPackage
import org.docx4j.wml.ArrayListWml
import org.docx4j.wml.P
import org.docx4j.wml.R
import org.docx4j.wml.Text
import scala.util.matching.Regex
import br.gov.lexml.renderer._
import br.gov.lexml.schema.scala.{data => X}
import br.gov.lexml.{doc => M}
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import br.gov.lexml.doc.xml.XmlConverter
import org.docx4j.Docx4J
import org.docx4j.openpackaging.parts.WordprocessingML.MainDocumentPart
import org.docx4j.openpackaging.parts.WordprocessingML.DocumentSettingsPart
import org.docx4j.jaxb.Context
import org.docx4j.wml.ObjectFactory
import java.math.BigInteger
import org.docx4j.wml.Style.BasedOn


object DocxBuilder {  
  val STYLE_NORMAL = "Normal"
  val STYLE_FORMULA_PROMULGACAO = "FormulaPromulgacaoP"
  val STYLE_EPIGRAFE = "EpigrafeP"
  val STYLE_EMENTA = "EmentaP"
  val STYLE_PREAMBULO = "PreambuloP"
}

class DocxBuilder(config : Config) {
  import DocxBuilder._
  
  private val wordMLPackage : WordprocessingMLPackage = WordprocessingMLPackage.createPackage()
  private val mdp : MainDocumentPart = wordMLPackage.getMainDocumentPart
  private val dsp : DocumentSettingsPart = mdp.getDocumentSettingsPart(true)
  private val factory : ObjectFactory = Context.getWmlObjectFactory  
  private val styles = factory.createStyles()
  
  
    
  def setupDocDefaults() {  
    val docdefaults = factory.createDocDefaults()
    val rPrDefault = factory.createDocDefaultsRPrDefault()
    docdefaults.setRPrDefault(rPrDefault)
    val rpr = factory.createRPr()
    rPrDefault.setRPr(rpr)    
    val hpsmeasure = factory.createHpsMeasure()
    rpr.setSz(hpsmeasure)
    hpsmeasure.setVal(BigInteger.valueOf(14))
    val rfonts = factory.createRFonts()
    rpr.setRFonts(rfonts)
    rfonts.setCs("FreeSans")
    rfonts.setAscii("Liberation Serif")
    rfonts.setHAnsi("Liberation Serif")
    rfonts.setEastAsia("Droid Sans Fallback")
    val hpsmeasure2 = factory.createHpsMeasure() 
    rpr.setSzCs(hpsmeasure2) 
    hpsmeasure2.setVal( BigInteger.valueOf(14)); 
    val language = factory.createCTLanguage()
    rpr.setLang(language); 
    language.setVal( "pt-BR"); 
    language.setBidi( "hi-IN"); 
    language.setEastAsia( "zh-CN"); 
    val docdefaultspprdefault = factory.createDocDefaultsPPrDefault() 
    docdefaults.setPPrDefault(docdefaultspprdefault) 
    val ppr = factory.createPPr() 
    docdefaultspprdefault.setPPr(ppr)     
  }
  
    
  def createParagraphStyle(styleId : String) {
    val style = factory.createStyle()    
    style.setStyleId(styleId)
    val bon = new BasedOn()
    bon.setVal(STYLE_NORMAL)
    style.setType("paragraph")
    style.setBasedOn(bon)
    styles.getStyle().add(style)
  }

  def createNormalStyle() {
    val style = factory.createStyle()
    styles.getStyle().add(style)
    style.setStyleId("Normal")
    // Create object for rPr
    val rpr2 = factory.createRPr()
    style.setRPr(rpr2)
    // Create object for sz
    val hpsmeasure3 = factory.createHpsMeasure()
    rpr2.setSz(hpsmeasure3)
    hpsmeasure3.setVal(BigInteger.valueOf(14))
    // Create object for color
    val color = factory.createColor()
    rpr2.setColor(color)
    color.setVal("00000A")
    // Create object for rFonts
    val rfonts2 = factory.createRFonts()
    rpr2.setRFonts(rfonts2)
    rfonts2.setCs("FreeSans")
    rfonts2.setAscii("Liberation SerifTimes New Roman")
    rfonts2.setHAnsi("Liberation SerifTimes New Roman")
    rfonts2.setEastAsia("Droid Sans Fallback")
    // Create object for szCs
    val hpsmeasure4 = factory.createHpsMeasure()
    rpr2.setSzCs(hpsmeasure4)
    hpsmeasure4.setVal(BigInteger.valueOf(14))
    // Create object for lang
    val language2 = factory.createCTLanguage()
    rpr2.setLang(language2)
    language2.setVal("pt-BR")
    language2.setBidi("hi-IN")
    language2.setEastAsia("zh-CN")
    // Create object for pPr
    val ppr2 = factory.createPPr()
    style.setPPr(ppr2)
    // Create object for bidi
    val booleandefaulttrue = factory.createBooleanDefaultTrue()
    ppr2.setBidi(booleandefaulttrue)
    // Create object for jc
    val jc = factory.createJc()
    ppr2.setJc(jc)
    jc.setVal(org.docx4j.wml.JcEnumeration.LEFT)
    // Create object for widowControl
    val booleandefaulttrue2 = factory.createBooleanDefaultTrue()
    ppr2.setWidowControl(booleandefaulttrue2)
    // Create object for suppressAutoHyphens
    val booleandefaulttrue3 = factory.createBooleanDefaultTrue()
    ppr2.setSuppressAutoHyphens(booleandefaulttrue3)
    // Create object for name
    val stylename = factory.createStyleName()
    style.setName(stylename)
    stylename.setVal(STYLE_NORMAL)
    style.setType("paragraph")
    styles.getStyle().add(style)
  }
  
  def setupStyles() {
    setupDocDefaults()
    createNormalStyle()
    createParagraphStyle(STYLE_FORMULA_PROMULGACAO)
    createParagraphStyle(STYLE_EPIGRAFE)
    createParagraphStyle(STYLE_EMENTA)
    createParagraphStyle(STYLE_PREAMBULO)
  }
  
  def init() {    
    val compat = factory.createCTCompat()
    compat.setCompatSetting("compatibilityMode", "http://schemas.microsoft.com/office/word", "15")    
    dsp.getContents.setCompat(factory.createCTCompat())    
    setupStyles()
    mdp.getStyleDefinitionsPart().setContents(styles)
    
  }
     
  def assemble() : Array[Byte] = {
    val bos = new java.io.ByteArrayOutputStream()    
    Docx4J.save(wordMLPackage,bos)
    try { bos.close() } catch { case _ : Exception =>  }
    bos.toByteArray()
  }
  
  private var currentParStyle : String = STYLE_NORMAL
  
  def parStyle(name : String)(f : => Any) {
    val old = currentParStyle
    currentParStyle = name
    f
    currentParStyle = old        
  }
  
  var currentParagraph : Option[P] = None
  
  def par(f : => Any) {
    if(currentParagraph.isDefined) { f } else {
      val p = factory.createP()
      currentParagraph = Some(p)
      mdp.addObject(p)
      f
      currentParagraph = None
    }     
  }
  
  private var currentRunStyle : String = STYLE_NORMAL
  
  def runStyle(name : String)(f : => Any) {
    val old = currentRunStyle
    currentRunStyle = name
    f
    currentRunStyle = old        
  }
  
  def text(txt : String) {
    def f() {
      val t = factory.createText()
      val textWrapped = factory.createRT(t)
      val r = factory.createR()
      r.getContent().add(textWrapped)
      t.setValue(txt)
      t.setSpace("preserve")
      currentParagraph.get.getContent.add(r)
    }
    if(!currentParagraph.isDefined) {
      par(f)
    } else {
      f
    }         
  }
}

object DocxRendering {
  val mimeType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
}


class DocxRendering(doc : M.LexmlDocument, config : Config) {
  private val docx = new DocxBuilder(config)
  
  def render() : RenderedDocument = {
    docx.init()
    doc.contents match {
      case pj : M.ProjetoNorma => projetoNorma(pj)
      case n : M. Norma => norma(n)
      case x => sys.error("FIXME: unsupported doc. content: " + x)
    }
    assemble()
  }
  
  private def assemble() : RenderedDocument = {
    val data = docx.assemble()
    ByteArrayDocument(data,DocxRendering.mimeType)    
  }
  
  private def projetoNorma(pj : M.ProjetoNorma) {
    norma(pj.norma)  
  }
  
  private def norma(n : M.Norma) {
    hierarchicalStructure(n.contents)
  }
  
  private def hierarchicalStructure(hs : M.HierarchicalStructure) {
    hs.formulaPromulgacao.foreach(formulaPromulgacao)
    hs.epigrafe.foreach(epigrafe)
    hs.ementa.foreach(ementa)
    hs.preambulo.foreach(preambulo)
    hierarquicalElements(hs.articulacao.elems)
  }
  
  def docx_paragrafo(inl : M.InlineSeq) {
    docx.par {
      inl.mixedElems.elems.foreach {
        case Right(txt) => docx.text(txt)
        case Left(v) =>
      }
    }
  }
  
  def formulaPromulgacao(f : M.FormulaPromulgacao) {
    docx.parStyle(DocxBuilder.STYLE_FORMULA_PROMULGACAO) {
      docx_paragrafo(f.inlineSeq)
    }
  }
  
  def epigrafe(e : M.Epigrafe) {
    docx.parStyle(DocxBuilder.STYLE_EPIGRAFE) {
      docx_paragrafo(e.inlineSeq)
    }
  }
  
  def ementa(e : M.Ementa) {
    docx.parStyle(DocxBuilder.STYLE_EMENTA) {
      docx_paragrafo(e.inlineSeq)
    }
  }
  
  def preambulo(p : M.Preambulo) {
    docx.parStyle(DocxBuilder.STYLE_PREAMBULO) {
      p.inlineSeqs.foreach{ x => docx_paragrafo(x.inlineSeq) }
    }
  }
  
  def hierarquicalElements(els : Seq[M.HierarchicalElement]) {
    
  }
  
}

class DocxRenderer extends Renderer {
  override def render(doc1 : X.LexML,config : Config = ConfigFactory.empty()) : RenderedDocument = {
    
    val doc = XmlConverter.scalaxbToModel(doc1)
    new DocxRendering(doc,config).render()       
  }
}