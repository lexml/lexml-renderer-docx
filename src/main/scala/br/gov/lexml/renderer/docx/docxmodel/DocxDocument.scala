package br.gov.lexml.renderer.docx.docxmodel

import scala.xml._
import java.io._
import org.apache.commons.io.IOUtils

object XmlUtils {
  def xmlToByteArray(e : Elem) = {
    val w = new StringWriter()
    XML.write(w,e,"utf-8",true,null,MinimizeMode.Always)
    w.close()
    w.toString().getBytes("utf-8")
  }
}

object DocxFile {
  val styleElem = (
      <w:styles xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" mc:Ignorable="w14">
  <w:docDefaults>
    <w:rPrDefault>
      <w:rPr>
        <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri" w:eastAsia="Calibri" w:cs="" w:asciiTheme="minorHAnsi" w:cstheme="minorBidi" w:eastAsiaTheme="minorHAnsi" w:hAnsiTheme="minorHAnsi"/>
        <w:szCs w:val="22"/>
        <w:lang w:val="pt-BR" w:eastAsia="en-US" w:bidi="ar-SA"/>
      </w:rPr>
    </w:rPrDefault>
    <w:pPrDefault>
      <w:pPr/>
    </w:pPrDefault>
  </w:docDefaults>
  <w:latentStyles w:defLockedState="0" w:defUIPriority="99" w:defSemiHidden="0" w:defUnhideWhenUsed="0" w:defQFormat="0" w:count="371">
    <w:lsdException w:name="Normal" w:uiPriority="0" w:qFormat="1"/>
    <w:lsdException w:name="heading 1" w:uiPriority="9"/>
    <w:lsdException w:name="heading 2" w:uiPriority="9" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="heading 3" w:uiPriority="9" w:semiHidden="1" w:unhideWhenUsed="1" w:qFormat="1"/>
    <w:lsdException w:name="heading 4" w:uiPriority="9" w:semiHidden="1" w:unhideWhenUsed="1" w:qFormat="1"/>
    <w:lsdException w:name="heading 5" w:uiPriority="9" w:semiHidden="1" w:unhideWhenUsed="1" w:qFormat="1"/>
    <w:lsdException w:name="heading 6" w:uiPriority="9" w:semiHidden="1" w:unhideWhenUsed="1" w:qFormat="1"/>
    <w:lsdException w:name="heading 7" w:uiPriority="9" w:semiHidden="1" w:unhideWhenUsed="1" w:qFormat="1"/>
    <w:lsdException w:name="heading 8" w:uiPriority="9" w:semiHidden="1" w:unhideWhenUsed="1" w:qFormat="1"/>
    <w:lsdException w:name="heading 9" w:uiPriority="9" w:semiHidden="1" w:unhideWhenUsed="1" w:qFormat="1"/>
    <w:lsdException w:name="index 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="index 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="index 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="index 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="index 5" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="index 6" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="index 7" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="index 8" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="index 9" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="toc 1" w:uiPriority="39" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="toc 2" w:uiPriority="39" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="toc 3" w:uiPriority="39" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="toc 4" w:uiPriority="39" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="toc 5" w:uiPriority="39" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="toc 6" w:uiPriority="39" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="toc 7" w:uiPriority="39" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="toc 8" w:uiPriority="39" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="toc 9" w:uiPriority="39" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Normal Indent" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="footnote text" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="annotation text" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="header" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="footer" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="index heading" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="caption" w:uiPriority="35" w:semiHidden="1" w:unhideWhenUsed="1" w:qFormat="1"/>
    <w:lsdException w:name="table of figures" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="envelope address" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="envelope return" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="footnote reference" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="annotation reference" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="line number" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="page number" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="endnote reference" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="endnote text" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="table of authorities" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="macro" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="toa heading" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List Bullet" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List Number" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List 5" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List Bullet 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List Bullet 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List Bullet 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List Bullet 5" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List Number 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List Number 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List Number 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List Number 5" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Title" w:uiPriority="10" w:qFormat="1"/>
    <w:lsdException w:name="Closing" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Signature" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Default Paragraph Font" w:uiPriority="1" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Body Text" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Body Text Indent" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List Continue" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List Continue 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List Continue 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List Continue 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="List Continue 5" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Message Header" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Subtitle" w:uiPriority="11" w:qFormat="1"/>
    <w:lsdException w:name="Salutation" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Date" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Body Text First Indent" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Body Text First Indent 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Note Heading" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Body Text 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Body Text 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Body Text Indent 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Body Text Indent 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Block Text" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Hyperlink" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="FollowedHyperlink" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Strong" w:uiPriority="22" w:qFormat="1"/>
    <w:lsdException w:name="Emphasis" w:uiPriority="20" w:qFormat="1"/>
    <w:lsdException w:name="Document Map" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Plain Text" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="E-mail Signature" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="HTML Top of Form" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="HTML Bottom of Form" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Normal (Web)" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="HTML Acronym" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="HTML Address" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="HTML Cite" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="HTML Code" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="HTML Definition" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="HTML Keyboard" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="HTML Preformatted" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="HTML Sample" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="HTML Typewriter" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="HTML Variable" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Normal Table" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="annotation subject" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="No List" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Outline List 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Outline List 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Outline List 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Simple 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Simple 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Simple 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Classic 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Classic 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Classic 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Classic 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Colorful 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Colorful 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Colorful 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Columns 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Columns 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Columns 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Columns 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Columns 5" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Grid 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Grid 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Grid 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Grid 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Grid 5" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Grid 6" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Grid 7" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Grid 8" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table List 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table List 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table List 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table List 4" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table List 5" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table List 6" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table List 7" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table List 8" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table 3D effects 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table 3D effects 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table 3D effects 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Contemporary" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Elegant" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Professional" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Subtle 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Subtle 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Web 1" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Web 2" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Web 3" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Balloon Text" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Table Grid" w:uiPriority="39"/>
    <w:lsdException w:name="Table Theme" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="Placeholder Text" w:semiHidden="1"/>
    <w:lsdException w:name="No Spacing" w:uiPriority="1" w:qFormat="1"/>
    <w:lsdException w:name="Light Shading" w:uiPriority="60"/>
    <w:lsdException w:name="Light List" w:uiPriority="61"/>
    <w:lsdException w:name="Light Grid" w:uiPriority="62"/>
    <w:lsdException w:name="Medium Shading 1" w:uiPriority="63"/>
    <w:lsdException w:name="Medium Shading 2" w:uiPriority="64"/>
    <w:lsdException w:name="Medium List 1" w:uiPriority="65"/>
    <w:lsdException w:name="Medium List 2" w:uiPriority="66"/>
    <w:lsdException w:name="Medium Grid 1" w:uiPriority="67"/>
    <w:lsdException w:name="Medium Grid 2" w:uiPriority="68"/>
    <w:lsdException w:name="Medium Grid 3" w:uiPriority="69"/>
    <w:lsdException w:name="Dark List" w:uiPriority="70"/>
    <w:lsdException w:name="Colorful Shading" w:uiPriority="71"/>
    <w:lsdException w:name="Colorful List" w:uiPriority="72"/>
    <w:lsdException w:name="Colorful Grid" w:uiPriority="73"/>
    <w:lsdException w:name="Light Shading Accent 1" w:uiPriority="60"/>
    <w:lsdException w:name="Light List Accent 1" w:uiPriority="61"/>
    <w:lsdException w:name="Light Grid Accent 1" w:uiPriority="62"/>
    <w:lsdException w:name="Medium Shading 1 Accent 1" w:uiPriority="63"/>
    <w:lsdException w:name="Medium Shading 2 Accent 1" w:uiPriority="64"/>
    <w:lsdException w:name="Medium List 1 Accent 1" w:uiPriority="65"/>
    <w:lsdException w:name="Revision" w:semiHidden="1"/>
    <w:lsdException w:name="List Paragraph" w:uiPriority="34" w:qFormat="1"/>
    <w:lsdException w:name="Quote" w:uiPriority="29" w:qFormat="1"/>
    <w:lsdException w:name="Intense Quote" w:uiPriority="30" w:qFormat="1"/>
    <w:lsdException w:name="Medium List 2 Accent 1" w:uiPriority="66"/>
    <w:lsdException w:name="Medium Grid 1 Accent 1" w:uiPriority="67"/>
    <w:lsdException w:name="Medium Grid 2 Accent 1" w:uiPriority="68"/>
    <w:lsdException w:name="Medium Grid 3 Accent 1" w:uiPriority="69"/>
    <w:lsdException w:name="Dark List Accent 1" w:uiPriority="70"/>
    <w:lsdException w:name="Colorful Shading Accent 1" w:uiPriority="71"/>
    <w:lsdException w:name="Colorful List Accent 1" w:uiPriority="72"/>
    <w:lsdException w:name="Colorful Grid Accent 1" w:uiPriority="73"/>
    <w:lsdException w:name="Light Shading Accent 2" w:uiPriority="60"/>
    <w:lsdException w:name="Light List Accent 2" w:uiPriority="61"/>
    <w:lsdException w:name="Light Grid Accent 2" w:uiPriority="62"/>
    <w:lsdException w:name="Medium Shading 1 Accent 2" w:uiPriority="63"/>
    <w:lsdException w:name="Medium Shading 2 Accent 2" w:uiPriority="64"/>
    <w:lsdException w:name="Medium List 1 Accent 2" w:uiPriority="65"/>
    <w:lsdException w:name="Medium List 2 Accent 2" w:uiPriority="66"/>
    <w:lsdException w:name="Medium Grid 1 Accent 2" w:uiPriority="67"/>
    <w:lsdException w:name="Medium Grid 2 Accent 2" w:uiPriority="68"/>
    <w:lsdException w:name="Medium Grid 3 Accent 2" w:uiPriority="69"/>
    <w:lsdException w:name="Dark List Accent 2" w:uiPriority="70"/>
    <w:lsdException w:name="Colorful Shading Accent 2" w:uiPriority="71"/>
    <w:lsdException w:name="Colorful List Accent 2" w:uiPriority="72"/>
    <w:lsdException w:name="Colorful Grid Accent 2" w:uiPriority="73"/>
    <w:lsdException w:name="Light Shading Accent 3" w:uiPriority="60"/>
    <w:lsdException w:name="Light List Accent 3" w:uiPriority="61"/>
    <w:lsdException w:name="Light Grid Accent 3" w:uiPriority="62"/>
    <w:lsdException w:name="Medium Shading 1 Accent 3" w:uiPriority="63"/>
    <w:lsdException w:name="Medium Shading 2 Accent 3" w:uiPriority="64"/>
    <w:lsdException w:name="Medium List 1 Accent 3" w:uiPriority="65"/>
    <w:lsdException w:name="Medium List 2 Accent 3" w:uiPriority="66"/>
    <w:lsdException w:name="Medium Grid 1 Accent 3" w:uiPriority="67"/>
    <w:lsdException w:name="Medium Grid 2 Accent 3" w:uiPriority="68"/>
    <w:lsdException w:name="Medium Grid 3 Accent 3" w:uiPriority="69"/>
    <w:lsdException w:name="Dark List Accent 3" w:uiPriority="70"/>
    <w:lsdException w:name="Colorful Shading Accent 3" w:uiPriority="71"/>
    <w:lsdException w:name="Colorful List Accent 3" w:uiPriority="72"/>
    <w:lsdException w:name="Colorful Grid Accent 3" w:uiPriority="73"/>
    <w:lsdException w:name="Light Shading Accent 4" w:uiPriority="60"/>
    <w:lsdException w:name="Light List Accent 4" w:uiPriority="61"/>
    <w:lsdException w:name="Light Grid Accent 4" w:uiPriority="62"/>
    <w:lsdException w:name="Medium Shading 1 Accent 4" w:uiPriority="63"/>
    <w:lsdException w:name="Medium Shading 2 Accent 4" w:uiPriority="64"/>
    <w:lsdException w:name="Medium List 1 Accent 4" w:uiPriority="65"/>
    <w:lsdException w:name="Medium List 2 Accent 4" w:uiPriority="66"/>
    <w:lsdException w:name="Medium Grid 1 Accent 4" w:uiPriority="67"/>
    <w:lsdException w:name="Medium Grid 2 Accent 4" w:uiPriority="68"/>
    <w:lsdException w:name="Medium Grid 3 Accent 4" w:uiPriority="69"/>
    <w:lsdException w:name="Dark List Accent 4" w:uiPriority="70"/>
    <w:lsdException w:name="Colorful Shading Accent 4" w:uiPriority="71"/>
    <w:lsdException w:name="Colorful List Accent 4" w:uiPriority="72"/>
    <w:lsdException w:name="Colorful Grid Accent 4" w:uiPriority="73"/>
    <w:lsdException w:name="Light Shading Accent 5" w:uiPriority="60"/>
    <w:lsdException w:name="Light List Accent 5" w:uiPriority="61"/>
    <w:lsdException w:name="Light Grid Accent 5" w:uiPriority="62"/>
    <w:lsdException w:name="Medium Shading 1 Accent 5" w:uiPriority="63"/>
    <w:lsdException w:name="Medium Shading 2 Accent 5" w:uiPriority="64"/>
    <w:lsdException w:name="Medium List 1 Accent 5" w:uiPriority="65"/>
    <w:lsdException w:name="Medium List 2 Accent 5" w:uiPriority="66"/>
    <w:lsdException w:name="Medium Grid 1 Accent 5" w:uiPriority="67"/>
    <w:lsdException w:name="Medium Grid 2 Accent 5" w:uiPriority="68"/>
    <w:lsdException w:name="Medium Grid 3 Accent 5" w:uiPriority="69"/>
    <w:lsdException w:name="Dark List Accent 5" w:uiPriority="70"/>
    <w:lsdException w:name="Colorful Shading Accent 5" w:uiPriority="71"/>
    <w:lsdException w:name="Colorful List Accent 5" w:uiPriority="72"/>
    <w:lsdException w:name="Colorful Grid Accent 5" w:uiPriority="73"/>
    <w:lsdException w:name="Light Shading Accent 6" w:uiPriority="60"/>
    <w:lsdException w:name="Light List Accent 6" w:uiPriority="61"/>
    <w:lsdException w:name="Light Grid Accent 6" w:uiPriority="62"/>
    <w:lsdException w:name="Medium Shading 1 Accent 6" w:uiPriority="63"/>
    <w:lsdException w:name="Medium Shading 2 Accent 6" w:uiPriority="64"/>
    <w:lsdException w:name="Medium List 1 Accent 6" w:uiPriority="65"/>
    <w:lsdException w:name="Medium List 2 Accent 6" w:uiPriority="66"/>
    <w:lsdException w:name="Medium Grid 1 Accent 6" w:uiPriority="67"/>
    <w:lsdException w:name="Medium Grid 2 Accent 6" w:uiPriority="68"/>
    <w:lsdException w:name="Medium Grid 3 Accent 6" w:uiPriority="69"/>
    <w:lsdException w:name="Dark List Accent 6" w:uiPriority="70"/>
    <w:lsdException w:name="Colorful Shading Accent 6" w:uiPriority="71"/>
    <w:lsdException w:name="Colorful List Accent 6" w:uiPriority="72"/>
    <w:lsdException w:name="Colorful Grid Accent 6" w:uiPriority="73"/>
    <w:lsdException w:name="Subtle Emphasis" w:uiPriority="19" w:qFormat="1"/>
    <w:lsdException w:name="Intense Emphasis" w:uiPriority="21" w:qFormat="1"/>
    <w:lsdException w:name="Subtle Reference" w:uiPriority="31" w:qFormat="1"/>
    <w:lsdException w:name="Intense Reference" w:uiPriority="32" w:qFormat="1"/>
    <w:lsdException w:name="Book Title" w:uiPriority="33" w:qFormat="1"/>
    <w:lsdException w:name="Bibliography" w:uiPriority="37" w:semiHidden="1" w:unhideWhenUsed="1"/>
    <w:lsdException w:name="TOC Heading" w:uiPriority="39" w:semiHidden="1" w:unhideWhenUsed="1" w:qFormat="1"/>
    <w:lsdException w:name="Plain Table 1" w:uiPriority="41"/>
    <w:lsdException w:name="Plain Table 2" w:uiPriority="42"/>
    <w:lsdException w:name="Plain Table 3" w:uiPriority="43"/>
    <w:lsdException w:name="Plain Table 4" w:uiPriority="44"/>
    <w:lsdException w:name="Plain Table 5" w:uiPriority="45"/>
    <w:lsdException w:name="Grid Table Light" w:uiPriority="40"/>
    <w:lsdException w:name="Grid Table 1 Light" w:uiPriority="46"/>
    <w:lsdException w:name="Grid Table 2" w:uiPriority="47"/>
    <w:lsdException w:name="Grid Table 3" w:uiPriority="48"/>
    <w:lsdException w:name="Grid Table 4" w:uiPriority="49"/>
    <w:lsdException w:name="Grid Table 5 Dark" w:uiPriority="50"/>
    <w:lsdException w:name="Grid Table 6 Colorful" w:uiPriority="51"/>
    <w:lsdException w:name="Grid Table 7 Colorful" w:uiPriority="52"/>
    <w:lsdException w:name="Grid Table 1 Light Accent 1" w:uiPriority="46"/>
    <w:lsdException w:name="Grid Table 2 Accent 1" w:uiPriority="47"/>
    <w:lsdException w:name="Grid Table 3 Accent 1" w:uiPriority="48"/>
    <w:lsdException w:name="Grid Table 4 Accent 1" w:uiPriority="49"/>
    <w:lsdException w:name="Grid Table 5 Dark Accent 1" w:uiPriority="50"/>
    <w:lsdException w:name="Grid Table 6 Colorful Accent 1" w:uiPriority="51"/>
    <w:lsdException w:name="Grid Table 7 Colorful Accent 1" w:uiPriority="52"/>
    <w:lsdException w:name="Grid Table 1 Light Accent 2" w:uiPriority="46"/>
    <w:lsdException w:name="Grid Table 2 Accent 2" w:uiPriority="47"/>
    <w:lsdException w:name="Grid Table 3 Accent 2" w:uiPriority="48"/>
    <w:lsdException w:name="Grid Table 4 Accent 2" w:uiPriority="49"/>
    <w:lsdException w:name="Grid Table 5 Dark Accent 2" w:uiPriority="50"/>
    <w:lsdException w:name="Grid Table 6 Colorful Accent 2" w:uiPriority="51"/>
    <w:lsdException w:name="Grid Table 7 Colorful Accent 2" w:uiPriority="52"/>
    <w:lsdException w:name="Grid Table 1 Light Accent 3" w:uiPriority="46"/>
    <w:lsdException w:name="Grid Table 2 Accent 3" w:uiPriority="47"/>
    <w:lsdException w:name="Grid Table 3 Accent 3" w:uiPriority="48"/>
    <w:lsdException w:name="Grid Table 4 Accent 3" w:uiPriority="49"/>
    <w:lsdException w:name="Grid Table 5 Dark Accent 3" w:uiPriority="50"/>
    <w:lsdException w:name="Grid Table 6 Colorful Accent 3" w:uiPriority="51"/>
    <w:lsdException w:name="Grid Table 7 Colorful Accent 3" w:uiPriority="52"/>
    <w:lsdException w:name="Grid Table 1 Light Accent 4" w:uiPriority="46"/>
    <w:lsdException w:name="Grid Table 2 Accent 4" w:uiPriority="47"/>
    <w:lsdException w:name="Grid Table 3 Accent 4" w:uiPriority="48"/>
    <w:lsdException w:name="Grid Table 4 Accent 4" w:uiPriority="49"/>
    <w:lsdException w:name="Grid Table 5 Dark Accent 4" w:uiPriority="50"/>
    <w:lsdException w:name="Grid Table 6 Colorful Accent 4" w:uiPriority="51"/>
    <w:lsdException w:name="Grid Table 7 Colorful Accent 4" w:uiPriority="52"/>
    <w:lsdException w:name="Grid Table 1 Light Accent 5" w:uiPriority="46"/>
    <w:lsdException w:name="Grid Table 2 Accent 5" w:uiPriority="47"/>
    <w:lsdException w:name="Grid Table 3 Accent 5" w:uiPriority="48"/>
    <w:lsdException w:name="Grid Table 4 Accent 5" w:uiPriority="49"/>
    <w:lsdException w:name="Grid Table 5 Dark Accent 5" w:uiPriority="50"/>
    <w:lsdException w:name="Grid Table 6 Colorful Accent 5" w:uiPriority="51"/>
    <w:lsdException w:name="Grid Table 7 Colorful Accent 5" w:uiPriority="52"/>
    <w:lsdException w:name="Grid Table 1 Light Accent 6" w:uiPriority="46"/>
    <w:lsdException w:name="Grid Table 2 Accent 6" w:uiPriority="47"/>
    <w:lsdException w:name="Grid Table 3 Accent 6" w:uiPriority="48"/>
    <w:lsdException w:name="Grid Table 4 Accent 6" w:uiPriority="49"/>
    <w:lsdException w:name="Grid Table 5 Dark Accent 6" w:uiPriority="50"/>
    <w:lsdException w:name="Grid Table 6 Colorful Accent 6" w:uiPriority="51"/>
    <w:lsdException w:name="Grid Table 7 Colorful Accent 6" w:uiPriority="52"/>
    <w:lsdException w:name="List Table 1 Light" w:uiPriority="46"/>
    <w:lsdException w:name="List Table 2" w:uiPriority="47"/>
    <w:lsdException w:name="List Table 3" w:uiPriority="48"/>
    <w:lsdException w:name="List Table 4" w:uiPriority="49"/>
    <w:lsdException w:name="List Table 5 Dark" w:uiPriority="50"/>
    <w:lsdException w:name="List Table 6 Colorful" w:uiPriority="51"/>
    <w:lsdException w:name="List Table 7 Colorful" w:uiPriority="52"/>
    <w:lsdException w:name="List Table 1 Light Accent 1" w:uiPriority="46"/>
    <w:lsdException w:name="List Table 2 Accent 1" w:uiPriority="47"/>
    <w:lsdException w:name="List Table 3 Accent 1" w:uiPriority="48"/>
    <w:lsdException w:name="List Table 4 Accent 1" w:uiPriority="49"/>
    <w:lsdException w:name="List Table 5 Dark Accent 1" w:uiPriority="50"/>
    <w:lsdException w:name="List Table 6 Colorful Accent 1" w:uiPriority="51"/>
    <w:lsdException w:name="List Table 7 Colorful Accent 1" w:uiPriority="52"/>
    <w:lsdException w:name="List Table 1 Light Accent 2" w:uiPriority="46"/>
    <w:lsdException w:name="List Table 2 Accent 2" w:uiPriority="47"/>
    <w:lsdException w:name="List Table 3 Accent 2" w:uiPriority="48"/>
    <w:lsdException w:name="List Table 4 Accent 2" w:uiPriority="49"/>
    <w:lsdException w:name="List Table 5 Dark Accent 2" w:uiPriority="50"/>
    <w:lsdException w:name="List Table 6 Colorful Accent 2" w:uiPriority="51"/>
    <w:lsdException w:name="List Table 7 Colorful Accent 2" w:uiPriority="52"/>
    <w:lsdException w:name="List Table 1 Light Accent 3" w:uiPriority="46"/>
    <w:lsdException w:name="List Table 2 Accent 3" w:uiPriority="47"/>
    <w:lsdException w:name="List Table 3 Accent 3" w:uiPriority="48"/>
    <w:lsdException w:name="List Table 4 Accent 3" w:uiPriority="49"/>
    <w:lsdException w:name="List Table 5 Dark Accent 3" w:uiPriority="50"/>
    <w:lsdException w:name="List Table 6 Colorful Accent 3" w:uiPriority="51"/>
    <w:lsdException w:name="List Table 7 Colorful Accent 3" w:uiPriority="52"/>
    <w:lsdException w:name="List Table 1 Light Accent 4" w:uiPriority="46"/>
    <w:lsdException w:name="List Table 2 Accent 4" w:uiPriority="47"/>
    <w:lsdException w:name="List Table 3 Accent 4" w:uiPriority="48"/>
    <w:lsdException w:name="List Table 4 Accent 4" w:uiPriority="49"/>
    <w:lsdException w:name="List Table 5 Dark Accent 4" w:uiPriority="50"/>
    <w:lsdException w:name="List Table 6 Colorful Accent 4" w:uiPriority="51"/>
    <w:lsdException w:name="List Table 7 Colorful Accent 4" w:uiPriority="52"/>
    <w:lsdException w:name="List Table 1 Light Accent 5" w:uiPriority="46"/>
    <w:lsdException w:name="List Table 2 Accent 5" w:uiPriority="47"/>
    <w:lsdException w:name="List Table 3 Accent 5" w:uiPriority="48"/>
    <w:lsdException w:name="List Table 4 Accent 5" w:uiPriority="49"/>
    <w:lsdException w:name="List Table 5 Dark Accent 5" w:uiPriority="50"/>
    <w:lsdException w:name="List Table 6 Colorful Accent 5" w:uiPriority="51"/>
    <w:lsdException w:name="List Table 7 Colorful Accent 5" w:uiPriority="52"/>
    <w:lsdException w:name="List Table 1 Light Accent 6" w:uiPriority="46"/>
    <w:lsdException w:name="List Table 2 Accent 6" w:uiPriority="47"/>
    <w:lsdException w:name="List Table 3 Accent 6" w:uiPriority="48"/>
    <w:lsdException w:name="List Table 4 Accent 6" w:uiPriority="49"/>
    <w:lsdException w:name="List Table 5 Dark Accent 6" w:uiPriority="50"/>
    <w:lsdException w:name="List Table 6 Colorful Accent 6" w:uiPriority="51"/>
    <w:lsdException w:name="List Table 7 Colorful Accent 6" w:uiPriority="52"/>
  </w:latentStyles>
  <w:style w:type="paragraph" w:styleId="Normal" w:default="1">
    <w:name w:val="Normal"/>
    <w:qFormat/>
    <w:pPr>
      <w:widowControl/>
      <w:bidi w:val="0"/>
      <w:spacing w:lineRule="auto" w:line="259" w:before="0" w:after="160"/>
      <w:jc w:val="left"/>
    </w:pPr>
    <w:rPr>
      <w:rFonts w:ascii="Calibri" w:hAnsi="Calibri" w:eastAsia="Calibri" w:cs="" w:asciiTheme="minorHAnsi" w:cstheme="minorBidi" w:eastAsiaTheme="minorHAnsi" w:hAnsiTheme="minorHAnsi"/>
      <w:color w:val="auto"/>
      <w:kern w:val="0"/>
      <w:sz w:val="22"/>
      <w:szCs w:val="22"/>
      <w:lang w:val="pt-BR" w:eastAsia="en-US" w:bidi="ar-SA"/>
    </w:rPr>
  </w:style>
  <w:style w:type="character" w:styleId="DefaultParagraphFont" w:default="1">
    <w:name w:val="Default Paragraph Font"/>
    <w:uiPriority w:val="1"/>
    <w:semiHidden/>
    <w:unhideWhenUsed/>
    <w:qFormat/>
    <w:rPr/>
  </w:style>
  
  <w:style w:type="character" w:styleId="PreambuloparChar" w:customStyle="1">
    <w:name w:val="preambulo_par Char"/>
    <w:basedOn w:val="DefaultParagraphFont"/>
    <w:link w:val="preambulopar"/>
    <w:qFormat/>
    
    <w:rPr/>
  </w:style>
  
  <w:style w:type="character" w:styleId="LivrorotuloChar" w:customStyle="1">
    <w:name w:val="livro_rotulo Char"/>
    <w:basedOn w:val="DefaultParagraphFont"/>
    <w:link w:val="livrorotulo"/>
    <w:qFormat/>
    
    <w:rPr/>
  </w:style>
  
  <w:style w:type="character" w:styleId="Artigorotulo" w:customStyle="1">
    <w:name w:val="artigo_rotulo"/>
    <w:basedOn w:val="DefaultParagraphFont"/>
    <w:uiPriority w:val="1"/>
    <w:qFormat/>
    
    <w:rPr/>
  </w:style>
 
  <w:style w:type="character" w:styleId="LivrorotuloaltChar" w:customStyle="1">
    <w:name w:val="livro_rotulo_alt Char"/>
    <w:basedOn w:val="LivrorotuloChar"/>
    <w:link w:val="livrorotuloalt"/>
    <w:qFormat/>
    
    <w:rPr/>
  </w:style>
  
  <w:style w:type="character" w:styleId="Artigorotuloalt" w:customStyle="1">
    <w:name w:val="artigo_rotulo_alt"/>
    <w:basedOn w:val="Artigorotulo"/>
    <w:uiPriority w:val="1"/>
    <w:qFormat/>
    
    <w:rPr/>
  </w:style>
  
  <w:style w:type="character" w:styleId="LivrotituloChar" w:customStyle="1">
    <w:name w:val="livro_titulo Char"/>
    <w:basedOn w:val="DefaultParagraphFont"/>
    <w:link w:val="livrotitulo"/>
    <w:qFormat/>
    
    <w:rPr/>
  </w:style>
  
  <w:style w:type="paragraph" w:styleId="Heading">
    <w:name w:val="Heading"/>
    <w:basedOn w:val="Normal"/>
    <w:next w:val="TextBody"/>
    <w:qFormat/>
    <w:pPr>
      <w:keepNext w:val="true"/>
      <w:spacing w:before="240" w:after="120"/>
    </w:pPr>
    <w:rPr>
      <w:rFonts w:ascii="Liberation Sans" w:hAnsi="Liberation Sans" w:eastAsia="Liberation Sans" w:cs="Liberation Sans"/>
      <w:sz w:val="28"/>
      <w:szCs w:val="28"/>
    </w:rPr>
  </w:style>
  <w:style w:type="paragraph" w:styleId="TextBody">
    <w:name w:val="Body Text"/>
    <w:basedOn w:val="Normal"/>
    <w:pPr>
      <w:spacing w:lineRule="auto" w:line="276" w:before="0" w:after="140"/>
    </w:pPr>
    <w:rPr/>
  </w:style>
  <w:style w:type="paragraph" w:styleId="List">
    <w:name w:val="List"/>
    <w:basedOn w:val="TextBody"/>
    <w:pPr/>
    <w:rPr/>
  </w:style>
  <w:style w:type="paragraph" w:styleId="Caption">
    <w:name w:val="Caption"/>
    <w:basedOn w:val="Normal"/>
    <w:qFormat/>
    <w:pPr>
      <w:suppressLineNumbers/>
      <w:spacing w:before="120" w:after="120"/>
    </w:pPr>
    <w:rPr>
      <w:i/>
      <w:iCs/>
      <w:sz w:val="24"/>
      <w:szCs w:val="24"/>
    </w:rPr>
  </w:style>
  <w:style w:type="paragraph" w:styleId="Index">
    <w:name w:val="Index"/>
    <w:basedOn w:val="Normal"/>
    <w:qFormat/>
    <w:pPr>
      <w:suppressLineNumbers/>
    </w:pPr>
    <w:rPr/>
  </w:style>
  <w:style w:type="paragraph" w:styleId="Preambulopar" w:customStyle="1">
    <w:name w:val="preambulo_par"/>
    <w:basedOn w:val="Normal"/>
    <w:link w:val="preambuloparChar"/>
    <w:qFormat/>
    
    <w:pPr/>
    <w:rPr/>
  </w:style>
  <w:style w:type="paragraph" w:styleId="Livrorotulo" w:customStyle="1">
    <w:name w:val="livro_rotulo"/>
    <w:basedOn w:val="Normal"/>
    <w:next w:val="Normal"/>
    <w:link w:val="livrorotuloChar"/>
    <w:qFormat/>
    
    <w:pPr/>
    <w:rPr/>
  </w:style>
  <w:style w:type="paragraph" w:styleId="Livrorotuloalt" w:customStyle="1">
    <w:name w:val="livro_rotulo_alt"/>
    <w:basedOn w:val="Livrorotulo"/>
    <w:next w:val="Normal"/>
    <w:link w:val="livrorotuloaltChar"/>
    <w:qFormat/>
    
    <w:pPr/>
    <w:rPr/>
  </w:style>
  <w:style w:type="paragraph" w:styleId="Livrotitulo" w:customStyle="1">
    <w:name w:val="livro_titulo"/>
    <w:basedOn w:val="Normal"/>
    <w:link w:val="livrotituloChar"/>
    <w:qFormat/>
    
    <w:pPr/>
    <w:rPr/>
  </w:style>
  <w:style w:type="numbering" w:styleId="NoList" w:default="1">
    <w:name w:val="No List"/>
    <w:uiPriority w:val="99"/>
    <w:semiHidden/>
    <w:unhideWhenUsed/>
    <w:qFormat/>
  </w:style>
  <w:style w:type="table" w:default="1" w:styleId="Tabelanormal">
    <w:name w:val="Normal Table"/>
    <w:uiPriority w:val="99"/>
    <w:semiHidden/>
    <w:unhideWhenUsed/>
    <w:tblPr>
      <w:tblInd w:w="0" w:type="dxa"/>
      <w:tblCellMar>
        <w:top w:w="0" w:type="dxa"/>
        <w:left w:w="108" w:type="dxa"/>
        <w:bottom w:w="0" w:type="dxa"/>
        <w:right w:w="108" w:type="dxa"/>
      </w:tblCellMar>
    </w:tblPr>
  </w:style>
</w:styles>       

  )

  lazy val fixedFiles : Map[String,Elem] = Map(
    "/[Content_Types].xml" -> 
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
  <Default Extension="xml" ContentType="application/xml"/>
  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
  <Default Extension="png" ContentType="image/png"/>
  <Default Extension="jpeg" ContentType="image/jpeg"/>
  <Override PartName="/_rels/.rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
  <Override PartName="/word/_rels/document.xml.rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
  <Override PartName="/word/settings.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml"/>
  <Override PartName="/word/fontTable.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml"/>
  <Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>
  <Override PartName="/word/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/>
</Types>
        ,
    "_rels/.rels" -> 
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="doc" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/>
</Relationships>            
    ,
    "word/_rels/document.xml.rels" -> 
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>
  <Relationship Id="rId2" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/fontTable" Target="fontTable.xml"/>
  <Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings" Target="settings.xml"/>
</Relationships>        
    ,
    "word/fontTable.xml" -> 
<w:fonts xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">
  <w:font w:name="Times New Roman">
    <w:charset w:val="00"/>
    <w:family w:val="roman"/>
    <w:pitch w:val="variable"/>
  </w:font>
  <w:font w:name="Symbol">
    <w:charset w:val="02"/>
    <w:family w:val="roman"/>
    <w:pitch w:val="variable"/>
  </w:font>
  <w:font w:name="Arial">
    <w:charset w:val="00"/>
    <w:family w:val="swiss"/>
    <w:pitch w:val="variable"/>
  </w:font>
  <w:font w:name="Liberation Serif">
    <w:altName w:val="Times New Roman"/>
    <w:charset w:val="01"/>
    <w:family w:val="roman"/>
    <w:pitch w:val="variable"/>
  </w:font>
  <w:font w:name="Calibri">
    <w:charset w:val="01"/>
    <w:family w:val="roman"/>
    <w:pitch w:val="variable"/>
  </w:font>
  <w:font w:name="Liberation Sans">
    <w:altName w:val="Arial"/>
    <w:charset w:val="01"/>
    <w:family w:val="roman"/>
    <w:pitch w:val="variable"/>
  </w:font>
</w:fonts>            
    ,
    "word/settings.xml" -> 
<w:settings xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">
  <w:compat>
    <w:compatSetting w:name="compatibilityMode" w:uri="http://schemas.microsoft.com/office/word" w:val="15"/>
    <w:compatSetting w:name="overrideTableStyleFontSizeAndJustification" w:uri="http://schemas.microsoft.com/office/word" w:val="1"/>
    <w:compatSetting w:name="enableOpenTypeFeatures" w:uri="http://schemas.microsoft.com/office/word" w:val="1"/>
    <w:compatSetting w:name="doNotFlipMirrorIndents" w:uri="http://schemas.microsoft.com/office/word" w:val="1"/>
    <w:compatSetting w:name="differentiateMultirowTableHeaders" w:uri="http://schemas.microsoft.com/office/word" w:val="1"/>
  </w:compat>
  <w:themeFontLang w:val="pt-BR" w:eastAsia="" w:bidi=""/>
</w:settings>,
   "word/styles.xml" -> DocxFile.styleElem 
    
  )
  
  def makeMainDocument(elems : Seq[Elem]) = (
<w:document xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" mc:Ignorable="w14 wp14">
	<w:body>
		{NodeSeq.fromSeq(elems)}
		<w:sectPr>
      <w:type w:val="nextPage"/>
      <w:pgSz w:w="11906" w:h="16838"/>
      <w:pgMar w:left="1701" w:right="1701" w:header="0" w:top="1417" w:footer="0" w:bottom="1417" w:gutter="0"/>
      <w:pgNumType w:fmt="decimal"/>
      <w:formProt w:val="false"/>
      <w:textDirection w:val="lrTb"/>
      <w:docGrid w:type="default" w:linePitch="360" w:charSpace="4096"/>
    </w:sectPr>
  </w:body>
</w:document>
      )
  def makePackageFile(elems : Seq[Elem]) = {    
    import java.io._
    import java.util.zip._
    
    val mainDoc = makeMainDocument(elems)
    val files = fixedFiles + ("/word/document.xml" -> mainDoc)
    
    val bo = new ByteArrayOutputStream()
    val zo = new ZipOutputStream(bo)
    for { 
      (path,root) <- files      
    } {
      val data = XmlUtils.xmlToByteArray(root)
      val ze = new ZipEntry(path)
      zo.putNextEntry(ze)
      zo.write(data)
      zo.closeEntry()
    }
    zo.close()    
    try { bo.close() } catch { case _ : Exception => }
    bo.toByteArray()    
  }                                
}

trait XmlComponent {
  def asXML : Elem
  implicit class RichOption[T](o : Option[T]) {
    def elem(f : T => Elem) : NodeSeq = {
      o.map(f).getOrElse(NodeSeq.Empty)
    }
  }  
  def cond(c : Boolean)(e : => NodeSeq) : NodeSeq = {
    if(c) { e } else { NodeSeq.Empty }    
  }
  implicit class RichSeq[T](o : Seq[T]) {
    def eachElem(f : T => Elem) : NodeSeq =
        NodeSeq.fromSeq(o.map(f))
  }
}

final case class DocxMainDocument(contents : Seq[DocxTextComponent] = Seq()) 
    extends XmlComponent {
  override def asXML : Elem = (
<w:document xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape" xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing" xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml" mc:Ignorable="w14 wp14">
	<w:body>
		{NodeSeq.fromSeq(contents.map(_.asXML))}
		<w:sectPr>
      <w:type w:val="nextPage"/>
      <w:pgSz w:w="11906" w:h="16838"/>
      <w:pgMar w:left="1701" w:right="1701" w:header="0" w:top="1417" w:footer="0" w:bottom="1417" w:gutter="0"/>
      <w:pgNumType w:fmt="decimal"/>
      <w:formProt w:val="false"/>
      <w:textDirection w:val="lrTb"/>
      <w:docGrid w:type="default" w:linePitch="360" w:charSpace="4096"/>
    </w:sectPr>
  </w:body>
</w:document>
   )  
}

sealed trait DocxTextComponent extends Product with XmlComponent {
  def isEmpty : Boolean
}

final case class P(runs : Seq[ParElement] = Seq(),pPr : Option[PPr] = None) extends DocxTextComponent {
  lazy val asXML = (
<w:p>
	{pPr.elem(_.asXML)}
	{runs.eachElem(_.asXML)}
</w:p>
  )      
  def insertFirst(els : ParElement*) =
    copy(runs = els ++ runs)
  def insertLast(els : ParElement*) =
    copy(runs = runs ++ els)
  def isEmpty = runs.forall(_.isEmpty)
}

sealed trait ParElement extends Product with XmlComponent {
  def isEmpty : Boolean = false
}

abstract sealed class CapsMode extends Product {
  def asXML : NodeSeq 
}

case object CM_Caps extends CapsMode {
  def asXML = (<w:caps w:val="true"/>)
}
case object CM_SmallCaps extends CapsMode {
  def asXML = (<w:smallCaps w:val="true"/>)
}
case object CM_Normal extends CapsMode {
  def asXML = NodeSeq.fromSeq(Seq(      
      <w:caps w:val="false"/>,
      <w:smallCaps w:val="false"/>
      ))
}

final case class RGB(red : Double, green : Double, blue : Double) {
  lazy val value = f"${r}%02X${g}%02X${b}%02X"
  private val r = norm(red)
  private val g = norm(green)
  private val b = norm(blue)
  private def norm(v : Double) = math.floor(255.0*v).toInt
}

final case class Fonts(
    ascii : Option[String] = None,
    cs : Option[String] = None,
    hAnsi : Option[String] = None) extends XmlComponent {
  lazy val asXML = (
<w:rFonts w:ascii={ascii.getOrElse(null)} w:cs={cs.getOrElse(null)}
       w:hAnsi={ascii.getOrElse(null)}/>
      )
}

abstract sealed class UnderlineOption extends Product with XmlComponent 

case object UO_NoUnderline extends UnderlineOption {
  val asXML = <w:u w:val="false"/>
}

abstract sealed class UnderlineStyle(val value : String) extends Product

case object US_Dash extends UnderlineStyle("dash")
case object US_DashDotDotHeavy extends UnderlineStyle("dashDotDotHeavy")
case object US_DashDotHeavy extends UnderlineStyle("dashDotHeavy")
case object US_DashedHeavy extends UnderlineStyle("dashedHeavy")
case object US_DashLong extends UnderlineStyle("dashLong")
case object US_DashLongHeavy extends UnderlineStyle("dashLongHeavy")
case object US_DotDash extends UnderlineStyle("dotDash")
case object US_DotDotDash extends UnderlineStyle("dotDotDash")
case object US_Dotted extends UnderlineStyle("dotted")
case object US_DottedHeavy extends UnderlineStyle("dottedHeavy")
case object US_Double extends UnderlineStyle("double")
case object US_None extends UnderlineStyle("none")
case object US_Single extends UnderlineStyle("single")
case object US_Thick extends UnderlineStyle("thick")
case object US_Wave extends UnderlineStyle("wave")
case object US_WavyDouble extends UnderlineStyle("wavyDouble")
case object US_WavyHeavy extends UnderlineStyle("wavyHeavy")
case object US_Words extends UnderlineStyle("words")



final case class UO_Underline(color : Option[RGB] = None) extends UnderlineOption {
  val asXML = (<w:u w:val="true" w:color={color.map(_.value).getOrElse(null)}/>)
}

abstract sealed class VertAlignment(val value : String) extends Product 

case object VA_Superscript extends VertAlignment("superscript")
case object VA_Subscript extends VertAlignment("subscript")
case object VA_Baseline extends VertAlignment("baseline")

final case class RPr(
    bold : Option[Boolean] = None,
    italics : Option[Boolean] = None,
    capsMode : Option[CapsMode] = None,
    color : Option[RGB] = None,
    lang : Option[String] = None,
    fonts : Option[Fonts] = None,
    rStyle : Option[String] = None,
    strike : Option[Boolean] = None,
    sz : Option[Int] = None,
    szCs : Option[Int] = None,
    underline : Option[UnderlineOption] = None,
    vertAlign : Option[VertAlignment] = None
    ) extends XmlComponent {
  lazy val asXML = (
<w:rPr>
{bold.elem(x => <w:b w:val={x.toString}/>)}
{italics.elem(x => <w:i w:val={x.toString}/>)}
{capsMode.map(_.asXML).getOrElse(NodeSeq.Empty)}
{color.elem(v => <w:color w:val={v.value}/>)}
{lang.elem(x => <w:lang w:val={x.toString}/>)}
{fonts.elem(_.asXML)}
{rStyle.elem(x => <w:rStyle w:val={x}/>)}
{rStyle.elem(x => <w:rStyle w:val={x}/>)}
{strike.elem(x => <w:strike w:val={x.toString}/>)}
{sz.elem(x => <w:sz w:val={x.toString}/>)}
{szCs.elem(x => <w:szCs w:val={x.toString}/>)}
{underline.elem(_.asXML)}
</w:rPr>      
      )
  def +(x : RPr) = RPr(
      bold = x.bold orElse bold,
      italics = x.italics orElse italics,
      capsMode = x.capsMode orElse capsMode,
      color = x.color orElse color,
      lang = x.lang orElse lang,
      fonts = x.fonts orElse fonts,  
      rStyle = x.rStyle orElse rStyle,
      strike = x.strike orElse strike,
      sz = x.sz orElse sz,
      szCs = x.szCs orElse szCs,
      underline = x.underline orElse underline,
      vertAlign = x.vertAlign orElse vertAlign)
    
}

final case class R(
    rPr : Option[RPr] = None,
    contents : Seq[RunContent] = Seq()) extends ParElement {
  lazy val asXML = (
<w:r>
{contents.map(_.asXML)}
</w:r>      
      )
  def insertFirst(els : RunContent*) = copy(contents = els ++ contents)
  def insertLast(els : RunContent*) = copy(contents = contents ++ els)
  override def isEmpty = contents.forall(_.isEmpty)
}

abstract sealed class RunContent extends Product with XmlComponent {
  def isEmpty : Boolean = false
}

case object Br extends RunContent {
  val asXML = <w:br/>
}

final case class DelText(text : String, preserveSpace : Boolean = false) extends RunContent {
  lazy val asXML = (
      <w:delText xml:space={if(preserveSpace) { "preserve" } else {  null }}>{text}</w:delText>
      )
}

final case class Del(id : String, content : Seq[ParElement] = Seq(), author : Option[String] = None, date : Option[java.time.ZonedDateTime] = None
    ) extends RunContent {
  lazy val asXML = (
      <w:del w:id={id} w:author={author.getOrElse(null)} date={date.map(_.toString).getOrElse(null)}>
		{NodeSeq.fromSeq(content.map(_.asXML))}
		</w:del>
      )
}

final case class Ins(id : String, content : Seq[ParElement] = Seq(), author : Option[String] = None, date : Option[java.time.ZonedDateTime] = None
    ) extends RunContent {
  lazy val asXML = (
      <w:ins w:id={id} w:author={author.getOrElse(null)} date={date.map(_.toString).getOrElse(null)}>
		{NodeSeq.fromSeq(content.map(_.asXML))}
		</w:ins>
      )
}

/*
<w:hyperlink r:id="rId9">
  <w:r>
    <w:t>http://www.example.com</w:t>
   </w:r>
</w:hyperlink>

<Relationships xmlns="...">
    <Relationship Id="rId9" Mode="External" Target=http://www.example.com />
</Relationships>

 */

final case class Hyperlink(content : Seq[ParElement] = Seq(), anchor : Option[String] = None, id : Option[String] = None, tooltip : Option[String] = None) extends RunContent {
  lazy val asXML = (
   <w:hyperlink r:id={id.getOrElse(null)} w:anchor={anchor.getOrElse(null)} w:tooltip={tooltip.getOrElse(null)}>
	 {NodeSeq.fromSeq(content.map(_.asXML))}
	</w:hyperlink>
      )
}




case object NoBreakHyphen extends RunContent {
  val asXML = <w:noBreakHyphen/>
}

case class Sym(font : String, char : String) extends RunContent {
  val asXML = <w:sym w:font={font} w:char={char}/>
}

case class T(text : String, preserveSpace : Boolean = false) extends RunContent {
  lazy val asXML = <w:t xml:space={if(preserveSpace) { "preserve" } else {  null }}>{text}</w:t>
  override val isEmpty = text.trim.size == 0
}



abstract sealed class PTabAlignment(val value : String) extends Product

case object PTA_Left extends PTabAlignment("left")
case object PTA_Center extends PTabAlignment("center")
case object PTA_Right extends PTabAlignment("right")

abstract sealed class PTabBase(val value : String) extends Product

case object PTB_Indent extends PTabBase("indent")
case object PTB_Margin extends PTabBase("margin")

case class PTab(
    alignment : PTabAlignment,
    leader : Option[TabLeader] = None,
    relativeTo : PTabBase) extends RunContent {
  val asXML = <w:ptab/>
}

/**
 * 
 * <Relationships ... >
...
<Relationship Id="rId8" TargetMode="Internal"
Type="http://purl.oclc.org/ooxml/officeDocument/relationships/customXml"
Target="math1.xml" />
...
</Relationships>
 * 
 */
final case class ContentPart(id : String) extends RunContent {
  val asXML = <w:contentPart r:id={id}/>
}


final case class Ind(
      start : Int = 0, end : Int = 0, hanging : Int = 0,
      firstLine : Int = 0//twentieths of a point
      ) extends XmlComponent {
  lazy val asXML = (
<w:ind w:start={start.toString} w:end={end.toString} w:hanging={hanging.toString} 
			w:firstLine={firstLine.toString}/>
    )      
}

abstract sealed class ST_Jc(val v : String) extends Product 

case object JC_Both extends ST_Jc("both")
case object JC_Center extends ST_Jc("center")
case object JC_Distribute extends ST_Jc("distribute")
case object JC_End extends ST_Jc("end")
case object JC_NumTab extends ST_Jc("numTab")
case object JC_Start extends ST_Jc("start")

abstract sealed class TabLeader(val value : String) extends Product

case object TL_Dot extends TabLeader("dot")
case object TL_Heavy extends TabLeader("heavy")
case object TL_Hyphen extends TabLeader("hyphen")
case object TL_MiddleDot extends TabLeader("middleDot")
case object TL_None extends TabLeader("none")
case object TL_Underscore extends TabLeader("underscore")

abstract sealed class TabStopType(val value : String) extends Product

/**
 * Specifies that the current tab stop shall result in a
location in the document where all following text is
aligned to its trailing edge (i.e. all text runs following
this tab stop and preceding the next tab stop shall be
aligned against the trailing edge with respect to the
tab stop location). [Example: In an RTL paragraph, the
trailing edge is the left edge, so text aligns to that
edge, extending to the right. end example]
 */
case object TST_End extends TabStopType("end")

/*
 * Specifies that the current tab stop shall result in a
location in the document where all following text is
aligned to its leading edge (i.e. all text runs following
this tab stop and preceding the next tab stop shall be
aligned against the leading edge with respect to the
tab stop location).
 */
case object TST_Start extends TabStopType("start")

final case class Tab(
    pos : Int,
    tabType : TabStopType,
    leader : TabLeader = TL_None
    ) extends RunContent {
  lazy val asXML = (
<w:tab w:leader={leader.value} w:pos={pos.toString} w:val={tabType.value}/>
      )
}

final case class PPr(
    divId : Option[String] = None,
    ind : Option[Ind] = None,
    jc : Option[ST_Jc] = None,
    style : Option[String] = None, //styleId
    tabs : Seq[Tab] = Seq()
    ) extends XmlComponent {
  lazy val asXML = ( 
<w:pPr>
	{divId.elem(n => <w:divId w:val={n}/>)}
	{ind.elem(_.asXML)}
	{jc.elem(x => <w:jc w:val={x.v}/>)}
	{style.elem(x => <w:pStyle w:val={x}/>)}
	{cond(!tabs.isEmpty)(<w:tabs>{tabs.eachElem(_.asXML)}</w:tabs>)}	
</w:pPr>
	)
}


/* final case class RelationshipProp(typ : String, inDoc : Boolean = false)

sealed trait DocxPart extends Product {
  def data : Array[Byte] 
  val name : String
  val mimeType : String
  val relations : Map[String, String] 
}

object DocxPart {
  def apply(name : String, mimeType : String, data : Array[Byte],rels : Map[String,String]) = {
    if(XmlDocxPart.appliesTo(mimeType)) {
      XmlDocxPart(name,mimeType,data,rels)
    } else {
      GenByteArrayDocxPart(name,mimeType,data,rels)
    }
  }
}

final case class GenByteArrayDocxPart(
    name : String,
    mimeType : String,
    override val data : Array[Byte],
    relations : Map[String,String] = Map()) extends DocxPart 

sealed trait XmlDocxPart extends DocxPart {
  def xmlContent : Elem
  lazy val  data = XmlUtils.xmlToByteArray(xmlContent)        
}

object XmlDocxPart  {
  def mimeTypes = Map[String,(String,String,Elem,Map[String,String]) => XmlDocxPart](      
      "application/vnd.openxmlformats-officedocument.custom-properties+xml" -> GenXmlPart,
      "application/vnd.openxmlformats-package.core-properties+xml" -> GenXmlPart,
      "application/vnd.openxmlformats-officedocument.extended-properties+xml" -> GenXmlPart,
      "application/vnd.openxmlformats-officedocument.theme+xml" -> GenXmlPart,
      "application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml" -> GenXmlPart,
      "application/vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml" -> FontTablePart.fromXML,
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml" -> DocxDocumentPart.fromXML,
      "application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml" -> DocxStylesPart.fromXML      
      )
   def appliesTo(mimeType : String) = mimeTypes.contains(mimeType)
   def apply(name : String, mimeType : String, data : Array[Byte], relations : Map[String,String]) : XmlDocxPart = {
     mimeTypes.get(mimeType) match {
       case None => sys.error("XmlDocxPart: wrong parameter mime type: " + (name,mimeType))
       case Some(gen) => {
         val is = new ByteArrayInputStream(data)
         val e = XML.load(is)
         is.close()
         gen(name,mimeType,e,relations)
       }
     } 
   }
}



final case class DocxDocumentPart(name : String = "/word/document.xml",
    relations : Map[String,String] = Map()) extends XmlDocxPart {  
  val mimeType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"
  lazy val xmlContent = {
    <empty/>
  }
}

object DocxDocumentPart {
  def fromXML(name : String, ignoredMimeType : String,e : Elem, rels : Map[String,String]) = 
    DocxDocumentPart(name,rels)
}

final case class DocxStylesPart(name : String = "/word/styles.xml") extends XmlDocxPart {  
  val mimeType = "application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"
  lazy val xmlContent = {
    <empty/>
  }
  override val relations : Map[String,String] = Map()
}

object DocxStylesPart {
  def fromXML(name : String, ignoredMimeType : String,e : Elem, rels : Map[String,String]) = 
    DocxStylesPart(name)
}


final case class FontTablePart(name : String = "/word/fontTable.xml") extends XmlDocxPart {  
  val mimeType = "application/vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml"
  lazy val xmlContent = {
    <empty/>
  }
  override val relations : Map[String,String] = Map()
}

object FontTablePart {
  def fromXML(name : String, ignoredMimeType : String,e : Elem, rels : Map[String,String]) = 
    FontTablePart(name)
}

object Relations {
  val mimeType = "application/vnd.openxmlformats-package.relationships+xml"
  
  def fromXML(e : Elem) = 
    (e \ "Relationship").to[Seq].collect { 
      case x : Elem => ((e \ "@Type").text,
                 (e \ "@Target").text) }.toMap
  def toXML(m : Map[String,String]) = (
    <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
		  { NodeSeq.fromSeq(m.to[Seq].zipWithIndex.collect { 
		    case ((tp,ta),n) => <Relationship Id={"Id" + n} Type={tp} Target={ta}/>
		  } ) }
		</Relationships>
      )
}

final case class GenXmlPart(
    override val name : String,
    override val mimeType : String,
    override val xmlContent : Elem,
    override val relations : Map[String,String]) extends XmlDocxPart



final case class ContentTypes(
    defaults : Map[String,String] = ContentTypes.defaultDefaultTypes,
    overrides : Map[String,String] = Map()
    ) {
  lazy val asXML : Elem = (
      <Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
			{ NodeSeq.fromSeq(defaults.to[Seq].collect { case (k,v) => <Default Extension={k} ContentType={v}/> }) }
			{ NodeSeq.fromSeq(overrides.to[Seq].collect { case(k,v) => <Override PartName={k} ContentType={v}/> }) }
			</Types>
        )
  lazy val data = XmlUtils.xmlToByteArray(asXML)    
}

object ContentTypes {
  val defaultDefaultTypes = Map(
     "xml" -> "application/xml",
     "rels" -> "application/vnd.openxmlformats-package.relationships+xml",
     "png" -> "image/png",
     "jpeg" -> "image/jpeg"
      )
  val relationsMimeType = "application/vnd.openxmlformats-package.relationships+xml"
  
  def fromXML(e : Elem) = {
    ContentTypes(
        (e \ "Default").to[Seq].collect { 
          case x : Elem =>
            (x \ "@Extension").text -> (x \ "@ContentType").text  
        }.toMap,
        (e \ "Override").to[Seq].collect { 
          case x : Elem =>
            (x \ "@PartName").text -> (x \ "@ContentType").text  
        }.toMap
        )
  }
}

final case class DocxPackage(
    contentTypes : ContentTypes = ContentTypes(),
    parts : Map[String,DocxPart] = Map(),
    relationships : Map[String,Map[String,String]] = Map()) {
  
  def toByteArray = {
    import java.io._
    import java.util.zip._
    
    val bo = new ByteArrayOutputStream()
    val zo = new ZipOutputStream(bo)    
    def put(path : String, data : Array[Byte]) {
      val ze = new ZipEntry(path)
      zo.putNextEntry(ze)
      zo.write(data)
      zo.closeEntry()
    }
    put("/[Content_Types].xml",contentTypes.data)
    val root_rels = relationships.getOrElse("",Map())
    if(root_rels.isEmpty) {
      put("/_rels/.rels",
          XmlUtils.xmlToByteArray(Relations.toXML(root_rels)))
    }
    for {
      (name,part) <- parts
      val rels = relationships.getOrElse(name,Map())
    } {
      put(name,part.data)
      if(!rels.isEmpty) {
        val f = new File(name)
        val rel_dir = new File(f.getParentFile,"_rels")
        val rel_f = new File(rel_dir,f.getName  + ".rels")
        put(rel_f.getPath,XmlUtils.xmlToByteArray(Relations.toXML(rels)))
      }
    }
    zo.close()
    try { bo.close() } catch { case _ : Exception => }
    bo.toByteArray()
  }
}

object DocxPackage {
  def apply(data : Array[Byte]) = {
    import java.io._
    import java.util.zip._
    val zis = new ZipInputStream(new ByteArrayInputStream(data))
    val files = {
      var b = Map.newBuilder[String,Array[Byte]]
      var ze = zis.getNextEntry
      while(ze != null) {
        if(!ze.isDirectory()) {
          val entryData = IOUtils.toByteArray(zis)
          b += (ze.getName -> entryData)
        }
        zis.closeEntry()
      }
      zis.close()
      b.result()
    }
  }
}
        
        
*/      
