# lexml-renderer-docx

O ponto de entrada está na classe `br.gov.lexml.renderer.docx.LexmlToDocx`:

```scala
class LexmlToDocx(config : LexmlToDocxConfig) {
  
  def convert(source : Array[Byte]) : Array[Byte] = { ... }
```

Para usar, basta instanciar passando uma instância de `br.gov.lexml.renderer.docx.LexmlToDocxConfig`
e chamar o método `convert` passando o arquivo lexml em formato binário. O resultado será o arquivo
DOCX em formato binário.
