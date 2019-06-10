library(GetDocElements)
library(ReadPDF)

f = system.file("samples", "2Column.xml", package = "GetDocElements")

doc = readPDFXML(f)

