library(GetDocElements)
library(ReadPDF)

f = system.file("samples", "3Column.xml", package = "GetDocElements")

doc = readPDFXML(f)
