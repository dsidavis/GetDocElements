margins =
    # XXX Deal with rotation when the text is actually rotated.
    # Specifying @rotation = 0 for case when there is a rotate line of text identifying how
    # the document was retrieved, e.g. downloaded by UC Davis.....
    #XXX should we extend this to get top and bottom.
    #XXX  and deal with a set of nodes, a page, and also an entire document.
    #XXX remove header and footer
function(page, bbox = getBBox2(getNodeSet(page, ".//text[@rotation = 0]")), ...)
    UseMethod("margins")

margins.character =
function(page, bbox = getBBox2(getNodeSet(page, ".//text[@rotation = 0]")), ...)        
{
    margins(readPDFXML(page), ...)
}

margins.PDFToXMLDoc =
function(page, bbox = getBBox2(getNodeSet(page, ".//text[@rotation = 0]")), asDataFrame = TRUE, ...)        
{
    ans = lapply(getPages(page), margins)
    if(asDataFrame)
        as.data.frame(do.call(rbind, ans))
    else
        ans
}

margins.XMLInternalNode = margins.PDFToXMLPage =
function(page, bbox = getBBox2(), ...)
{
    margins(getNodeSet(page, ".//text[@rotation = 0]"))
}

margins.list = margins.XMLNodeSet =
function(page, bbox = getBBox2(unlist(page)), ...)
{    
   c(left = min(bbox[, 1]), right = max(bbox[,1] + bbox[,3]), top = min(bbox[,2]), bottom = max(bbox[,4]))
}
