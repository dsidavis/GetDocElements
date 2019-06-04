findKeywordDecl =
    # Find Key Words, Keywords, etc. 
    # Could add test for bold font.
function(page)
{
    getNodeSet(page, ".//text[starts-with(lower-case(.), 'key words') or starts-with(., 'Keywords') or starts-with(., 'Key words') or starts-with(., 'Keyword Index')]")    
}
anyTextToLeft =
function(node, bbox = getBBox2(list(node)), page = xmlParent(node))
{
  length(getNodeSet(page, sprintf(".//text[ abs(@top - %f) < 30 and @left <=  %f]", bbox[1,2], bbox[1,1]*.66))) > 0
}
spansColumns =
function(page, cols = getColPositions(as(page, "XMLInternalDocument"), docFont = FALSE),
         colNodes = getTextByCols(page, asNodes = TRUE, breaks = cols),
         doc = as(page, "XMLInternalDocument"), abstractDecl = NULL)
{
    #
    # Find all the lines that span multiple columns.  This includes title, authors, and abstract and possibly centered
    #  section headers.
    # Find the ones that don't have a large gap between the text segments,
    #                    start in the first column
    # Within these lines, find the subset that are consecutive, left aligned (allowing for a small indentation) and span 
    #

    if(is.list(cols))
       cols = cols[[1]] # XXX make better

    # Get all the lines of text, across all columns
    ll = nodesByLine(getNodeSet(page, ".//text"))
    # get the left and right of each line 
    r = t(sapply(ll, function(x) { bb = getBBox2(x); c(start = min(bb[,1] ), end = max(bb[,1] + bb[,3]))}))
    #
    mdiff = sapply(ll, function(x) max(gapBetweenSegments(x)))
    gap = mostCommon(mdiff[mdiff > 0])
    w = r[, "start"] < cols[2] & r[, "end"] > cols[length(cols)] & mdiff < gap
    if(!any(w))
       return(list())
    
    idx = split((1:length(ll))[w], cumsum(!w)[w])
    # Pick the block with the most text. XXX  Need to make this more robust

    b = idx[[which.max(sapply(idx, length))]]

    # now patch up
    
    # XXX add an extra line after this as the final line may not span the entire width to be beyond the start of the last column.
    ans = unlist(ll[ c(b, max(b) + 1) ] )

    bb2 = getBBox2(ans)
    left = mostCommon(bb2[, "left"])
    i = abs(r[, "start"] - left) < 15
    ans = unlist(ll[i])

    if(length(abstractDecl)) {
        bb = getBBox2(ans)
        top = getBBox2(abstractDecl)
        ans = ans[ bb[, "top"] >= (top[1, "top"] - 5)]
    } 
    
    ans
}

mostCommon =
function(x, convert = as.numeric)
{
    tt = table(x)
    ans = names(tt)[tt == max(tt)]
    if(!is.null(convert))
        convert(ans)
    else
        ans
}

spansColumns2 =
function(page, cols = getColPositions(page, docFont = FALSE), colNodes = getTextByCols(page, asNodes = TRUE, breaks = cols),
            doc = as(page, "XMLInternalDocument") )
{
    bb = getBBox2(colNodes[[1]])
    w = bb[,1] + bb[,3] > cols[length(cols)] #XXXX!!!!
    nodes = NULL
   
    if(any(w)) {
        nodes = colNodes[[1]][w]
        # Different ways to filter just the abstract.
        # Last paragraph of these nodes with a big enough space in between
        # all the nodes with the same font as the last line.
        # The lines are right aligned (and left) as opposed to just centered
        # Below a line with Received

                       # taken from above for lines - merge.
        fontInfo = getFontInfo(doc)
        fonts = sapply(nodes, xmlGetAttr, "font")
#        browser()
        w = fonts == fonts[length(fonts)]
        nodes = nodes[w]
        # Now have to get all the nodes in this region, not just the ones
        # with these fonts as that drops, e.g., italics.
        nodes = getNodesBetween(nodes[[1]], nodes[[length(nodes)]])
    }

    nodes
}


gapBetweenSegments =
function(nodes, bbox = getBBox2(nodes))
{
   n = nrow(bbox)
   if(n == 1) return(-1)
   bb = bbox[order(bbox[,1]), ] 
   bb[-1, 1] - (bb[1:(n-1), 1] + bb[1:(n-1), 3])
}



context =
function(pos, vec, num = 10)
{
    if(length(pos) > 1)
        return(lapply(pos, context, vec))


    vec[seq(max(1, pos - num),  min(length(vec), pos + num))]
}



isNodeIn =
function(node, boxes, pos = getBBox2(list(node)))
{
    pos[,1] >= boxes[,1] & pos[,2] > boxes[,2] & (pos[,1] + pos[,3]) < boxes[,3] & (pos[,2] + pos[,4]) < boxes[,4] 
}


getBelowLine =
    ## check that the font for the content below is different from the document font
    ## XXX If line in 2nd column and lower than one in column 1, we won't get anything. So deal with both columns.
    ###
function(doc, col = 1, colPos = getColPositions(p1))
{
    if(is.character(doc))
        doc = readPDFXML(doc)

    p1 = doc[[1]]
    
    lines = getBBox(p1)
     # Get horizontal lines only
    lines = lines[ lines[, "y0"] == lines[, "y1"], ]

    tlines = if(col == length(colPos))
               lines[lines[, "x0"] >= colPos[2] ,]
            else
                lines[lines[, "x0"] < colPos[2] ,]
    tlines = tlines[tlines[,"y0"] == max(tlines[, "y0"]) ,]
    if(is.matrix(tlines))
        tlines = tlines[1,]

    xpath.query = sprintf(".//text[ @top > %f and (@left + @width ) < %f]", tlines["y0"], colPos[2] )
    txt = getNodeSet(p1, xpath.query)
    ## Check these are all in a different font than the document's regular text.

    txt
}
