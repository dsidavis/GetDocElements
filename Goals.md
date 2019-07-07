# Goals for this GetDocElements

Given bounding boxes from either Rtesseract or ReadPDF,

1. convert to a common format for subsequent operations

2. Identify/reconstruct elements from the bounding boxes, including:

  + columns - 2, 3 or more
  
  + header, footer, and page numbers/etc.
  
  + document title, authors, and date
  
  + section headers
  
  + section text, including sections that span pages or are
    interrupted by tables or figures.
  
  + images/tables with captions - not parsed at this stage, but identified and collected
  
  + lines, boxes and other page dividers?
  
Domain specific tasks for reading tabular data, bibliographies,
etc. are handled by ReadArticle and ReadTabularDocs.
