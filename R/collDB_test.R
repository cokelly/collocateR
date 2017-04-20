#' Tests input document and returns in collDB if required
#' 
#' @param document The document to be analysed
#' @param window The size of the collocate window on each side of tne node
#' @param node A key word or phrase
#' @param remove_stops If TRUE, stopwords are removed (stopwords derived from quanteda package)
#' @param remove_numerals If TRUE, numerals are removed
#' @param remove_punct If TRUE, puntuation is removed

collDB_test <- function(document, window, node, remove_stops = TRUE, remove_numerals = TRUE, remove_punct = TRUE){
# Test to see if the document is a collDB class or a text document
if(class(document) != "character" && class(document) != "collDB"){
      stop("This package requires a character vector or an internal 'collDB' list")
}
# That is, that it has already gone through save_collocates
if(class(document) != "collDB"){
      doc <- save_collocates(document = document, 
                             window = window, 
                             node = node, 
                             remove_stops = remove_stops,
                             remove_numerals = remove_numerals,
                             remove_punct = remove_punct)
} else {doc <- document}

return(doc)
}