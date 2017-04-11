#' pointwise mutual information: a significance test for keywords in context
#' 
#' @param document 
#' 
#' 
pmi <- function(document, window, node, remove_stops = TRUE){
      # Test to see if the document is a collDB class
      # That is, that it has already gone through save_collocates
      if(class(document) != "collDB"){
            doc <- save_collocates(document = document, 
                                   window = window, 
                                   node = node, 
                                   remove_stops = remove_stops)
      }
      
}