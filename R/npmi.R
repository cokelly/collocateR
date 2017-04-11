#' normalised pointwise mutual information: a significance test for keywords in context, normalised between 1 and -1
#' 
#' @param document A collDB, produced through save_collocates, or a text file
#' @param floor Collocates that occur fewer times than floor will be removed
#' @param window OPTIONAL The size of the collocate window on each side of tne node. Not required if importing a collDB from save_collocates
#' @param node OPTIONAL A key word or phrase. Not required if importing a collDB from save_collocates
#' @param remove_stops OPTIONAL If TRUE, stopwords are removed (stopwords derived from tidytext package). Not required if importing a collDB from save_collocates
#' @param remove_numerals OPTIONAL If TRUE, numerals are removed. Not required if importing a collDB from save_collocates
#' @param remove_punct OPTIONAL If TRUE, puntuation is removed. Not required if importing a collDB from save_collocates
#' @include CollDB.R save_collocates.R pmi.R
#' @import tibble dplyr
#' @keywords mutual information, collocates, kwic
#' @export
npmi <- function(document, floor = 3, window, node, remove_stops = TRUE, remove_numerals = TRUE, remove_punct = TRUE){
      # Test to see if the document is a collDB class
      # That is, that it has already gone through save_collocates
      if(class(document) != "collDB"){
            doc <- save_collocates(document = document, 
                                   window = window, 
                                   node = node, 
                                   remove_stops = remove_stops)
      } else {doc <- document}
      
      pmi <- pmi(document, floor = 3, window, node, remove_stops = TRUE)
      
      npmi <- bind_rows(pmi,
                        npmi = pmi/-log(probxy))

return(npmi)
}