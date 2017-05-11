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
npmi <- function(document, floor = 3, ngrams = 1){
      
      ## Test taht the document is of class collDB
      if(!is.collDB(document)){
            stop("Use the save_collocates function to process the collocates in your document before processing")
            } else {
      # Get frequencies
      freqs <- get_freqs(document, ngrams)
      # Filter for floor
      freqs <- freqs %>% filter(coll_freq >= floor)
      
      if(nrow(freqs) == 0){
            npmi <- "No collocates. Try setting the floor at a lower level"
      } else {
      pmis <- get_pmi(document, freqs)
      
      npmi <- pmis %>%
            add_column(npmi = pmis$pmi/(-log(pmis$probxy))) %>%
            dplyr::select(phrase, npmi) %>% 
            arrange(., desc(npmi))
}
return(npmi)
            }
}