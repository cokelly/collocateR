#' pointwise mutual information: a significance test for keywords in context
#' 
#' @param document A collDB, produced through save_collocates, or a text file
#' @param floor Collocates that occur fewer times than floor will be removed
#' @param ngrams The length of the ngrams to be tested
#' @param window OPTIONAL The size of the collocate window on each side of tne node. Not required if importing a collDB from save_collocates
#' @param node OPTIONAL A key word or phrase. Not required if importing a collDB from save_collocates
#' @param remove_stops OPTIONAL If TRUE, stopwords are removed (stopwords derived from tidytext package). Not required if importing a collDB from save_collocates
#' @param remove_numerals OPTIONAL If TRUE, numerals are removed. Not required if importing a collDB from save_collocates
#' @param remove_punct OPTIONAL If TRUE, puntuation is removed. Not required if importing a collDB from save_collocates
#' @include CollDB.R count_unigrams.R
#' @import tibble dplyr
#' @include get_freqs.R
#' @keywords mutual information, collocates, kwic
#' @export
pmi <- function(document, floor = 3, ngrams = 1){

      # Test taht the document is of class collDB
      if(!is.collDB){stop("Use the save_collocates function to process the collocates in your document first")}
      # Get frequencies
      freqs <- get_freqs(document, ngrams)
      # Filter for floor
      freqs <- freqs %>% filter(coll_freq >= floor)
      
      if(nrow(freqs) == 0){
            pmi <- "No collocates. Try setting the floor at a lower level"
      } else {
      # Calculate pointwise mutual information
      # This is in a separate function to allow for use in npmi function
      raw_pmi <- get_pmi(document, freqs)
      
      pmi <- raw_pmi %>% 
            dplyr::select(phrase, pmi) %>% 
            arrange(., desc(pmi))
      }
      return(pmi)
}
