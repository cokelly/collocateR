#' A z-score calculates a probability score that compares the observed collocate frequency to its expected value
#'
#' @param document A collDB, produced through save_collocates, or a text file
#' @param floor Collocates that occur fewer times than floor will be removed
#' @param ngrams The length of the ngrams to be tested
#' @param window OPTIONAL The size of the collocate window on each side of tne node. Not required if importing a collDB from save_collocates
#' @param node OPTIONAL A key word or phrase. Not required if importing a collDB from save_collocates
#' @param remove_stops OPTIONAL If TRUE, stopwords are removed (stopwords derived from tidytext package). Not required if importing a collDB from save_collocates
#' @param remove_numerals OPTIONAL If TRUE, numerals are removed. Not required if importing a collDB from save_collocates
#' @param remove_punct OPTIONAL If TRUE, puntuation is removed. Not required if importing a collDB from save_collocates
#'
#' @include CollDB.R count_unigrams.R
#' @import tibble dplyr
#' @include get_freqs.R
#' @keywords zscore, collocates, kwic
#' @export

z_score <- function(document, floor = 3, ngrams = 1){

      # Test taht the document is of class collDB
      if(!is.collDB(document)){
            stop("Use the save_collocates function to process the collocates in your document before processing")
            } else {
      # Get frequencies
      freqs <- get_freqs(document, ngrams)
      # Filter for floor
      freqs <- freqs %>% filter(coll_freq >= floor)
      if(nrow(freqs) == 0){
            z_score <- "No collocates. Try setting the floor at a lower level"
      } else {
      #calculcate the z-score
      z_score <- tibble(word = freqs$word, # Collocates
                        `collocate freq` = freqs$coll_freq, 
                        #prob = Probability of collocate occuring where the node does not occur:
                        #frequency in document / overall word count - freq of node
                        prob = freqs$doc_freq/(nrow(document$doc_table)-document$node_recurrence), 
                        # expected: how many times would collocate occur if randomly distributed?:
                        # prob * freq of node * window * span (window but here calculated from collDB list)
                        expected = prob*document$node_recurrence*(length(document$left_locs[[1]])*2),
                        # frequency of collocates in context - expected / sqrt (expected * (1-prob))
                        zscore = (freqs$coll_freq-expected)/sqrt(expected*(1-prob))) # (Fn,c-E/sqrt(E(1-p)))

      z_score <- z_score %>% select(word, `collocate freq`, z_score = zscore) %>% arrange(desc(z_score))
}

return(z_score)
            }
}
