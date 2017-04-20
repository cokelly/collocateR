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
#' @importFrom magrittr "%>%"
#' @include get_freqs.R
#' @keywords zscore, collocates, kwic
#' @export

z_score <- function(document, floor = 3, ngrams = 1, window, node, remove_stops = TRUE, remove_numerals = TRUE, remove_punct = TRUE){

      # Test the document and return a collDB list if it hasn't been done already
      document <- collDB_test(document, window, node, remove_stops, remove_numerals, remove_punct)
      # Get frequencies
      freqs <- get_freqs(document, ngrams, window = window, node = document$node, remove_stops = TRUE, remove_numerals = TRUE, remove_punct = TRUE)
      # Filter for floor
      freqs <- freqs %>% filter(coll_freq >= floor)

      #calculcate the z-score
      z_score <- tibble(word = freqs$word,
                        prob = freqs$doc_freq/(nrow(document$doc_table)-collocates$node_recurrence), #prob = Fc/(N-Fn)
                        expected = prob*collocates$node_recurrence*(length(collocates$left_locs[[1]])*2), #expected = pFnS
                        zscore = (freqs$coll_freq-expected)/sqrt(expected*(1-prob))) # (Fn,c-E/sqrt(E(1-p)))

      z_score <- z_score %>% select(word, z_score = zscore) %>% arrange(desc(z_score))


return(z_score)
}
