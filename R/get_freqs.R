#' Get raw frequencies for ngrams within the document
#' 
#' @param document A collDB, produced through save_collocates, or a text file
#' @param ngrams The length of the ngrams to be tested
#' @include save_collocates.R count_multigrams.R count_unigrams.R
#' @keywords frequencies
#' @export

# This function is really about directing the phrases to the relevant unigram or multigram function
get_freqs <- function(document, ngrams = 1, window, node, remove_stops = TRUE, remove_numerals = TRUE, remove_punct = TRUE){

      # Test the document and return a collDB list if it hasn't been done alread
      document <- collDB_test(document, window, node, remove_stops, remove_numerals, remove_punct)
      
# Test ngram lengths to send request for unigrams to the more efficient function
# For unigrams
if(isTRUE(length(ngrams) == 1) && (isTRUE(ngrams == 1))){
      freqs <- count_unigrams(document)
}
# For multigrams
if(isTRUE(length(ngrams) == 1) && (isTRUE(ngrams > 1))){
      freqs <- count_multigrams(document, ngrams)
}
# For a variety of ngrams
if(isTRUE(length(ngrams) > 1)){
      direct_to_ngram_function <- function(document, ngram){
            if(ngram == 1){
                  freqs <- count_unigrams(document)
            } else {
                  freqs <- count_multigrams(document, ngram)
            }
            return(freqs)
      }
      freqs <- lapply(seq_along(1:length(ngrams)), function(x) 
            direct_to_ngram_function(document, ngrams[x])) %>%
            bind_rows
}
      return(freqs)
}