#' pointwise mutual information: internal function (for use in pmi and npmi functions)
#' 
#' @param doc Acharacter vector or list of character vectors
#' @param keyword A key word or phrase to test
#' @param window The number of context words to be displayed around the keyword Default 6
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param remove_stopwords Remove stopwords, derived from Quanteda's list
#' @param min_count Collocates that occur fewer times than floor will be removed
#' @param span Whether to include a window's width of words to the left of the keyword, to the right or on both sides
#' @include internal_pmi.R
#' @import tibble dplyr
#' @importFrom utils globalVariables
#' @keywords mutual information, collocates, kwic
#' @export

npmi <- function(doc, keyword, window = 6, ngram = 1, remove_stopwords = TRUE, min_count = 2, span = "both"){
      
      ## get pmi using get_pmi_internal
      pmi <- internal_pmi(doc = doc, keyword = keyword, window = window, ngram = ngram, remove_stopwords = remove_stopwords, min_count = min_count, span = span)
      
      npmi <- pmi %>%
            dplyr::mutate(npmi = pmi/(-log(probxy))) %>%
            dplyr::select(ngram, npmi) %>% 
            dplyr::arrange(., desc(npmi)) 

return(npmi)
}