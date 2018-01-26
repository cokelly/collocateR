#' pointwise mutual information: internal function (for use in pmi and npmi functions)
#' 
#' @param doc Acharacter vector or list of character vectors
#' @param keyword A key word or phrase to test
#' @param window The number of context words to be displayed around the keyword Default 6
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param min_count Collocates that occur fewer times than floor will be removed
#' @param cache Organising collocates is the most time-consuming step in calculating frequencies and other collocation algorithms. The memoise package is used to cache specific iterations of this process. Default FALSE.
#' @include get_freqs.R
#' @import tibble dplyr memoise
#' @importFrom utils globalVariables
#' @keywords mutual information, collocates, kwic
#' @export

if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "probxy"))
get_npmi <- function(doc, keyword, window = 6, ngram = 1, min_count = 2, cache = FALSE){
      
      ## get pmi using get_pmi_internal
      pmi <- internal_get_pmi(doc = doc, keyword = keyword, window = window, ngram = ngram, min_count = min_count, cache = cache)
      
      npmi <- pmi %>%
            mutate(npmi = pmi/(-log(probxy))) %>%
            dplyr::select(ngram, npmi) %>% 
            arrange(., desc(npmi))

return(npmi)
}