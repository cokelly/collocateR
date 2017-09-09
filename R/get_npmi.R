#' normalised pointwise mutual information: a significance test for keywords in context, normalised between 1 and -1
#' 
#' @param document Acharacter vector or list of character vectors
#' @param node A key word or phrase to test
#' @param window The number of context words to be displayed around the keyword Default 5
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param floor Collocates that occur fewer times than floor will be removed

#' @param remove_stopwords Remove stopwords from the document (based on tidytext's stopwords data). Default TRUE.
#' @param remove_numerals Remove numerals
#' @param cache Organising collocates is the most time-consuming step in calculating frequencies and other collocation algorithms. The memoise package is used to cache specific iterations of this process. Default FALSE.
#' @include internal_get_pmi.R
#' @import tibble dplyr
#' @keywords mutual information, collocates, kwic
#' @export


get_npmi <- function(document, pattern, window = 6, ngram = 1, floor = 3, remove_stopwords = TRUE, remove_numerals = TRUE,  cache = FALSE){
      
      ## get pmi using get_pmi_internal
      pmi <- get_pmi2(document = document, pattern = pattern, window = window, ngram = ngram, floor = floor, remove_stopwords = remove_stopwords, remove_numerals = remove_numerals, cache = cache)
      
      npmi <- pmi %>%
            add_column(npmi = pmi$pmi/(-log(pmi$probxy))) %>%
            dplyr::select(phrase, npmi) %>% 
            arrange(., desc(npmi))

return(npmi)
}