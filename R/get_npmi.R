#' normalised pointwise mutual information: a significance test for keywords in context, normalised between 1 and -1
#' 
#' @param document A collDB, produced through save_collocates, or a text file
#' @param floor Collocates that occur fewer times than floor will be removed
#' @param window OPTIONAL The size of the collocate window on each side of tne node. Not required if importing a collDB from save_collocates
#' @param node OPTIONAL A key word or phrase. Not required if importing a collDB from save_collocates
#' @param remove_stops OPTIONAL If TRUE, stopwords are removed (stopwords derived from tidytext package). Not required if importing a collDB from save_collocates
#' @param remove_numerals OPTIONAL If TRUE, numerals are removed. Not required if importing a collDB from save_collocates
#' @param remove_punct OPTIONAL If TRUE, puntuation is removed. Not required if importing a collDB from save_collocates
#' @include get_pmi_internal.R
#' @import tibble dplyr
#' @keywords mutual information, collocates, kwic
#' @export
get_npmi <- function(document, pattern, window = 5, ngram = 1, floor = 3, remove_stopwords = TRUE, cache = FALSE){
      
      ## get pmi using get_pmi_internal
      pmi <- get_pmi2(document = document, pattern = pattern, window = window, ngram = ngram, remove_stopwords = remove_stopwords, cache = cache)
      
      npmi <- pmi %>%
            add_column(npmi = pmi$pmi/(-log(pmi$probxy))) %>%
            dplyr::select(phrase, npmi) %>% 
            arrange(., desc(npmi))

return(npmi)
}