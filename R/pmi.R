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

pmi <- function(doc, keyword, window = 6, ngram = 1, remove_stopwords = TRUE, min_count = 2, span = "both"){

      internal_pmi <- internal_pmi(doc, keyword, window, ngram, remove_stopwords, min_count, span = span)
      
# Calculate the pmi
pmi <- internal_pmi %>%
      select(ngram, pmi) %>%
      arrange(desc(pmi)) 

return(pmi)
}