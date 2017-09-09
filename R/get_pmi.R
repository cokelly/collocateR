#' pointwise mutual information: internal function (for use in pmi and npmi functions)
#' 
#' @param document Acharacter vector or list of character vectors
#' @param node A key word or phrase to test
#' @param window The number of context words to be displayed around the keyword Default 5
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param floor Collocates that occur fewer times than floor will be removed

#' @param remove_stopwords Remove stopwords from the document (based on tidytext's stopwords data). Default TRUE.
#' @param remove_numerals Remove numerals
#' @param cache Organising collocates is the most time-consuming step in calculating frequencies and other collocation algorithms. The memoise package is used to cache specific iterations of this process. Default FALSE.
#' @include internal_get_freqs.R
#' @import tibble dplyr
#' @keywords mutual information, collocates, kwic
#' @export

get_pmi <- function(document, pattern, window = 6, ngram = 1, floor = 3, remove_stopwords = TRUE, remove_numerals = TRUE,  cache = FALSE){
  
  # Get frequencies using the internal algorithm (which returns wordcounts etc from get_collocates)
freqs <-  get_freqs2(document = document, pattern = pattern, window = window, ngram = ngram, remove_stopwords = remove_stopwords, remove_numerals = remove_numerals, cache = cache)
  
# Assemble the bases for calculating pmi
pattern_recurrence <- freqs[[2]]
wordlength <- freqs[[3]]
freqs <- freqs[[1]]

freqs <- freqs %>% filter(`kwic freqs` >= floor)


if(nrow(freqs) == 0){
  pmi <- "No collocates. Try setting the floor at a lower level"
} else {
  

probx <- pattern_recurrence/wordlength
proby <- freqs$`doc freqs`/wordlength
probxy <- freqs$`kwic freqs`/wordlength

pmi <- tibble(probx = rep(probx, length(proby))) %>%
  add_column(proby) %>%
  add_column(probxy) %>%
  add_column(pmi = log(probxy/(probx*proby))) %>%
  add_column(phrase = freqs$word, .before = "probx") %>%
  select(phrase, pmi) %>%
  arrange(desc(pmi))
}
return(pmi)
}