#' pointwise mutual information: internal function (for use in pmi and npmi functions)
#' 
#' @param document A collDB, produced through save_collocates, or a text file
#' @param pattern A character vector containing a keyword 
#' @param window The number of context words to be displayed around the keyword Default 5
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param remove_stopwords Remove stopwords from the document (based on tidytext's stopwords data). Default TRUE.
#' @param cache Organising collocates is the most time-consuming step in calculating frequencies and other collocation algorithms. The memoise package is used to cache specific iterations of this process.
#' @param floor A frequency cutoff for words or phrases to test
#' @include get_freqs_internal.R
#' @import tibble dplyr
#' @keywords mutual information, collocates, kwic

get_pmi <- function(document, pattern, window = 6, ngram = 1, floor = 3, remove_stopwords = TRUE, cache = FALSE){
  
  # Get frequencies using the internal algorithm (which returns wordcounts etc from get_collocates)
freqs <-  get_freqs2(document = document, pattern = pattern, window = window, ngram = ngram, remove_stopwords = remove_stopwords, cache = cache)
  
# Assemble the bases for calculating pmi
pattern_recurrence <- freqs[[2]]
wordlength <- freqs[[3]]
freqs <- freqs[[1]]

freqs <- freqs %>% filter(`kwic freqs` <= floor)

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

return(pmi)
}