#' Get raw frequencies for ngrams within the document (plus wordcount for internal use)
#' 
#' @param doc A character vector or list of character vectors
#' @param pattern A character vector containing a keyword 
#' @param window The number of context words to be displayed around the keyword Default 5
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param remove_stopwords Remove stopwords from the document (based on tidytext's stopwords data). Default TRUE.
#' @param remove_numerals Remove numerals
#' @param cache Organising collocates is the most time-consuming step in calculating frequencies and other collocation algorithms. The memoise package is used to cache specific iterations of this process. Deault FALSE.
#' @include internal_get_collocates.R
#' @import dplyr tibble
#' @keywords frequencies

# This function is really about directing the phrases to the relevant unigram or multigram function
get_freqs2 <- function(document, pattern, window = 5, ngram = ngram, remove_stopwords = TRUE, remove_numerals, cache = FALSE){
  
  collocates <- get_collocates(document = document, pattern = pattern, window = window, ngram = ngram, remove_stopwords = remove_stopwords, remove_numerals = remove_numerals, cache = cache)
  
  full_doc <- collocates[[1]]
  kwics <- collocates[[2]]
  
  kwic_words <- kwics %>%
    count(word)
  
  freqs <- (all_freqs = full_doc %>% count(word)) %>%
    inner_join(., kwic_words, by = "word") %>%
    rename(`doc freqs` = n.x) %>%
    rename(`kwic freqs` = n.y) %>%
    arrange(desc(`kwic freqs`))
  
  freqs.l <- list(freqs, collocates[[3]], collocates[[4]])
  return(freqs.l)
}
