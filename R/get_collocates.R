#' Organise a character vector or a list of character vectors into two ngram tibbles, one for the complete document and one for collocates 
#' 
#' @param doc A character vector or list of character vectors
#' @param pattern A character vector containing a keyword 
#' @param window The number of context words to be displayed around the keyword Default 5
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param remove_stopwords Remove stopwords from the document (based on tidytext's stopwords data). Default TRUE.
#' @param cache Organising collocates is the most time-consuming step in calculating frequencies and other collocation algorithms. The memoise package is used to cache specific iterations of this process.
#' @import tidytext dplyr purr memoise tibble
#' @keywords frequencies
 
get_collocates <- function(document, pattern, window = 5, ngram = 1, remove_stopwords = TRUE, cache = FALSE){
  # Ensure there are no spaces in the pattern
if(length(ngram) == 1 && ngram == 1){
  doc_locations <- get_unigrams(document = document, pattern = pattern, window = window, remove_stopwords = remove_stopwords, cache = cache)
  doc.t2 <- doc_locations[[1]]
  pattern_locations <- doc_locations[[2]]
}
  
if(length(ngram) == 1 && ngram > 1){
  doc_locations <- get_multigrams(document = document, pattern = pattern, ngram = ngram, window = window, remove_stopwords = remove_stopwords, cache = cache) 
  doc.t2 <- doc_locations[[1]]
  pattern_locations <- doc_locations[[2]]
}
  
  # For ngram of length >1 we're going to have to do a x %in% 1 and then map over the multigrams
  
  if(length(pattern_locations) == 0){
    warning("This document does not contain the pattern")
  } else { # the remainder calculates freqs if the pattern is found

  
all_locations <- pattern_locations %>%
  map(., function(x) (x-window):(x-1)) %>%
  map(., function(x) c(x, max(x)+1)) %>%
  map(., function(x) c(x, (max(x)+1):(max(x)+window))) %>%
  map(., as_tibble) %>%
  map(., function(x) x %>% rename(location = value))

all_locations <- unlist(all_locations) %>% unname
#remove duplicates
duplicates <- which(duplicated(all_locations) == TRUE)
all_locations[duplicates] <- NA

kwic_words <- all_locations %>% map(., function(x) doc.t2[x,]) %>%
  bind_rows

collocates <- list(all_words = doc.t2, kwics = kwic_words, pattern_recurrence = length(pattern_locations), doc_length = nrow(doc.t2))
}
return(collocates)
}