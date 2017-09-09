#' Internal formula for processing multigrams
#' 
#' @param document A character vector or list of character vectors
#' @param pattern A character vector containing a keyword 
#' @param window The number of context words to be displayed around the keyword Default 5
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param remove_stopwords Remove stopwords from the document (based on tidytext's stopwords data). Default TRUE.
#' @param remove_numerals Remove numerals
#' @param cache Organising collocates is the most time-consuming step in calculating frequencies and other collocation algorithms. The memoise package is used to cache specific iterations of this process. Default FALSE.
#' @include internal_get_collocates.R
#' @import dplyr tibble
#' @importFrom tidytext unnest_tokens
#' @importFrom stringr str_replace_all
#' @keywords frequencies
#'


get_multigrams <- function(document = document, pattern = pattern, window = window, ngram = ngram, remove_stopwords = remove_stopword, remove_numerals = remove_numerals, cache = cache){
  # Good treatment of possessive apostrophes "'s"and contractions "'t" plus handle curly apostrophes etc.
  # Remove curly quotes etc
  document <- stringi::stri_trans_general(document, "latin-ascii")
  # Try to deal with crappy word fragments and apostrophes
  document <- stringr::str_replace_all(document, "[\r\n]", " ")
  document <- stringr::str_replace_all(document, "-", " ")
  document <- stringr::str_replace_all(document, "'s", "s")
  document <- stringr::str_replace_all(document, "’s", "s")
  document <- stringr::str_replace_all(document, "'t", "t")
  document <- stringr::str_replace_all(document, "’t", "t")   
  
  if(isTRUE(remove_numerals)){
    document <- gsub("[[:digit:]]", " ", document)
  }
  

  if(isTRUE(remove_stopwords)){
    doc.t3 <- remove_stops(document)
  } else { # if remove_stopwords is false
    # a tibble of ngrams
  doc.t3 <- document %>% as_tibble %>%
    tidytext::unnest_tokens(.,
                  word,
                  value,
                  token = "ngrams",
                  n = ngram)
  }
  
  
  
  pattern_locations <- grep(pattern, doc.t3$word)
    
doc_and_patterns <- list(doc.t3, pattern_locations)
  
  return(doc_and_patterns)
}