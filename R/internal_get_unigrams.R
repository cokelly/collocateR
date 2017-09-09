#' Internal formula for processing unigrams
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
#' @importFrom tidytext unnest_tokens
#' @importFrom stringr str_replace_all
#' @keywords frequencies
#'
#'
get_unigrams <- function(document = document, pattern = pattern, window = window, ngram = ngram, remove_stopwords = remove_stopword, remove_numerals = remove_numerals, cache = cache){
  
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
  # If the pattern has more than one word
  if(isTRUE(length(unlist(strsplit(pattern, " "))) > 1)){
  pattern_no_spaces <- gsub(" ", "_", pattern)
  doc <- gsub(pattern, pattern_no_spaces, document)
  pattern <- pattern_no_spaces
  } else {
    doc <- document
  }
  # Turn doc into tibble
  doc.t <- doc %>% as_tibble %>%
    tidytext::unnest_tokens(.,
                  word,
                  value,
                  token = "ngrams",
                  n = 1)
  # Add a column of row numbers for sensemaking purposes (see after anti_join below)
  row_numbers <- 1:nrow(doc.t)
  
  doc.t <- doc.t %>%
    add_column(`row numbers` = row_numbers, .before = "word")
  
  
  if(isTRUE(remove_stopwords)){
    # Get tidytext stops
    stop_words <- tidytext::stop_words
    suppressMessages(doc.t2 <- doc.t %>%
                       anti_join(stop_words, by = "word") %>%
                     arrange(`row numbers`))
    
    #Next is just a bit of a sensemaking line, aimed at the sometimes odd arrangement of lines after the anti_join. See https://github.com/tidyverse/dplyr/issues/2964
    
    doc.t2 <- doc.t2 %>% arrange(`row numbers`) %>% 
      select(word)
  } else {
    doc.t2 <- doc.t
  }
  
  
  # Which row numbers match the pattern
  pattern_locations <- which(doc.t2$word == pattern)
  
  doc_and_patterns <- list(doc.t2, pattern_locations)
  
  return(doc_and_patterns)
  }