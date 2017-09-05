#' Internal formula for processing unigrams
#' 
#' @param doc A character vector or list of character vectors
#' @param pattern A character vector containing a keyword 
#' @param window The number of context words to be displayed around the keyword Default 5
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param remove_stopwords Remove stopwords from the document (based on tidytext's stopwords data). Default TRUE.
#' @param cache Organising collocates is the most time-consuming step in calculating frequencies and other collocation algorithms. The memoise package is used to cache specific iterations of this process. Deault FALSE.
#' @include get_collocates.R
#' @import dplyr tibble
#' @importFrom tidytext unnest_tokens
#' @keywords frequencies
#'
#'
get_unigrams <- function(document, pattern, window, remove_stopwords, cache){
  
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