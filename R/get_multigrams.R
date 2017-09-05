#' Internal formula for processing multigrams
#' 
#' @param document A character vector or list of character vectors
#' @param pattern A character vector containing a keyword 
#' @param window The number of context words to be displayed around the keyword Default 5
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param remove_stopwords Remove stopwords from the document (based on tidytext's stopwords data). Default TRUE.
#' @param cache Organising collocates is the most time-consuming step in calculating frequencies and other collocation algorithms. The memoise package is used to cache specific iterations of this process. Default FALSE.
#' @include get_collocates.R
#' @import dplyr tibble
#' @importFrom tidytext unnest_tokens
#' @keywords frequencies
#'


get_multigrams <- function(document = document, pattern = pattern, window = window, ngram = ngram, remove_stopwords = remove_stopword, cache = cache){
  # Crete unigrams so as to remove stopwords
  if(isTRUE(remove_stopwords)){
  doc.t <- document %>% as_tibble %>%
  tidytext::unnest_tokens(.,
                word,
                value,
                token = "ngrams",
                n = 1)
  
# Add a column of row numbers for sensemaking purposes (see after anti_join below)
  row_numbers <- 1:nrow(doc.t)
  
  doc.t <- doc.t %>%
    add_column(`row numbers` = row_numbers, .before = "word")
  
  # Get tidytext stops
  stop_words <- tidytext::stop_words
  suppressMessages(doc.t2 <- doc.t %>%
                     anti_join(stop_words, by = "word") %>%
    arrange(`row numbers`))
# Turn it back into a character vector
  doc.t2 <- doc.t2 %>% 
    select(word) %>%
    collect %>%
    .[["word"]]
  #create a tibble of ngrams
  doc.t3 <- doc.t2 %>% as_tibble %>%
    tidytext::unnest_tokens(.,
                  word,
                  value,
                  token = "ngrams",
                  n = ngram)
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