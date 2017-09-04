#'
#' @importFrom gtools odd

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