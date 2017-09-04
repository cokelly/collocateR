#'
#'
#'@param 
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