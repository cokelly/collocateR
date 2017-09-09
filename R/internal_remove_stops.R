remove_stops <- function(document){
  
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
  return(doc.t3)
}