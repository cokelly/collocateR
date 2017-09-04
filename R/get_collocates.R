#' Organise a character vector or a list of character vectors into two ngram tibbles, one for the complete document and one for collocates 
#' 
#' @param doc A character vector or list of character vectors
#' @param pattern A character vector containing a keyword 
#' @param window The number of context words to be displayed around the keyword Default 5
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param remove_stopwords Remove stopwords from the document (based on tidytext's stopwords data). Default TRUE.
#' @param cache Organising collocates is the most time-consuming step in calculating frequencies and other collocation algorithms. The memoise package is used to cache specific iterations of this process.
#' @import tidytext dplyr purr memoise
#' @importFrom gtools odd
#' @keywords frequencies
 
get_collocates <- function(document, pattern, window = 5, ngram = 1, remove_stopwords = TRUE, cache = FALSE){
  # Ensure there are no spaces in the pattern
if(isTRUE(length(unlist(strsplit(pattern, " "))) > 1)){
  pattern_no_spaces <- gsub(" ", "_", pattern)
} else {
  pattern_no_spaces <- pattern
}

doc <- gsub(pattern, pattern_no_spaces, doc)

# Turn doc into tibble
doc.t <- doc %>% as_tibble %>%
  unnest_tokens(.,
                word,
                value,
                token = "ngrams",
                n = ngram)
# Add a column of row numbers for sensemaking purposes (see after anti_join below)
row_numbers <- 1:nrow(doc.t)

doc.t <- doc.t %>%
  add_column(`row numbers` = row_numbers, .before = "word")

if(isTRUE(remove_stopwords)){
  # Get tidytext stops
  data("stop_words")
  suppressMessages(doc.t2 <- doc.t %>%
    anti_join(stop_words))


#Next is just a bit of a sensemaking line, aimed at the sometimes odd arrangement of lines after the anti_join. See https://github.com/tidyverse/dplyr/issues/2964

doc.t2 <- doc.t2 %>% arrange(`row numbers`) %>% 
  select(word)
} else {
  doc.t2 <- doc.t
}
# Which row numbers match the pattern
pattern_locations <- which(doc.t2$word == pattern_no_spaces)

# Message if the pattern is not found
if(isTRUE(length(pattern_locations) == 0)){
  message("No pattern match in this document")
  collocates <- NULL
} else { # the remainder calculates freqs if the pattern is found

# Shorten window if required so as to ensure only bigrams within the range are returned
  if(ngram > 1){
  window0 <- window/ngram
  if(isTRUE(odd(floor(window0)))){
    window <- floor(window0)+1
  } else {
    window <- window0
  }}
  
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