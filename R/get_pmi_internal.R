#' pointwise mutual information: internal function (for use in pmi and npmi functions)
#' 
#' @param document A collDB, produced through save_collocates, or a text file
#' @param freqs collocate and document frequencies
#' @import tibble dplyr
#' @keywords mutual information, collocates, kwic

get_pmi2 <- function(document, pattern, window = 6, ngram = 1, floor = 3, remove_stopwords = TRUE, cache = FALSE){
  
  
freqs <-  get_freqs2(document = document, pattern = pattern, window = window, ngram = ngram, remove_stopwords = remove_stopwords, cache = cache)
  
# Assemble the bases for calculating pmi
pattern_recurrence <- freqs[[2]]
wordlength <- freqs[[3]]
freqs <- freqs[[1]]

freqs <- freqs %>% filter(`kwic freqs` >= floor)

probx <- pattern_recurrence/wordlength
proby <- freqs$`doc freqs`/wordlength
probxy <- freqs$`kwic freqs`/wordlength

pmi <- tibble(probx = rep(probx, length(proby))) %>%
  add_column(proby) %>%
  add_column(probxy) %>%
  add_column(pmi = log(probxy/(probx*proby))) %>%
  add_column(phrase = freqs$word, .before = "probx") %>%
  select(phrase, probxy, pmi) %>%
  arrange(desc(pmi))

return(pmi)
}