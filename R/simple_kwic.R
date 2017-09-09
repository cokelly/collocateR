#' This function does not aim to replace the more thorough function from the Quanteda package. Instead it simply returns a tibble of KWICs arranged by row.
#' Internal formula for processing multigrams
#' 
#' @param document A character vector or list of character vectors
#' @param pattern A character vector containing a keyword 
#' @param window The number of context words to be displayed around the keyword Default 5
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param remove_stopwords Remove stopwords from the document (drawn from tidytext's stopwords data). Default TRUE.
#' @param remove_numerals Remove numerals
#' @param cache Organising collocates is the most time-consuming step in calculating frequencies and other collocation algorithms. The memoise package is used to cache specific iterations of this process. Deault FALSE.
#' @include internal_get_collocates.R
#' @import dplyr tibble
#' @importFrom tidytext unnest_tokens
#' @keywords keywords in context
#' @export

simple_kwic <- function(document, pattern, window = 5, ngram = 1, remove_stopwords = TRUE, remove_numerals = TRUE, cache = FALSE) {
  
  collocates <- get_collocates(document, pattern, window = window, ngram = ngram, remove_stopwords = remove_stopwords, remove_numerals = remove_numerals, cache = cache)
  
  kwic <- collocates[[2]]
  # this returns a single column tibble that I want to split back into relevant columns
  # of length...
  kwic_windows <- (window * 2) + 1
  # Just getting a range of positions
  kwic_range <- seq_along(1:nrow(kwic))
  # Probably overcomplicated chunk function using map, based on an answer from celacanto here: https://stackoverflow.com/questions/31062486/quickly-split-a-large-vector-into-chunks-in-r In fact this is slightly slower in tests with small datasets than the simpler version.
  chunk <- function(kwic_range, kwic_windows) {
    is <- seq(from = 1, to = length(kwic_range), by = kwic_windows)
    if (tail(is, 1) != length(kwic_range)) {
      is <- c(is, length(kwic_range))
    }
    chunks <- map(head(seq_along(is), -1), function(i) {
      start <- is[i]
      end <- is[i + 1] - 1
      kwic_range[start:end]
    })
    lc <- length(chunks)
    td <- tail(kwic_range, 1)
    chunks[[lc]] <- c(chunks[[lc]], td)
    return(chunks)
  }
  kwic_chunks <- chunk(kwic_range, kwic_windows)
  
  kwic_list <- kwic_chunks %>% map(., function(x) kwic[x, ])
  
  kwic.tibble <- kwic_list %>% bind_cols %>% t %>% as_tibble
  
  # Organise colnames
  Lcols <- paste("L", seq_along(window:1), sep = "") %>% rev
  Rcols <- paste("R", seq_along(1:window), sep = "")
  colnames(kwic.tibble) <- c(Lcols, "keyword", Rcols)
  
  return(kwic.tibble)
}