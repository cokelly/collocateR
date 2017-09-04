#' This function does not aim to replace the more thorough function from the Quanteda package. Instead it simply returns a tibble of KWICs arranged by row.
#'@param 
#' 
#'@param import dplyr purr
#'@param export

simple_kwic <- function(document, pattern, window = 5, ngram = 1, remove_stopwords = TRUE, cache = FALSE){
  
  collocates <- get_collocates(document, pattern = pattern, window = 5, ngram = 1, remove_stopwords = TRUE, cache = FALSE)

kwic <- collocates[[2]]
# this returns a single column tibble that I want to split back into relevant columns of length...
kwic_windows <- (window*2)+1
# Just getting a range of positions
kwic_range <- seq_along(1:nrow(kwic))
# Probably overcomplicated chunk function using map
chunk <- function(kwic_range, kwic_windows){
  is <- seq(from = 1, to = length(kwic_range), by = kwic_windows)
  if(tail(is, 1) != length(kwic_range)){
    is <- c(is, length(kwic_range))
  }
  chunks <- map(head(seq_along(is), -1),
                     function(i){
                       start <- is[i];
                       end <- is[i+1]-1;
                       kwic_range[start:end]})
                lc <- length(chunks)
                td <- tail(kwic_range, 1)
                chunks[[lc]] <- c(chunks[[lc]], td)
                return(chunks)
}
kwic_chunks <- chunk(kwic_range, kwic_windows)

kwic_list <- kwic_chunks %>% 
  map(., function(x) kwic[x,])

kwic.tibble <- kwic_list %>% bind_cols %>% t %>% as_tibble

#Organise colnames
Lcols <- paste("L", seq_along(window:1), sep = "") %>% rev
Rcols <- paste("R", seq_along(1:window), sep = "")
colnames(kwic.tibble) <- c(Lcols, "keyword", Rcols)

return(kwic.tibble)
}