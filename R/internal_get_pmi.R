#' pointwise mutual information: internal function (for use in pmi and npmi functions)
#' 
#' @param doc Acharacter vector or list of character vectors
#' @param keyword A key word or phrase to test
#' @param window The number of context words to be displayed around the keyword Default 5
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param min_count Collocates that occur fewer times than floor will be removed
#' @param cache Organising collocates is the most time-consuming step in calculating frequencies and other collocation algorithms. The memoise package is used to cache specific iterations of this process. Default FALSE.
#' @include get_freqs.R
#' @import tibble dplyr memoise
#' @keywords mutual information, collocates, kwic

internal_get_pmi <- function(doc, keyword, window = 6, ngram = 1, min_count = 2, cache = FALSE){
      
      # Get wordcount and the number of times the keyword recurs
      
      # Get collocate frequencies
      if(cache == TRUE){
            mget_freqs <- memoise::memoise(get_freqs)
            freqs <-  mget_freqs(doc = doc, keyword = keyword, window = window, ngram = ngram, min_count = min_count, cache = cache)
      } else {
            freqs <-  get_freqs(doc = doc, keyword = keyword, window = window, ngram = ngram, min_count = min_count, cache = cache)
      }
      wordcount <- as.integer(sum(str_count(doc, "\\S+")))
      #It would be easier and perhaps more efficient to count the below from the documentary source with sum(str_count(keyword, " ")) but this returns a higher count than the kwic function, than tibble%>%unnest_tokens%>%summarise etc
      keyword_recurrence <- freqs %>% 
            filter(ngram == keyword) %>%
            select(`Document Frequency`) %>% 
            unlist %>% 
            as.integer
      
      # Calculate the pmi
      pmi <- freqs %>%
            mutate(probx = as.integer(keyword_recurrence)/wordcount) %>%
            mutate(proby = as.integer(`Document Frequency`)/wordcount) %>% 
            mutate(probxy = as.integer(`Collocate Frequency`)/wordcount) %>%
            mutate(pmi = log(probxy/(probx*proby)))

return(pmi)
}