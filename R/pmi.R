#' pointwise mutual information: a significance test for keywords in context
#' 
#' @param document A collDB, produced through save_collocates, or a text file
#' @param floor Collocates that occur fewer times than floor will be removed
#' @param window OPTIONAL The size of the collocate window on each side of tne node. Not required if importing a collDB from save_collocates
#' @param node OPTIONAL A key word or phrase. Not required if importing a collDB from save_collocates
#' @param remove_stops OPTIONAL If TRUE, stopwords are removed (stopwords derived from tidytext package). Not required if importing a collDB from save_collocates
#' @param remove_numerals OPTIONAL If TRUE, numerals are removed. Not required if importing a collDB from save_collocates
#' @param remove_punct OPTIONAL If TRUE, puntuation is removed. Not required if importing a collDB from save_collocates
#' @include CollDB.R count_unigrams.R
#' @import tibble dplyr
#' @keywords mutual information, collocates, kwic
#' @export
pmi <- function(document, floor = 3, window, node, remove_stops = TRUE, remove_numerals = TRUE, remove_punct = TRUE){
      
      # Test to see if the document is a collDB class or a text document
      if(class(document) != "character" && class(document) != "collDB"){
            stop("This package requires a character vector or an internal 'collDB' list")
      }
      # That is, that it has already gone through save_collocates
      if(class(document) != "collDB"){
            doc <- save_collocates(document = document, 
                                   window = window, 
                                   node = node, 
                                   remove_stops = remove_stops)
      } else {doc <- document}
      
      # Count the collocates
      
      coll_counts <- count_unigrams(doc)
      
      coll_counts <- filter(coll_counts, coll_count > floor)
      
      # Count all relevant words in the whole document
      # Note, for efficiency sake I only count the words that are in the collocate window
      all_words_counts <- filter(doc$doc_table, word %in% coll_counts$word) %>% 
            table(.) %>%
            tibble(word = names(.), all_count = .)
      
      counts <- left_join(coll_counts, all_words_counts, by = "word")
      
      # Calculate the PMI
      # Calculate pmi
      
      pmi <- tibble(phrase = coll_counts$word, 
            collocate_freqs = as.numeric(coll_counts$coll_count), 
            doc_freqs = as.numeric(all_words_counts$all_count),
            probx = as.numeric(doc$node_recurrence/nrow(doc$doc_table)),
            proby = as.numeric(doc_freqs/nrow(doc$doc_table)),
            probxy = as.numeric(collocate_freqs/nrow(doc$doc_table)),
            pmi = as.numeric(log(probxy/(probx*proby))))
      
      return(pmi)
}