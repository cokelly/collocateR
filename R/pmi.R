#' pointwise mutual information: a significance test for keywords in context
#' 
#' @param document 
#' @param window OPTIONAL
#' @param node OPTIONAL
#' @param floor OPTIONAL
#' @param remove_stops OPTIONAL
#' 
#' 
pmi <- function(document, floor = 3, window, node, remove_stops = TRUE){
      # Test to see if the document is a collDB class
      # That is, that it has already gone through save_collocates
      if(class(document) != "collDB"){
            doc <- save_collocates(document = document, 
                                   window = window, 
                                   node = node, 
                                   remove_stops = remove_stops)
      } else {doc <- document}
      
      # Count the collocates
      
      coll_counts <- count_collocates(doc)
      
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
            collocate_freqs = coll_counts$coll_count, 
            doc_freqs = all_words_counts$all_count,
            probx = doc$node_recurrence/nrow(doc$doc_table),
            proby = doc_freqs/nrow(doc$doc_table),
            probxy = collocate_freqs/nrow(doc$doc_table),
            pmi = log(probxy/(probx*proby)),
            npmi = pmi/-log(probxy))
      
      return(pmi)
}