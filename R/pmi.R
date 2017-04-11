#' pointwise mutual information: a significance test for keywords in context
#' 
#' @param document 
#' @param window OPTIONAL
#' @param node OPTIONAL
#' @param floor OPTIONAL
#' @param remove_stops OPTIONAL
#' 
#' 
pmi <- function(document, window, node, floor, remove_stops = TRUE){
      # Test to see if the document is a collDB class
      # That is, that it has already gone through save_collocates
      if(class(document) != "collDB"){
            doc <- save_collocates(document = document, 
                                   window = window, 
                                   node = node, 
                                   remove_stops = remove_stops)
      }
      # Convert duplicates, locations below 1 and above the word count to NA
      left_locs <- doc[[1]] %>% unlist(.)
      left_locs[duplicated(left_locs)] <- NA
      left_locs[left_locs < 1] <- NA
      left_locs[left_locs > nrow(doc$doc_table)] <- NA
      
      all_locs <- c(left_locs, right_locs)
      # Convert locations into words
      all_counts <- doc$doc_table[all_locs,] %>%
            table(.) %>%
            tibble(word = names(.), count = .)
      # Leaving this here for offering locational collocate data
      #left_counts <- doc$doc_table[left_locs, ] %>%
       #     table(.) %>% 
        #    tibble(word = names(.), count = .)  
      
      #right_counts <- doc$doc_table[right_locs, ] %>%
       #     table(.) %>% 
        #    tibble(word = names(.), count = .)
}