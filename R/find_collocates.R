#' returns a list containing node locations, window locations, the node and its hash, and a doc_table
#'
#' @param document The document to be analysed
#' @param window The size of the collocate window on each side of tne node
#' @param node A key word or phrase
#' @param remove_stops If TRUE, stopwords are removed (stopwords derived from tidytext package)
#' @import tidyverse tidytext
#' @importFrom digest sha1
#' @keywords collocates kwic
#' @export
#' 

find_collocates <- function(document, window, node, remove_stops = TRUE){
      node_length <- length(unlist(strsplit(node, " ")))
      # Test to see if the node phrase is larger than the window
      if(node_length > ((window*2)+1)){ # longer than twice the window plus the keyword
            stop("Error: the node phrase is longer than the kwic window")
      }
      document <- tolower(document)
      # Hash the node to to create a single phrase (and ensure stopwords aren't removed)
      node1 <- sha1(node)
      document <- str_replace_all(document, node, node1)
      # Unnest
      word.t <- tibble(document) %>% 
            unnest_tokens(word, 
                          document,
                          token = "ngrams",
                          n = 1)
      # If required remove stopwords
      if(remove_stops == TRUE){
            data("stop_words")
            x <- which(!(word.t$word %in% stop_words))
                       word.t <- word.t[x,]
      }
      # Get locations of node
      node_loc <- which(word.t == node1)
      # If there are no matches, just return a vector of NAs
      if(length(node_loc) == 0){
            collocate_locs <- list(rep(NA, times=(window)), rep(NA, times=(window)), node, node1, word.t)
            
      } else {
            left_locs <- lapply(node_loc, function(x) ((x-window):(x-1)))
            right_locs <- lapply(node_loc, function(x) ((x+1):(x+window)))
            # No need for a tibble here: a list of vectors would be more efficient. 
            # A tibble would only be required if we were going to be displaying the locs, ubt we're not
            #collocate_locs <- lapply(seq_along(1:length(left_locs)), function(x) as_tibble(t(min(left_locs[[x]]):max(right_locs[[x]]))))
            #collocate_locs <- bind_rows(collocate_locs)
      collocate_locs <- list(left_locs, right_locs, node, node1, word.t)
      }
      names(collocate_locs) <- c("left_locs", "right_locs", "node", "node_hash", "doc_table")
      #left_cols <- paste("L", seq_along(window:1), sep="")
      #right_cols <- paste("R", seq_along(1:window), sep="")
      #name_cols <- c(left_cols, "node", right_cols)
      #colnames(collocate_locs) <- name_cols
      
 return(collocate_locs)     
}