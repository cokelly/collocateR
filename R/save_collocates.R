#' returns a list containing node locations, window locations, the node and its hash, and a doc_table
#'
#' @param document The document to be analysed
#' @param window The size of the collocate window on each side of tne node
#' @param node A key word or phrase
#' @param remove_stops If TRUE, stopwords are removed (stopwords derived from tidytext package)
#' @param remove_numerals If TRUE, numerals are removed
#' @param remove_punct If TRUE, puntuation is removed
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom digest sha1
#' @importFrom quanteda stopwords
#' @importFrom stringr str_replace_all
#' @keywords collocates kwic
#' @export

save_collocates <- function(document, window, node, remove_stops = TRUE, remove_numerals = TRUE, remove_punct = TRUE){
      node_length <- length(unlist(strsplit(node, " ")))
      # Test to see if the node phrase is larger than the window
      if(node_length > ((window*2)+1)){ # longer than twice the window plus the keyword
            stop("Error: the node phrase is longer than the kwic window")
      }
      ## print("L1")
      # Remove numerals
      if(remove_numerals == TRUE){
            document <- str_replace_all(document, "[0-9]", "")
      }
      if(remove_punct == TRUE){
            document <- str_replace_all(document, "[^[:alnum:]. ]", "")
      }
      #
      ## print("L2")
      #
      document <- tolower(document)
      ## print("L3")
      # Hash the node to to create a single phrase (and ensure stopwords contained in the
      # node aren't removed)
      node1 <- sha1(node)
      document <- gsub(x = document, pattern = paste("\\b", node, "\\b", sep = ""), replacement = node1)
      ## print("L4")
      # Unnest
      word.t <- tibble(document) %>%
            tidytext::unnest_tokens(word,
                          document,
                          token = "ngrams",
                          n = 1)
      ## print("L5")
      # If required remove stopwords
      if(remove_stops == TRUE){
            stops <- quanteda::stopwords("english")
            `%notin%` = function(x,y) !(x %in% y)
            word.t <- word.t %>% dplyr::filter(., word %notin% stops)
            #x <- which(word.t$word %notin% stop$word)
            #word.t <- word.t[x,]
      }
      ## print("L6")
      # Get locations of node
      node_loc <- which(word.t == node1)
      ## print("L7")
      # If there are no matches, just return a vector of NAs
      if(length(node_loc) == 0){
            collocate_locs <- list(rep(NA, times=(window)),
                                   rep(NA, times=(window)),
                                   node,
                                   node1,
                                   word.t)

      } else {
            left_locs <- lapply(node_loc, function(x)
                  ((x-window):(x-1)))
            right_locs <- lapply(node_loc,
                                 function(x)
                                       ((x+1):(x+window)))
            # convert any node_locs into NA
            left_locs <- lapply(left_locs,
                        function(x)
                              unlist(sapply(x,
                                            function(a)
                                                  ifelse(a %in% node_loc, yes = a <- NA, no = a <- a))))

            right_locs <- lapply(right_locs,
                        function(x)
                              unlist(sapply(x,
                                          function(a)
                                                ifelse(a %in% node_loc, yes = a <- NA, no = a <- a))))

            collocate_locs <- list(left_locs, right_locs, node, node1, length(node_loc), word.t)
      }
      ## print("L8")
      names(collocate_locs) <- c("left_locs", "right_locs", "node", "node_hash", "node_recurrence", "doc_table")
## print("L9")
      collocate_locs <- as(object = collocate_locs, Class = "collDB")

      return(collocate_locs)
}
