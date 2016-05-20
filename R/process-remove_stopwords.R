#' Removes stopwords from a vector of text files, using quanteda's stopwords
#'
#' @param vectors A vector of text files
#' @import quanteda
#' @keywords tidy

remove_stops <- function(vectors){
      print("removing stopwords...")
      # a vector of stopwords
      stops <- stopwords("english")
      
      vectors <- as.list(vectors)
      
      # strsplit each vector
      vectors <- lapply(vectors, function(x) unlist(strsplit(x, " ")))
      # Identify the locations of stopwords
      stoplocs <- lapply(vectors, function(x) which(x %in% stops))
      # A small function for removing stopwords
      removestops <- function(vector, stops){
            vector <- vector[-stops]
      }
      vectors <- lapply(seq_along(1:length(vectors)), function(x) removestops(vectors[[x]], stoplocs[[x]]))
      
      # Return the files
      vectors <- lapply(vectors, function(x) paste(x, sep = "", collapse = " "))
      vectors <- unlist(vectors)
      return(vectors)
}