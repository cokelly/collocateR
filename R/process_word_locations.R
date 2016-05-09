#' Turn a text vector into a data frame of keyword locations
#'
#' Turn a text vector into a data frame with three columns: 1. the filename or assigned name; 2. the word location; 3. keyword match (0 for does not match, 1 for match)
#' @param vect A text vector
#' @param keyword A keyword
#' @param A vectorname The Vector name or an assigned vectorname
#' @keywords word locations

word_locations <- function(vect, keyword, vectorname){
      # Split the vector into words
      vect <- unlist(strsplit(vect, split = " "))
      # find the keyword, replace keyword locations with 1 and all other locations with 0
      keyword_locs <- which(vect == keyword)
      not_keyword_locs <- which(vect != keyword)
      vect[keyword_locs] <- 1
      vect[not_keyword_locs] <- 0
      # Create a vectorname that includes n for words
      vectorname <- paste(vectorname, "\n", "n = (", length(keyword_locs), ")", sep = "")
      #Turn into a data frame
      vect <- as.integer(vect)
      positions <- seq_along(1:length(vect))
      vect.df <- data.frame(cbind(rep(vectorname, length(vect)), positions, vect), stringsAsFactors = FALSE)
      colnames(vect.df) <- c("file", "word", "matchwords")
      
      return(vect.df)
}