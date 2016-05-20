#' Calculates a pointwise mutual information score for ngrams in a corpus: log2(prob(xy)/prob(x)prob(y))
#'
#' @param vect0 A vector of text files
#' @param keyword A keyword to feed to the kwic vector
#' @param window The width of the kwic vector
#' @keywords kwic corpus mutual information
#' @export

simple_kwics <- function(vect0, keyword, window){
  if(length(vect0 == 1)){
    vect0 <- unlist(strsplit(vect0, " "))
  }
  # Remove blank locates
  x <- which(vect0 == "")
  vect0 <- vect0[-x]
  # Get the keyword
  keyword_locs <- which(vect0 == keyword)
  # Get the lower and upper bounds to the window for each instance of the keyword
  lower_bound <- keyword_locs-window
  lower_bound <- ifelse(lower_bound < 1, NA, lower_bound)
  upper_bound <- keyword_locs+window
  upper_bound <- ifelse(upper_bound > length(vect0), NA, upper_bound)
  # create a list of ranges
  ranges_list <- lapply(seq_along(1:length(lower_bound)), function(x) lower_bound[x]:upper_bound[x])
  # Get rid of overlaps
  ranges_matrix0 <- do.call("rbind", ranges_list)
  # A function to find dups by comparing each row
  erase_dups <- function(vector1, vector2){
    truefalse_vector <- vector1 %in% vector2
    vector1[truefalse_vector == TRUE] <- NA
    return(vector1)
  }
  # Where are the overlaps?
  ranges_matrix <- t(sapply(seq_along(2:(nrow(ranges_matrix0))), function(x) erase_dups(ranges_matrix0[x,], ranges_matrix0[x+1,])))
  # Restore the first row
  ranges <- rbind(ranges_matrix[1,], ranges_matrix)
  #Isolate the words
  kwic_collocates <- matrix(vect0[ranges], ncol = ncol(ranges))
  return(kwic_collocates)
}