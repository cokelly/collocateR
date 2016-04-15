#' A keywords in context function
#'
#' Takes a tm corpus as input, returns a list of matrices containing Keywords in Context as output.
#' @param corpus A tm corpus
#' @param keyword The keyword that you wish to find collocates for
#' @param collocation_width The number of neighbouring words on each side
#' @param tidy Tidy the corpus - remove punctuation, numbers, empty lines and whitespace, convert to lowercase
#' @param exact FALSE to use 'grep' to find all words containg your keyword phrase or TRUE to use 'which' to find the exact keyword (default: TRUE (is much faster))
#' @import tm
#' @keywords collocates concordance
#' @export
#' @examples
#' library(tm)
#' data("crude")
#' kwic(crude,"oil", collocation_width = 6, tidy = TRUE)


kwic <- function(corpus, keyword, collocation_width = 4, tidy = FALSE, exact = TRUE){
      
      # preserve corpus names
cnames <- names(corpus)
      # If I need to tidy (recommend it's done before)      
if (tidy == TRUE) {
      corpus <- tidy(corpus)
}
      # Split the corpus into vectors
vectors <- sapply(corpus, "content")
      # Strsplit to separate each word out
vectors <- lapply(vectors, function(x) unlist(strsplit(x, split = " ")))
      # Get locations for keywords
if(exact == FALSE){
keywordlocs <- lapply(vectors, function(x)
      grep(keyword, x))
            } else {
if(exact == TRUE){
keywordlocs <- lapply(vectors, function(x) 
      which(x == keyword))
            } else {
print("'exact' must be TRUE or FALSE")}}
      # Get prior locations to collocation width
prior_words <- lapply(keywordlocs, function(y)
      sapply(y, function(x) 
            x-(rev(seq_along(1:collocation_width)))))
      # Get subsequent locations to collocation width      
subsequent_words <- lapply(keywordlocs, function(y)
      sapply(y, function(x)
            x+(seq_along(1:collocation_width))))
      # Combine to create a matrix of locations matrices
full_matrices <- lapply(seq_along(1:length(keywordlocs)), function(x) do.call(
            rbind,
            list(prior_words[[x]],
                 keywordlocs[[x]],
                 subsequent_words[[x]]
                 )))
######################################
      # A function to replace negative numbers with NA
######################################
replace_minuses <- function(matrices){
      matrices[matrices<1] <- NA
      return(matrices)
}
######################################
full_matrices <- lapply(full_matrices, function(x)
      replace_minuses(x))
######################################
      # isolate the finished matrices (and transpose)
######################################
allocate_finished_matrices <- function(vect, full_matrix){
if(ncol(full_matrix) >= 1){
      finished <- matrix(vect[full_matrix], ncol = ncol(full_matrix))
      finished <- t(finished)
      #Allocate names to each column
seq_colls <- seq_along(1:collocation_width)
      vector_names_final <- c(
      paste((rev(seq_colls)), "L", sep = ""),
      paste("keyword"),
      paste(seq_colls, "R", sep = "")
      )
colnames(finished) <- vector_names_final
      }  else {
finished <- "The keyword was not found in this document"}
      return(finished)
}
######################################
      # Allocate finished matrices      
final_list <- lapply(seq_along(1:length(vectors)), function(x) allocate_finished_matrices(vectors[[x]], full_matrices[[x]]))
      # Apply corpus names
names(final_list) <- cnames
           
return(final_list)
}