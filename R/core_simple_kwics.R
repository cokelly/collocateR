#' Calculates a pointwise mutual information score for ngrams in a corpus: log2(prob(xy)/prob(x)prob(y))
#'
#' @param vect A vector of text files
#' @param keyword A keyword to feed to the kwic vector
#' @param window The width of the kwic vector
#' @keywords kwic corpus mutual information
#' @export

simple_kwics <- function(vect, keyword, window, stem = FALSE){
      if(stem == TRUE){
            print("Stemming....")
            vect0 <- unlist(strsplit(vect, " "))
            vect <- wordstem(tokenize(vect0))
            x <- which(vect == "character(0)")
            if(length(x) > 0){
                  vect <- vect[-x]
            }
            vect <- paste(vect, sep = " ", collapse = " ")
            keyword <- wordstem(tokenize(keyword))
            keyword <- as.character(keyword)
            print("...done")
      }
      if(length(vect) == 1){
            vect <- unlist(strsplit(vect, " "))
            }
# Remove blank locates
x <- which(vect == "")
if(length(x) > 0){
      vect <- vect[-x]
}
# Get the keyword
print("Processing kwics...")
keyword_locs <- which(vect == keyword)
# Return NA if the keyword is not found otherwise continue processing
if(length(keyword_locs) == 0){
      sentences_vect <- NA
      } else {
            # Get the lower and upper bounds to the window for each instance of the keyword
            lower_bound <- keyword_locs-window
            # control for window falling below position 1
            lower_bound <- ifelse(lower_bound < 1, NA, lower_bound)
            lower_bound[which(is.na(lower_bound))] <- 1
            # get upper bound
            upper_bound <- keyword_locs+window
            # control for window exceeding wordcount
            upper_bound <- ifelse(upper_bound > length(vect), NA, upper_bound)
            upper_bound[which(is.na(upper_bound))] <- length(vect)
            # create a list of ranges
            ranges_list <- lapply(seq_along(1:length(lower_bound)), function(x)
                  lower_bound[x]:upper_bound[x])
            # Get rid of overlaps
            # Generate a vector of locations
            ranges_vector_with_dups <- do.call("c", ranges_list)
            # Get unique elements
            ranges_vector <- unique(ranges_vector_with_dups)
            # Find the points in the vector where elements skip (so one element is more than a single point from the previous)
            if(length(ranges_list) == 1){
                  sentences_vect <- vect[ranges_vector]
            } else {
            gaps_are_FALSE <- sapply(seq_along(length(ranges_vector):1), function(x) isTRUE(ranges_vector[x] - ranges_vector[x-1] == 1))
            isfalse <- which(gaps_are_FALSE == FALSE)
            # Mark these out with a pipe symbol
            ranges_vector[isfalse] <- paste("|", ranges_vector[isfalse], sep = " ")
            ranges_vector <- unlist(strsplit(ranges_vector, " "))
            gaps <- as.numeric(which(ranges_vector == "|"))
            # Generate a list of 'sentences' as in of sequential locations
            if(length(gaps) == 1){
                  text_locs_list <- lapply(ranges_vector, function(a) a[-gaps])
            } else {
            text_locs_list <- lapply(seq_along(1:(length(gaps)-1)), function(a) ranges_vector[(gaps[a]+1):(gaps[a+1]-1)])
      }
            # Add the final sentence in the vector.
            final_sentence <- ranges_vector[(max(gaps)+1):length(ranges_vector)]
            text_locs_list <- c(text_locs_list, list(final_sentence))
            # Turn the elements into numeric
            text_locs_list <- lapply(text_locs_list, as.numeric)
            # TUrn into a list of text elements
            text_list <- lapply(seq_along(1:length(text_locs_list)), function(x) vect[text_locs_list[[x]]])
            # Create sentences
            sentences_list <- lapply(seq_along(1:length(text_list)), function(a) paste(text_list[[a]], sep=" ", collapse = " "))
            # Unlist
            sentences_vect <- unlist(sentences_list)
            }
      }
return(sentences_vect)
}