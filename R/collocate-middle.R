#' A function that isolates keywords and neighbours
#' 
#' Addresses words that are less than a collocation_width away from the document's end
#' @param collocation_width The collocation width
#' @param input_vector The vector that is being processed
#' @param keyword The relevant keyword
#' @param keyword_locations Keyword locations within the doc
#' @keywords collocates concordance

normal_collocates <- function(collocation_width, input_vector, keyword_locations, 
    keyword) {
    
    # Get words preceding the keyword
    before <- matrix(ncol = collocation_width)
    before <- sapply(keyword_locations, function(x) matrix(input_vector[(x) - 
        (seq(1, collocation_width))]))
    before <- as.matrix(before[seq(collocation_width, 1), ])
    before <- t(before)
    # Get words after the keyword
    after <- sapply(keyword_locations, function(i) matrix(input_vector[i + 
        (seq(1, collocation_width))]))
    after <- t(after)
    # Assemble the matrix
    collocates.df <- matrix(paste(keyword), ncol = 1, nrow = length(keyword_locations))
    collocates.df <- cbind(before, collocates.df, after)
    
    return(collocates.df)
} 
