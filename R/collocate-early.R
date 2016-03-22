#' A function that tackles early words
#' 
#' Addresses words that are less than a collocation_width away from the document's beginning
#' @param collocation_width The collocation width
#' @param early The early word's location
#' @param input_vector The vector that is being processed
#' @param keyword The relevant keyword
#' @param keyword_locations Keyword locations within the doc
#' @keywords collocates concordance

early_collocates <- function(collocation_width, early, input_vector, keyword, keyword_locations){
    # This function isolates the earliest word(s) and treats the remainder as
    # normal.  Early word specify the early word location(s)
    early2 <- keyword_locations[early]
    early.df <- matrix(ncol = collocation_width)
    if (length(early2) == 1) {
        keywords_to_fill <- (1:(early2 - 1))  # the location(s) in the input vector
        early.df[, (collocation_width - (max(keywords_to_fill) - 1)):collocation_width] <- input_vector[keywords_to_fill]
    } else {
        keywords_to_fill0 <- sapply(early2, function(x) 1:x)
        early_function <- function(keywords_to_fill0) {
            early.df[, (collocation_width - (max(keywords_to_fill0) - 1)):collocation_width] <- input_vector[keywords_to_fill0]
            return(early.df)
        }
        early.df <- lapply(keywords_to_fill0, early_function)
        early.df <- matrix(unlist(early.df), ncol = collocation_width, byrow = TRUE)
    }
    
    
    earlyafter <- sapply(early2, function(x) matrix(input_vector[x + (seq(1, 
        collocation_width))]))  # get collocation width of words after the early keyword
    earlyafter <- t(earlyafter)
    # Assemble the matrix
    earlycollocates.df <- matrix(paste(keyword), ncol = 1, nrow = length(early2))  # paste into a full matrix
    earlycollocates.df <- cbind(early.df, earlycollocates.df, earlyafter)
    
    # return early collocations df
    return(earlycollocates.df)
    
} 
