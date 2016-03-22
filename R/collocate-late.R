#' A function that tackles late words
#' 
#' Addresses words that are less than a collocation_width away from the document's end
#' @param collocation_width The collocation width
#' @param doc_length The document's word count
#' @param input_vector The vector that is being processed
#' @param keyword The relevant keyword
#' @param keyword_locations Keyword locations within the doc
#' @param late The late word's location
#' @keywords collocates concordance

late_collocates <- function(collocation_width, doc_length, input_vector, keyword,keyword_locations, late){
    
    late2 <- keyword_locations[late]  # location of late word(s)
    
    # number of empty columns number_full_cols <- sapply(late2, function(x)
    # doc_length-x) which columns will receive words? full_cols <-
    # sapply(number_full_cols, function(x) seq(1, x[1:length(x)]))
    
    keywords_to_fill <- sapply(late2, function(x) seq((x[1:length(x)] + 1), 
        doc_length))
    
    if (length(late2) > 1) {
        late.df <- matrix(ncol = collocation_width)
        late_words_function <- function(keywords_to_fill0) {
            late.df[, 1:length(keywords_to_fill0)] <- input_vector[keywords_to_fill0]
            return(late.df)
        }
        late.df0 <- lapply(keywords_to_fill, function(x) late_words_function(x))
        late.df <- matrix(unlist(late.df0), nrow = length(late.df0), byrow = TRUE)
    } else {
        keywords_to_fill <- t(keywords_to_fill)
        late.df <- matrix(ncol = collocation_width)
        late.df[, 1:length(keywords_to_fill)] <- input_vector[keywords_to_fill]
    }
    # Get the words before
    latebefore0 <- sapply(late2, function(x) matrix(input_vector[seq(x - (collocation_width), 
        x - 1)]))
    latebefore <- t(latebefore0)
    
    # bind into a full matrix
    
    latecollocates.df <- matrix(paste(keyword), ncol = 1, nrow = length(late2))  # paste into a full matrix
    latecollocates.df <- cbind(latebefore, latecollocates.df, late.df)
    
} 
