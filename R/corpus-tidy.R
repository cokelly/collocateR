#' Uses tm to tidy a corpus
#'
#' Addresses words that are less than a collocation_width away from the document's end
#' @param corpus_raw A corpus that has not been processed
#' @keywords collocates concordance
#' @export

tidy <- function(corpus_raw) {

    corpus_tidied <- tm_map(corpus_raw, content_transformer(removePunctuation))
    corpus_tidied <- tm_map(corpus_tidied, content_transformer(tolower))
    corpus_tidied <- tm_map(corpus_tidied, content_transformer(removeNumbers))
    corpus_tidied <- tm_map(corpus_tidied, content_transformer(stripWhitespace))

    #########################

corpus_tidied <- tm_map(corpus_tidied, content_transformer(remove_empty_lines))

    #########################

    return(corpus_tidied)
}
