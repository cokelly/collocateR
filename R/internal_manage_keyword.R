#' pointwise mutual information: internal function (for use in pmi and npmi functions)
#' 
#' @param doc Acharacter vector or list of character vectors
#' @param keyword A key word or phrase to test
#' @importFrom stringr str_split str_replace_all
#' @importFrom stringi stri_rand_strings
#' @keywords mutual information, collocates, kwic

internal_manage_keyword <- function(doc, keyword){

		original_keyword <- keyword
        collapsed_keyword <- paste(unlist(str_split(keyword, " ")), sep = "", collapse = "")
        keyword <- c(paste(collapsed_keyword, stringi::stri_rand_strings(1, 10, pattern = "[a-z]"), sep = "", collapse = ""))
        doc2 <- str_replace_all(doc, original_keyword, keyword)

        return(list(doc2, keyword, original_keyword))
}
