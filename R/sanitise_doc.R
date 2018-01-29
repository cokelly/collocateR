#' Remove certain puntuations (apostrophes etc) and then retain only letters and spaces
#' 
#' @param doc A character vector
#' @importFrom stringr str_replace_all
#' @importFrom stringi stri_trans_general
#' @keywords tidy sanitise

# Tidy and lemmatise
sanitise_doc <- function(doc){
      # Remove curly quotes etc
      doc <- stringi::stri_trans_general(doc, "latin-ascii")
      # Try to deal with crappy word fragments and apostrophes
      doc <- stringr::str_replace_all(doc, "[\r\n]", " ")
      doc <- stringr::str_replace_all(doc, "-", " ")
      doc <- stringr::str_replace_all(doc, "'s", "s")
      doc <- stringr::str_replace_all(doc, "’s", "s")
      doc <- stringr::str_replace_all(doc, "'t", "t")
      doc <- stringr::str_replace_all(doc, "’t", "t")
      doc <- stringr::str_replace_all(doc, "[^[:alpha:] ]", " ")
      return(doc)
}