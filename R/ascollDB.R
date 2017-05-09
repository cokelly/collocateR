#' function to convert a list into a collDB
#' 
#' @param document The document to be analysed
#' @importFrom tibble is.tibble
#' @export


as.collDB <- function(document){
      if(is.list(document) &&
         length(document) == 7 &&
         is.list(document[[1]]) && 
         is.list(document[[2]]) &&
         is.character(document[[3]]) &&
         is.character(document[[4]]) &&
         is.integer(document[[5]]) &&
         is.tibble(document[[6]]) &&
         is.list(document[[7]])){
document <- as(object = document, Class = "collDB")
return(document)
      } else {
      stop("This obect cannot be converted into a collDB")
      }
}