#' function to convert a list into a collDB
#' 
#' @param document The document to be analysed
#' @export


as.collDB <- function(document){
      if(document == list &&
         length(document == 7 &&)
         class(document[[1]]) == list && 
         class(document[[2]]) == list &&
         class(document[[3]]) == character &&
         class(document[[4]]) == character &&
         class(document[[5]]) == integer &&
         class(document[[6]]) == tbl_df &&
         class(document[[7]]) == list){
document <- as(object = document, Class = "collDB")
return(document)
      } else {
      stop("This obect cannot be converted into a collDB")
      }
}