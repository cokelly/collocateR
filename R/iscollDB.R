#' internal function to test whether an object is a collDB
#' 
#' @param document The document to be analysed
#' @export


is.collDB <- function(document){
      if(class(document) == "collDB"){
            return(TRUE)
      } else {
            return(FALSE)
      }
}