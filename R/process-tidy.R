#' Tidies a vector of text files
#'
#' @param vector A vector of text files
#' @importFrom stringr str_replace_all
#' @keywords tidy

tidy <- function(vector){

print("preparing to tidy...")
vectors <- lapply(vector, function(x) as.matrix(unlist(strsplit(x, " "))))

# remove any \n
print("removing incorrectly coded carriage returns...")
vectors <- lapply(vectors, function(x) str_replace_all(x, "\n", " "))

  # To lower
print("to lower...")
vectors <- lapply(vectors, function(x) as.matrix(tolower(x)))

 # Remove all punctuation
print("removing punctuation...")
vectors <- lapply(vectors, function(x) str_replace_all(x,"[[:punct:]]",""))

print("removing empty lines...")
remove_empty_lines <- function(vector){
  vector <- as.matrix(vector)
  empty_lines <- which(vector == "") # get rid of empty lines
  vector <- as.matrix(vector[-empty_lines,])
  vector <- as.character(vector)

  return(vector)
}

vectors <- lapply(vectors, function(x) remove_empty_lines(x))

# Generate character vector
vectors <- lapply(vectors, function(x) paste(x, sep = "", collapse = " "))

vectors <- unlist(vectors)
return(vectors)
}
