#' collocateR: a package that calculates significance measures for collocates
#' 
#' This package acts as an effective plugin for the Quanteda and Tidytext packages 
#' It applies statistical measures to collocates, generated as keywords in context
#'
#' @docType package
#' @name collocateR
#' @examples
#' # Example usage:
#' library(collocateR)
NULL


## quiets concerns of R CMD check re: the .'s that appear in pipelines with Dplyr, plus other internal variables
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",
                                                        "all_locs0",
                                                        "all_locs00",
                                                        "Collocate Frequency",
                                                        "collocation",
                                                        "docname",
                                                        "Document Frequency",
                                                        "expected",
                                                        "from",
                                                        "full_window",
                                                        "full_window0",
                                                        "left_locs",
                                                        "post",
                                                        "pre",
                                                        "prob",
                                                        "probx",
                                                        "probxy",
                                                        "proby",
                                                        "right_locs",
                                                        "to",
                                                        "value",
                                                        "word",
                                                        "z score"))