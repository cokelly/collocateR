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
	"E1",
	"E2",
	"d",
	"doc_count",
	"docname",
	"expected",
	"from",
	"full_sentence",
	"is_character",
	"keyword_count",
	"keyword_loc",
	"kwic_count",
	"log-log",
	"post",
	"post_list",
	"pre",
	"pre_list",
	"prob",
	"probx",
	"probxproby",
	"probxy",
	"proby",
	"span",
	"str_count",
	"top",
	"word",
	"wordcount",
	"z_score",
	"z score"))