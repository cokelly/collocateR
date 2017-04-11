#' a class containing a list of collocate information, derived from the 'save_collocates' function
#' @import tibble
#' @exportClass collDB
 

collDB <- setOldClass(c("tbl_df", "tbl", "data.frame"))

collDB <- setClass("collDB", 
                   slots = c(left_locs = "list", 
                             right_locs = "list", 
                             node = "character", 
                             node_hash = "character", 
                             node_recurrence = "numeric",
                             doc_table = "tbl_df"), 
                   contains = "list")