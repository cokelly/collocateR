#' pointwise mutual information: internal function (for use in pmi and npmi functions)
#' 
#' @param document A collDB, produced through save_collocates, or a text file
#' @param freqs collocate and document frequencies
#' @import tibble dplyr
#' @keywords mutual information, collocates, kwic

get_pmi <- function(document, freqs){
      pmi <- tibble(phrase = freqs$word, 
                    collocate_freqs = as.numeric(freqs$coll_freq), 
                    doc_freqs = as.numeric(freqs$doc_freq),
                    probx = as.numeric(document$node_recurrence/nrow(document$doc_table)),
                    proby = as.numeric(doc_freqs/nrow(document$doc_table)),
                    probxy = as.numeric(collocate_freqs/nrow(document$doc_table)),
                    pmi = as.numeric(log(probxy/(probx*proby))))
return(pmi)
}