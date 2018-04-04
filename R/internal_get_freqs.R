#' Extract frequencies for the keywords in context and the full document
#' 
#' @param doc A character vector or list of character vectors
#' @param keyword A key word or phrase to test
#' @param window The number of context words to be displayed around the keyword Default 6
#' @param ngram The size of phrases the frequencies of which we are to test (so, unigram = 1, bigram = 2, trigram = 3 etc) 
#' @param min_count Collocates that occur fewer times than floor will be removed
#' @param cache Getting frequencies is the most time-consuming step in calculating frequencies and other collocation algorithms. The memoise package is used to cache specific iterations of this process. Default FALSE.
#' @include remove_duplicates.R
#' @import tibble magrittr dplyr quanteda
#' @importFrom stringr str_split str_detect str_replace
#' @importFrom quanteda stopwords
#' @importFrom purrr is_character
#' @keywords mutual information, collocates, kwic
#' @export



internal_get_freqs <- function(doc, keyword, window = window, ngram = ngram, min_count = min_count, cache = cache){
      
      # start off by sanitising the document
      doc <- sanitise_doc(doc)
      
    # If ngrams are smaller than the keyword size, swap out the keyword for the moment.
    if(ngram < length(unlist(str_split(keyword, " ")))){
        managed_keyword <- internal_manage_keyword(doc, keyword)
        doc <- managed_keyword[[1]]
        keyword <- managed_keyword[[2]]
        original_keyword <- managed_keyword[[3]]
    }
    
      # If the doc is a list, unlist it.
      # If it is not a character vector, stop.
      if(is.list(doc) == TRUE){
            doc <- unlist(doc)
      }
      if(is_character(doc) != TRUE){
            stop("collocateR will only act on character vectors (for now).")
      }
      if(length(doc) > 1 && unique(map(doc, length)) == 1){
            doc <- paste(doc, sep = " ", collapse = " ")
      }
      
      # Sanity check that the window is a double
window <- as.double(window)
      # ...and that the keyword exists
if(!exists("keyword")){stop("Please supply a keyword")}

if(length(unlist(str_split(keyword, " "))) == 1){
kwics<- doc %>% unlist %>%
      quanteda::kwic(., pattern = keyword, window = window, valuetype = "fixed")
} else {
      kwics<- doc %>% unlist %>%
            quanteda::kwic(., pattern = phrase(keyword), window = window, valuetype = "fixed")
}
kwics <- as.matrix(kwics)
# Best moment to get the measure below. Then return it for other functions.
keyword_recurrence <- nrow(kwics)
      
if(isTRUE(keyword_recurrence == 0)){stop(print(paste("No collocates for the phrase", keyword, "were found in this document.\nIf you lemmatised the document, be sure to lemmatise the keyword", sep = " ")))}

kwics_processed <- kwics %>% remove_duplicates(., keyword, window)

sentence_end_marker <- kwics_processed[[1]]
kwics2 <- kwics_processed[[2]]

kwics_string <- paste(kwics2$word, sep = " ", collapse = " ")

if(ngram == 1){
      collocates <- kwics_string %>% as_tibble %>% unnest_tokens(., ngram, value) %>% group_by(ngram) %>% summarise(`Collocate Frequency` = n()) %>% filter(`Collocate Frequency` >= min_count) %>% arrange(desc(`Collocate Frequency`))
} else {
      if(ngram > 1){
      collocates <- kwics_string %>% quanteda::textstat_collocations(., size = ngram, min_count = min_count) %>% as_tibble %>% select(ngram = collocation, `Collocate Frequency` = count) %>% arrange(desc(`Collocate Frequency`))
      } else {
      stop("Ngrams must be a positive number.")
      }
}
# Remove rows containing sentence end or beginning boundaries. 
boundaries <- str_detect(collocates$ngram, sentence_end_marker)
collocates <- collocates[which(boundaries == FALSE),]

# Get the corresponding frequencies for the whole document
# First cut out useless words from the docs
docs_freqs <- x <- doc %>% as_tibble %>% unnest_tokens(., ngram, value, token = "ngrams", n = ngram) %>% filter(ngram %in% collocates$ngram) %>% group_by(ngram) %>% summarise(`Document Frequency` = n()) %>% ungroup

collocates <- full_join(collocates, docs_freqs, by = "ngram")

# # If ngrams are smaller than the keyword size, return the keyword to the original.
if(exists("original_keyword")){
     collocates <- collocates %>%
         mutate(ngram = case_when(ngram == keyword ~ str_replace(ngram, keyword, original_keyword), ngram != keyword ~ ngram))
}


return(list(collocates, keyword_recurrence))
}