#' A stopwords script from tidytext (https://github.com/juliasilge/tidytext/blob/3006065b79ff4cb3f2c4edc37914176448105943/data-raw/setup_data.R)
#' 
#' @import dplyr
#' @import qdapDictionaries
#' @importFrom devtools use_data
#' @importFrom readr write_csv
#' 
stop_words <- function(){
SMART <- data_frame(word = tm::stopwords("SMART"), lexicon = "SMART")
snowball <- data_frame(word = tm::stopwords("en"), lexicon = "snowball")
onix <- data_frame(word = qdapDictionaries::OnixTxtRetToolkitSWL1, lexicon = "onix")

stop_words <- bind_rows(SMART, snowball, onix) %>%
      filter(!stringr::str_detect(word, "[^[:ascii:]]"))

readr::write_csv(stop_words, "data-raw/stop_words.csv")
devtools::use_data(stop_words, overwrite = TRUE)
}