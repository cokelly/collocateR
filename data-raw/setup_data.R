# note that in addition to the packages imported and suggested by tidytext,
# this script requires the devtools and qdapDictionaries packages
# The stop_words function is drawn from tidytext @ https://github.com/juliasilge/tidytext

library(dplyr)
library(qdapDictionaries)
library(devtools)

SMART <- data_frame(word = tm::stopwords("SMART"), lexicon = "SMART")
snowball <- data_frame(word = tm::stopwords("en"), lexicon = "snowball")
onix <- data_frame(word = qdapDictionaries::OnixTxtRetToolkitSWL1, lexicon = "onix")

stop_words <- bind_rows(SMART, snowball, onix) %>%
      filter(!stringr::str_detect(word, "[^[:ascii:]]"))

readr::write_csv(stop_words, "data-raw/stop_words.csv")
devtools::use_data(stop_words, overwrite = TRUE)