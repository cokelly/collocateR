[![Build Status](https://travis-ci.org/cokelly/collocateR.svg?branch=master)](https://travis-ci.org/cokelly/collocateR)  [![DOI](https://zenodo.org/badge/54512562.svg)](https://zenodo.org/badge/latestdoi/54512562)

# CollocateR

CollocateR is a package for the statistical programming language [R](https://www.r-project.org/).
Albeit imperfectly, the package increasingly uses functions and workflows from the [tidyverse](http://tidyverse.org/) and [tidytext](http://tidytextmining.com/) packages.

## Purpose

CollocateR serves a simple purpose. It processes collocates for keywords in context in text files and calculates significance for them, based on tests set out in Barnbrook et al's [_Collocation: Applications and Implications_](https://www.palgrave.com/gb/book/9781403946126), Palgrave 2013, and formulae explained in the [British National Corpus](http://rdues.bcu.ac.uk/bncweb/manual/bncwebman-collocation.htm) home.

## Functions

~~- **save_collocates**: Return a list containing a tokenised version of the original document, a record of the node in original and hashed format, lists of left and right collocate locations, and document word_length.~~
- **get_freqs**: A frequency count for collocates, both in context and in the document in general
- **pmi**: a 'pointwise mutual information' significance test based on the probability of nodes and collocates occurring together compared to the probability of their occurring independently.
- **npmi**: as above, but normalised so all results occur between 1 (perfect collocation) and -1 (the terms never collocate).
- **z-score**: a probability test comparing probability of collocate occurring in near the node versus its occurrence across the text

## TODO

- [x] save_collocates
- [x] pmi
- [x] npmi
- [x] z-score
- [x] MI Cubed
- [x] log_log
- [x] log_likelihood
- [ ] Import other elements


### Acknowledgement

README generated with [readme2tex](https://github.com/leegao/readme2tex).
