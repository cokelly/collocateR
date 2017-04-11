# CollocateR

CollocateR is a package for the statistical programming language [R](https://www.r-project.org/).
Albeit imperfectly, the package increasingly uses functions and workflows from the [tidyverse](http://tidyverse.org/) and [tidytext](http://tidytextmining.com/) packages.

## Purpose

CollocateR serves a simple purpose. It processes collocates for keywords in context in text files and calculates significance for them, based on tests set out in Barnbrook et al's [_Collocation: Applications and Implications_](https://www.palgrave.com/gb/book/9781403946126), Palgrave 2013, and formulae explained in the [British National Corpus](http://rdues.bcu.ac.uk/bncweb/manual/bncwebman-collocation.htm) home.

## Functions

- **save_collocates**: Return a list containing a tokenised version of the original document, a record of the node in original and hashed format, lists of left and right collocate locations, and document word_length.
- **pmi**: a 'pointwise mutual information' significance test based on the probability of nodes and collocates occurring together compared to the probability of their occurring independently. This is calculated as: <img src="https://rawgit.com/in	git@github.com:cokelly/collocateR/None/svgs/46a5443dfbf0abe15dc3d4478ed8253d.svg?invert_in_darkmode" align=middle width=146.96418pt height=33.14091pt/>.
- **npmi**: as above, but normalised so all results occur between 1 (perfect collocation) and -1 (the terms never collocate).

## TODO

- MI3
- z-score
- observed_expected
- log_log
- log_likelihood
