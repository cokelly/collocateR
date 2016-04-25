# CollocateR

CollocateR is a package designed to generate collocates for a keyword in a corpus. Taking a corpus generated through the [tm](https://r-forge.r-project.org/projects/tm/) package,<sup id="a1">[1](#f1)</sup> CollocateR generates a list of matrices with the keyword in the middle column and neighbouring words in columns to each side. So, for instance, the first ten collocates for 'poor' in Charles Dickens's *Bleak House* are set out as follows:

|     4L     |  3L   |    2L    | 1L  | keyword |    1R    |     2R     |   3R    |       4R       |
|:----------:|:-----:|:--------:|:---:|:-------:|:--------:|:----------:|:-------:|:--------------:|
|   woman    |   i   |   felt   | so  |  poor   |    so    |  trifling  |   and   |       so       |
|    the     |  day  |  after   | my  |  poor   |   good   | godmother  |   was   |     buried     |
| unexpected | sight |    of    | the |  poor   | children |  outside   | waving  |     their      |
|    his     | best  | appeared | so  |  poor   |    a     | substitute |   for   |      the       |
|    said    |  to   |  humour  | the |  poor   |   old    |    lady    |  that   |       we       |
|     no     | means |   said   | the |  poor   |   old    |    lady    | keeping |       up       |
|    the     | area  | railings | oh  |  poor   |  child   |    said    |    i    |      let       |
|     my     |  way  |    to    | the |  poor   |  child   |    who     |   was   |      one       |
|  presence  |  one  |    of    | the |  poor   |  little  |   things   |  fell   | downstairsdown |
|  hearing   |  we   | followed | the |  poor   |  child   |    who     |   had   |    tumbled     |

## Preparing the corpus

Import a series of texts using the 'tm' package
```
library(tm)
dickens <- Corpus(DirSource("/Dickens Texts/"), readerControl = list(language = "en"))
```

## Collocate

The kwic (Keyword in Context) function produces the basic collocation matrices.

```
dickens_kwic <- kwic(dickens, "poor", collocation_width = 4, tidy=TRUE)
```

## Tidy

The `tidy` function is simply a shortcut to `tm` functions. It

1. Removes punctuation and numbers
2. Converts to lowercase
3. Strips white spaces
4. Removes empty lines

Although it is much slower, I strongly recommend running 'tidy' on the corpus. The function is separated out in case you wish to make additional adjustments to the corpus between tidying and running the collocate.

## Significance testing

The package includes a basic significance test, measuring each collocated word's occurrence against the word's occurrence in the document as a whole. The function requires the tidied original corpus and a kwic matrix as calculated above.

```
dickens_significance <- significance(dickens, dickens_kwic, tidy = TRUE, threshold = 3)
```

This test is not good with low-occurrence words and so a threshold can be set. The threshold default is 3 (inclusive).

## Todo

This is an early draft: statistical significance tests, largely drawn from Barnbrook et al 2013<sup id="a2">[1](#f2)</sup> are to be included asap. These will include:

1. t-score
2. z-score
3. mutual information
4. tf-idf (both calculating collocate matrix to document and using tm's functions to calculate from corpus as a whole)



<!-- footnote -->

<b id="f1">1</b> Ingo Feinerer, Kurt Hornik, and David Meyer (2008). Text Mining Infrastructure in R. _Journal of
  Statistical Software_ 25(5): 1-54. URL: http://www.jstatsoft.org/v25/i05/  [↩](#a1)

 <b id="f2">2</b> Barnbrook Geoff and others, _Collocation: Applications and Implications_ (Palgrave Macmillan 2013). [↩](#a2)


<!-- footnote ends -->
