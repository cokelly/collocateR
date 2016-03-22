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

The collocate function produces the basic collocation matrices.

```
dickens_collocates <- collocate(dickens, "poor", collocation_width = 4, tidy=TRUE)
```

### Tidy

The `tidy` function is simply a shortcut to `tm` functions. It

1. Removes punctuation and numbers
2. Converts to lowercase
3. Strips white spaces
4. Removes empty lines


This is an early draft: statistical significance tests are to be included asap.



<!-- footnote -->

<b id="f1">1</b> Ingo Feinerer, Kurt Hornik, and David Meyer (2008). Text Mining Infrastructure in R. _ Journal of
  Statistical Software_ 25(5): 1-54. URL: http://www.jstatsoft.org/v25/i05/  [↩](#a1)

<!-- footnote ends -->
