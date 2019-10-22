library(microbenchmark)

microbenchmark(source("llr_functions.R"), times = 10)


Unit: milliseconds
expr      min       lq     mean   median       uq      max neval
source("llr_functions.R") 493.8058 642.8336 788.2934 713.6931 915.1111 1347.226   100