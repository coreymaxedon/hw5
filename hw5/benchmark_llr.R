library(microbenchmark)

bench <- microbenchmark(source("llr_functions.R"), times = 100)
print(bench)
