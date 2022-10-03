library(dplyr)
library(tidyr)
library(stringi)
library(stringr)

generate_nas <- function (n, pct) {
  data.frame(X = sample.int(1000, n, replace = TRUE),
             Y = sample.int(1000, n, replace = TRUE),
             Z = sample.int(1000, n, replace = TRUE)) %>%
    mutate(across(.cols = "X",
                  .fns = ~ ifelse(row_number(.x) %in% sample(1:n(), size = ((pct * 100) * n() / 100)), NA, .x)
    ))
}

# replace_na directly is much faster
results <- bench::press(
  n = c(100, 1000, 10000),
  pct = c(0.05, 0.1, 0.25, 0.5),
  {
    dat <- generate_nas(n, pct)
    bench::mark(
      replace_na = dat %>% tidyr::replace_na(list(X = 0)),
      mutate = dat %>% mutate(X = tidyr::replace_na(X, 0)),
      iterations = 1000,
      check = TRUE)
  }
)


x <- stri_flatten(stri_rand_strings(100000, 3, "[A-Z]"), ",")

# stri_count(_fixed) is much faster
bench::mark(
  str_count = str_count(x, "AAA"),
  stri_count = stri_count(x, fixed = "AAA"),
  stri_count_fixed = stri_count_fixed(x, "AAA"),
  iterations = 1000,
  check = TRUE
)

# A tibble: 3 x 13
#expression            min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result    memory              time               gc
#<bch:expr>       <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list>    <list>              <list>             <list>
#1 str_count         831.8us    999us      930.    16.3KB        0  1000     0      1.07s <int [1]> <Rprofmem [23 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>
#2 stri_count         34.6us   36.9us    25603.        0B        0  1000     0    39.06ms <int [1]> <Rprofmem [0 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>
#3 stri_count_fixed   30.7us   34.4us    24735.        0B        0  1000     0    40.43ms <int [1]> <Rprofmem [0 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>


x <- stri_rand_strings(100000, 3, "[A-Z]")

# which is slightly faster than vector, but much faster than rle
bench::mark(
  vector = length(x[x == "AAA"]),
  rle = sum(rle(x == "AAA")$values),
  which = length(which(x == "AAA")),
  iterations = 1000,
  check = TRUE
)

# A tibble: 3 x 13
#expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result    memory              time               gc
#<bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list>    <list>              <list>             <list>
#1 vector      506.6us  766.2us     1048.  781.35KB     25.8   976    24   931.39ms <int [1]> <Rprofmem [2 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>
#2 rle          1.76ms    3.5ms      292.    4.96MB     63.3   822   178      2.81s <int [1]> <Rprofmem [13 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>
#3 which       501.8us  715.3us     1218.  781.35KB     31.2   975    25    800.2ms <int [1]> <Rprofmem [2 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>