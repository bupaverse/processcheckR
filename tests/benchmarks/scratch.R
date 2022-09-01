library(dplyr)
library(tidyr)

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