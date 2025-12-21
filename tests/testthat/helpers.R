rand_bool <- function(size) {
  sample(c(TRUE, FALSE), size, replace = TRUE)
}

expect_all_true <- function(code) {
  results <- sapply(1:100, function(x) force(code))
  expect_true(all(results))
}
