# Atomic vector representing bytes, converted to vector of bools
str_to_bool <- function(string) {
  vec <- strsplit(string, "")[[1]]
  vec == "1"
}

# ...Opposite direction
bool_to_str <- function(bool) {
  paste0(ifelse(bool, "1", "0"), collapse = "")
}

read_cmp_file <- function(path) {
  x <- read.table(here(path), sep = "|", header = TRUE, colClasses = "character")
  x <- x[2:(ncol(x) - 1)]
  x %>% mutate(across(everything(), ~ str_trim(.x, "both")))
}
