input <- readr::read_lines(here::here("day3/input.txt")) |>
  strsplit("") |>
  lapply(as.numeric)

## Part 1
find_max_joltage <- function(x) {
  n <- length(x)
  best <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      val <- 10 * x[i] + x[j]
      if (val > best) best <- val
    }
  }
  best
}
vapply(input, find_max_joltage, FUN.VALUE = numeric(1)) |>
  sum() |>
  print()

## Part 2

get_max <- function(x, n, j) {
  m <- max(x[1:(n - j)])
  w <- which.max(x[1:(n - j)])
  return(
    list(m, x[(w + 1):n])
  )
}

find_max_joltage2 <- function(x) {
  jolts <- integer()
  while (length(jolts) < 12) {
    n <- length(x)
    t <- get_max(x, n, (11 - length(jolts)))
    x <- t[[2]]
    jolts <- c(jolts, t[[1]])
  }
  jolts <- jolts * (10^(rev(seq_along(jolts)) - 1))
  return(sum(jolts))
}
vapply(input, find_max_joltage2, FUN.VALUE = numeric(1)) |>
  sum() |>
  print()
