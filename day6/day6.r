options(scipen = 999)

# Prepping Input
input <- readr::read_lines(here::here("day6/input.txt")) |>
  strsplit(" ") |>
  lapply(\(x) x[x != ""])

# Math Function
cephalopod_math <- function(x) {
  op <- ifelse(x[length(x)] == "*", `prod`, `sum`)
  x <- x[-length(x)] |> as.numeric()
  return(op(x))
}


## Part 1
exprs <- lapply(seq_len(length(input[[1]])), \(i) {
  expr <- lapply(input, \(x) x[i]) |> unlist()
  return(expr)
})

part1 <- vapply(
  exprs,
  cephalopod_math,
  FUN.VALUE = numeric(1)
) |>
  sum()
print(part1)

## Part 2
input <- readr::read_lines("day6/input.txt")
operators <- strsplit(input[[5]], "")[[1]]
operators <- operators[operators %in% c("*", "+")]

exprs2 <- input[-5] |>
  strsplit("") |>
  unlist() |>
  matrix(ncol = nchar(input[[1]]), byrow = TRUE) |>
  apply(2, paste, collapse = "") |>
  as.numeric()

part2 <- split(nums, cumsum(is.na(nums))) |>
  lapply(\(x) x[!is.na(x)]) |>
  purrr::map2(operators, \(x, y) {
    x[length(x) + 1] <- y
    return(x)
  }) |>
  vapply(cephalopod_math, FUN.VALUE = numeric(1)) |>
  sum()
print(part2)
