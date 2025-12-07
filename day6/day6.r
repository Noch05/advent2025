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
operators <- input[[5]] |> strsplit("")
operators <- operators[[1]][operators[[1]] %in% c("*", "+")]
exprs2 <- input[-5] |>
  strsplit("") |>
  unlist() |>
  matrix(nrow = nchar(input[[1]]), byrow = TRUE) |>
  apply(2, paste, collapse = "")


exalign_num <- function(x, align) {
  n <- max(nchar(x))
  x <- vapply(x, FUN.VALUE = character(1), \(num) {
    pad <- n - nchar(num)
    padding <- paste(rep(" ", pad), collapse = "")
    paste0(padding, num)
  })
  names(x) <- NULL
  return(x)
}
col_num <- function(x) {
  x <- x[x != " "]
  num <- 0
  for (i in seq_along(x)) {
    num <- num + (as.numeric(x[i]) * 10^(length(x) - i))
  }
  return(num)
}
transpose <- function(x) {
  op <- x[length(x)]
  x_list <- align_num(x[-(length(x))]) |> strsplit("")
  x_t <- do.call(rbind, x_list) |> apply(2, col_num)
  return(c(x_t, op))
}
