options(scipen = 999)
## Gathering and Splitting Inputs
input <- readr::read_lines(here::here("day5/input.txt"))
split_idx <- which(input == "")
ranges <- input[1:(split_idx - 1)] |>
  strsplit("-") |>
  lapply(as.numeric)
to_test <- input[(split_idx + 1):length(input)] |> as.numeric()

## Part 1
part1 <- vapply(
  to_test,
  \(x) {
    for (range in ranges) {
      if (x >= range[1] && x <= range[2]) {
        return(TRUE)
        break
      }
    }
    return(FALSE)
  },
  FUN.VALUE = logical(1)
) |>
  sum() |>
  print()

# Part 2
## Sorting Ranes by Lower Bound
ranges <- ranges[order(vapply(ranges, \(x) x[1], FUN.VALUE = numeric(1)))]

## Initializing
max <- 0
total_fresh <- 0
for (range in ranges) {
  if (range[2] > max) {
    if (range[1] > max) {
      count <- range[2] - range[1] + 1
    } else {
      count <- range[2] - max
    }

    total_fresh <- total_fresh + count
    max <- range[2]
  }
}
print(total_fresh)
