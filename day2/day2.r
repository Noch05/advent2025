input <- readr::read_lines(here("day2/input.txt")) |>
  strsplit(",") |>
  unlist() |>
  strsplit("-") |>
  lapply(\(x) as.numeric(x))

# Each Item is a Range of Numbers
# Numbers made up of repeated numbers are invalid, others are valid
# 1010 Invalid, 66 Invalid etc.
# 101 Valid, 4374 valid, etc.

# Part 1

is_invalid <- function(nums, part1) {
  chars <- as.character(nums)
  n <- nchar(chars)

  even <- n %% 2 == 0
  if (!any(even)) {
    return(integer())
  }

  chars <- chars[even]
  nums <- nums[even]
  n <- n[even]
  half <- n / 2

  first <- substr(chars, 1, half)
  second <- substr(chars, half + 1, n)
  return(nums[first == second])
}

#-----------------------------
# Part 2:
is_invalid2 <- function(x) {
  s <- as.character(x)
  stringi::stri_detect_regex(s, "^(.+?)\\1+$")
}
invalid_vec <- function(x, y) {
  vec <- x:y
  return(vec[vapply(vec, is_invalid2, FUN.VALUE = logical(1))])
}

invalid_ids1 <- integer()
invalid_ids2 <- integer()
for (pair in input) {
  invalid_ids1 <- c(invalid_ids1, is_invalid(pair[1]:pair[2], TRUE))
  invalid_ids2 <- c(invalid_ids2, invalid_vec(pair[1], pair[2]))
}

part1 <- sum(invalid_ids1)
part2 <- sum(invalid_ids2)
print(c(part1, part2))
