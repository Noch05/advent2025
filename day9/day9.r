# Prepping and Splitting Coordinates
input <- readr::read_csv("day9/input.txt", col_names = FALSE)

## Part 1

x_mat <- abs(outer(input$X1, input$X1, `-`)) + 1
y_mat <- abs(outer(input$X2, input$X2, `-`)) + 1
area_mat <- x_mat * y_mat
part1 <- max(area_mat)

## Part 2
