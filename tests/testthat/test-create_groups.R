context("Checking Create Group function")

data <- data.frame("a" = "a", "b" = "b", "c" = "c", "A" = "A", "B" = "B", "C" = "C")
dictionary <- data.frame("A" = c("a", "A"), "B" = c("b", "B"), "C" = c("c", "C"))

test_that("Group Created according to dictionary", {
  expect_equal(create_group(dictionary, data), list(
    A = c("a" , "A"),
    B = c("b", "B"),
    C = c("c", "C"),
    unlisted = logical()
  ))
})

data_with_extra_columns <- data.frame("a" = "a", "b" = "b", "c" = "c", "A" = "A", "B" = "B", "C" = "C", "d" ="d", "D" = "D")

test_that("Data not in dictionary are going in unlisted", {
  expect_equal(create_group(dictionary, data_with_extra_columns), list(
    A = c("a" , "A"),
    B = c("b", "B"),
    C = c("c", "C"),
    unlisted = c("d", "D")
  ))
})
