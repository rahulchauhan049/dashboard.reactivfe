context("Checking Find Field Functions")

data <- data.frame("a" = c("A", "B", "C"), "b" = c("A", "B", "C"),  "c" = c(1,2,3), "d" = c(1))

test_that("When Input is Bubble", {
  expect_equal(find_fields("bubble", data), list(
    x = c("a", "b"),
    y = c("c", "d")
  ))
})


test_that("When Input is Line", {
  expect_equal(find_fields("line", data), list(
    x = c("a", "b"),
    y = c("c", "d")
  ))
})


test_that("When Input is pie", {
  expect_equal(find_fields("pie", data), list(
    x = c("a", "b")
  ))
})

test_that("When Input is bar", {
  expect_equal(find_fields("bar", data), list(
    x = c("a", "b")
  ))
})