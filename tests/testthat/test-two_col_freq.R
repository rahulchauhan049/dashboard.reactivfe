context("Two Column Frequency Function Working")

data <- data.frame("name"=c("a", "a", "a", "b", "b", "c"), "year"=c(1,2,1,3,4,1))


test_that("Working with test data", {
  expect_equal(find_two_column_frequency(data, "name", "year"),
               data.frame(
                 "x" = c("a", "c", "a", "b", "b"),
                 "y" = c("1", "1", "2", "3", "4"),
                 "Freq" = c(2, 1, 1, 1, 1),
                 stringsAsFactors = TRUE
                )
              )
})
