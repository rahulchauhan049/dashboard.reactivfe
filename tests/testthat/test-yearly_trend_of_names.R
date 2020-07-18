context("Yearly Trend of Names")

data <- data.frame(
  "name"= c("a", "a", "b", "b", "c", "c", "d"),
  "year"=c(1,2,2,1,2,2,2)
)

test_that("function works", {
  expect_equal(yearly_trend_of_names(data, "name", "year"), list(
    a = data.frame("Var1"=as.factor(c("1", "2")), "Freq"=c(1, 1), "cumsum"=c(1,2)),
    b = data.frame("Var1"=as.factor(c("1", "2")), "Freq"=c(1, 1), "cumsum"=c(1,2)),
    c = data.frame("Var1"=as.factor(c("2")), "Freq"=c(2), "cumsum"=c(2)),
    d = data.frame("Var1"=as.factor(c("2")), "Freq"=c(1), "cumsum"=c(1))
  ))
})

