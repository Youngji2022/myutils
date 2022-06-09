test_that("x is a numeric vector", {
  expect_error(numeric_summary(c("a", "b", "c")),
               "x must be a numeric vector")
  expect_error(numeric_summary(c(T, F, F, T, NA)),
               "x must be a numeric vector")
})

test_that("numeric_summary produces expected output", {
  output_vector <-c(min=1.0, max=5.0, mean=3.0, sd=sd(1:5), length=5., Nmiss=0)
  expect_equal(numeric_summary(1:5), output_vector)


}
)
