test_that("load survey data into a dataframe", {
  data <- read_survey('data.csv')
  expect_equal(nrow(data), 29)
  expect_equal(ncol(data), 1101)
})
