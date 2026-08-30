test_that("clean_data returns a data.frame", {

  df <- data.frame(

    A = c(1, 2, NA),

    B = c(4, 5, 6)

  )

  result <- clean_data(df)

  expect_true(is.data.frame(result))

})
