test_that("split_data returns train and test", {

  df <- data.frame(

    x1 = rnorm(100),

    x2 = rnorm(100),

    MilkYield305 = rnorm(100)

  )

  split <- split_data(

    data = df,

    target = "MilkYield305",

    train_size = 0.80,

    seed = 123

  )

  expect_true(is.list(split))

  expect_true("train" %in% names(split))

  expect_true("test" %in% names(split))

})
