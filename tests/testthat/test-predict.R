test_that("predict returns numeric vector", {
  
  df <- data.frame(
    
    x1 = rnorm(50),
    
    x2 = rnorm(50),
    
    MilkYield305 = rnorm(50)
    
  )
  
  model <- fit_model(
    data = df,
    target = "MilkYield305",
    predictors = c(
      "x1",
      "x2"
    ),
    method = "linear"
  )
  
  pred <- predict(
    model,
    df
  )
  
  expect_true(is.numeric(pred))
  
  expect_equal(length(pred), nrow(df))
  
})