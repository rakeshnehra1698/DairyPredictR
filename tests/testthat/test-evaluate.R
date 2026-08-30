test_that("evaluate returns performance table", {
  
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
  
  result <- evaluate(
    
    model,
    
    df,
    
    verbose = FALSE
    
  )
  
  expect_true(is.data.frame(result))
  
  expect_true("Metric" %in% names(result))
  
  expect_true("Value" %in% names(result))
  
})