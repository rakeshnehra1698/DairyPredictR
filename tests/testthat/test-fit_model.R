test_that("fit_model returns DairyPredictModel", {
  
  df <- data.frame(
    
    AnimalID = 1:50,
    
    TD1 = rnorm(50),
    
    TD2 = rnorm(50),
    
    TD3 = rnorm(50),
    
    MilkYield305 = rnorm(50)
    
  )
  
  model <- fit_model(
    data = df,
    target = "MilkYield305",
    predictors = c(
      "TD1",
      "TD2",
      "TD3"
    ),
    identifier = "AnimalID",
    method = "linear"
  )
  
  expect_true(
    inherits(model, "DairyPredictModel")
  )
  
})