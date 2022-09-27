test_that("Check that lmerModList_lt return a p-value when tidied with
          broom.mixed::tidy", {
  #Generate Data
  df <- expand.grid(Subject=1:20,
                    Days=1:10,
                    Sleep = stats::rnorm(200, 6, 3))
  sims <- replicate(n = 10,
                    expr = df[sample(row.names(df), 100),],
                    simplify=FALSE)
  #Write formula
  formula <- "Sleep ~ Days + (1 | Subject)"

  #Run model
  mod <- lmerModList_lt(formula, data = sims)

  #Get output for first model
  mod_1 <- broom.mixed::tidy(mod[[1]])

  expect_equal(names(mod_1)[[length(mod_1)]], "p.value")
})
