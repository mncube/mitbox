test_that("modelFixedEff_x returns an object of class data.frame", {

  #The test is loosely based on the example presented in
  #?merTools::modelFixedEff

  #Generate Data
  df <- expand.grid(Subject=1:20,
                    Days=1:10,
                    Reaction = stats::rbinom(200, 1, 0.5))
  sims <- replicate(n = 10,
                    expr = df[sample(row.names(df), 100),],
                    simplify=FALSE)
  #Write formula
  formula <- "Reaction ~ Days + (1 | Subject)"

  #Run model
  mod <- merTools::glmerModList(formula,
                                data = sims,
                                family = binomial(link = "logit"),
                                nAGQ=0,
                                control = lme4::glmerControl(tolPwrss=1e-3))

  #Extract output
  output <- modelFixedEff_x(mod, exponentiate = FALSE)

  #Test
  expect_s3_class(output, "data.frame")
})
