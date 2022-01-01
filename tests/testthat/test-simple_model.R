test_that("Simple linear model", {
  mod3 <- simple_model(data = data_1w_death,  #grafify code
                       "Death", 
                       "Genotype")
  mod4 <- lm(Death ~ Genotype,  #base R code
             data_1w_death)
  expect_equal(class(mod3), class(mod4))
  expect_identical(mod3$call$formula[2], mod4$call$formula[2])
  expect_equal(mod3$coefficients, mod4$coefficients)
  expect_equal(mod3$residuals, mod4$residuals)
  expect_equal(mod3$terms[[3]], mod4$terms[[3]])
  expect_equal(mod3$call, mod4$call)
  expect_equal(mod3$call$formula, mod4$call$formula)
})