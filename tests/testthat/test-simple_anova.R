test_that("Simple ANOVA table", {
  mod1 <- simple_anova(data_1w_death,  #grafify code
                       "Death", 
                       "Genotype")
  mod2 <- car::Anova(lm(Death ~ Genotype,  #base R code
                        data_1w_death))
  expect_equal(mod1$`Sum Sq`, mod2$`Sum Sq`)
  expect_equal(mod1$Df, mod2$Df)
  expect_equal(mod2$`Sum Sq`/mod2$Df, mod1$`Mean sq`)
  expect_equal(mod1$`F value`, mod2$`F value`)
})


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
