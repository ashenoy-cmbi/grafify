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

