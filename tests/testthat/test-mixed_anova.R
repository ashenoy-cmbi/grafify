test_that("Mixed effects ANOVA table defaults", {
  mmod1 <- mixed_anova(data_2w_Festing,        #fit with grafify call
                       "GST",
                       c("Treatment", "Strain"),
                       "Block")
  mmod2 <- anova(lmerTest::lmer(GST ~ Treatment*Strain + #fit with native call
                                  (1|Block),
                          data = data_2w_Festing),
                 type = "II",                 #default in grafify
                 ddf = "Kenward-Roger")       #default in grafify
  expect_equal(mmod1$`Sum Sq`, mmod2$`Sum Sq`)    #check some cells in ANOVA table
  expect_equal(mmod1$`F value`, mmod2$`F value`)
  expect_equal(mmod1$`Pr(>F)`[1], mmod2$`Pr(>F)`[1])
})

test_that("Mixed effects ANOVA table custom", {
  mmod3 <- mixed_anova(data_2w_Festing,            #grafify call
                       "GST",
                       c("Treatment", "Strain"),
                       "Block",
                       Df_method = "Satterthwaite") #change ddf method
  mmod4 <- anova(lmerTest::lmer(GST ~ Treatment*Strain +  #native call
                                  (1|Block),
                                data = data_2w_Festing),
                 type = "II",
                 ddf = "Satterthwaite")        #change ddf method
  expect_equal(mmod3$`Sum Sq`, mmod4$`Sum Sq`) #check cells from ANOVA table
  expect_equal(mmod3$`F value`, mmod4$`F value`)
  expect_equal(mmod3$`Pr(>F)`[1], mmod4$`Pr(>F)`[1])
  expect_equal(class(mmod3), class(mmod4))
})

test_that("Mixed effects models default", {
  mmod5 <- mixed_model(data_2w_Festing,         #grafify model
                       "GST",
                       c("Treatment", "Strain"),
                       "Block")
  mmod6 <- lmerTest::lmer(GST ~ Treatment*Strain + 
                            (1|Block),          #native call
                                data = data_2w_Festing)
  expect_equal(mmod5@call$formula[2], mmod6@call$formula[2]) #compare models
  expect_equal(mmod5@call$data , mmod6@call$data)
  expect_equal(mmod5@sigma, mmod6@sigma)
  expect_equal(mmod5@frame[[1]], mmod6@frame[[1]])
  expect_equal(mmod5@beta, mmod6@beta)
})
