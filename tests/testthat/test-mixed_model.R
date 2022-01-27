test_that("Mixed effects models", {
  mmod5 <- mixed_model(data_2w_Festing,         #grafify model
                       "GST",
                       c("Treatment", "Strain"),
                       "Block")
  mmod6 <- lmer(GST ~ Treatment*Strain + 
                  (1|Block),          #native call
                data = data_2w_Festing)
  mmod6 <- lmerTest::as_lmerModLmerTest(mmod6)
  expect_equal(mmod5@call$formula[2], mmod6@call$formula[2]) #compare models
  expect_equal(mmod5@call$data , mmod6@call$data)
  expect_equal(mmod5@sigma, mmod6@sigma)
  expect_equal(mmod5@frame[[1]], mmod6@frame[[1]])
  expect_equal(mmod5@beta, mmod6@beta)
  #expect_equal(mmod5@call, mmod6@call) #only difference is lmerTest:: in native call
})

test_that("plot model residuals qqmodel", {
  mmod7 <- mixed_model(data_2w_Festing,         #grafify model
                       "GST",
                       c("Treatment", "Strain"),
                       "Block")
  p <- plot_qqmodel(mmod7)
  #test key layers and data file
  expect_s3_class(p, "gg")
  })
