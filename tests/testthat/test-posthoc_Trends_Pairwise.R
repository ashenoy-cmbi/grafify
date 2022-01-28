test_that("Check post-hoc Trends comparisons", {
  #make a mixed effects model with grafify
  mmod5 <- mixed_model(data_2w_Tdeath,         #grafify model
                       "PI",
                       c("Genotype", "Time2"),
                       "Experiment")
  #fit same model with lmer call
  mmod6 <- lmerTest::lmer(PI ~ Genotype*Time2 + 
                            (1|Experiment),          #native call
                          data = data_2w_Tdeath)
  
  #compare posthoc_ tests from grafify with native emmeans call on grafify model 
  em1 <- posthoc_Trends_Pairwise(mmod5, "Genotype", "Time2")
  em2 <- emtrends(mmod5, pairwise ~ Genotype, var = "Time2")
  expect_equal(em1$contrasts@grid, em2$contrasts@grid)
  expect_equal(em1$emtrends@levels, em2$emtrends@levels)
  
  em3 <- posthoc_Trends_Levelwise(mmod5, c("Genotype", "Time2"), "Time2")
  em4 <- emtrends(mmod5, pairwise ~ Genotype|Time2, var = "Time2")
  expect_equal(em3$contrasts@grid, em4$contrasts@grid)
  expect_equal(em3$emtrends@levels, em4$emtrends@levels)
  
  em5 <- posthoc_Trends_vsRef(mmod5, "Genotype", "Time2")
  em6 <- emtrends(mmod5, trt.vs.ctrl ~ Genotype, var = "Time2")
  expect_equal(em5$contrasts@grid, em6$contrasts@grid)
  expect_equal(em5$emtrends@levels, em6$emtrends@levels)
  
  })
  