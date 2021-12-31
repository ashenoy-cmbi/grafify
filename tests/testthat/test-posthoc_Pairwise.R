test_that("Check post-hoc pairwise comparisonss", {
    #make a mixed effects model with grafify
    mmod5 <- mixed_model(data_2w_Festing,         #grafify model
                         "GST",
                         c("Treatment", "Strain"),
                         "Block")
    #fit same model with lmer call
    mmod6 <- lmerTest::lmer(GST ~ Treatment*Strain + 
                              (1|Block),          #native call
                            data = data_2w_Festing)
    
    #compare posthoc_ tests from grafify with native emmeans call on grafify model 
    em1 <- posthoc_Pairwise(mmod5, c("Treatment", "Strain"))
    em2 <- emmeans(mmod5, pairwise ~Treatment*Strain, adjust = "fdr")
    expect_equal(em1$contrasts@grid, em2$contrasts@grid)
    expect_equal(em1$emmeans@misc$sigma, em2$emmeans@misc$sigma)
    expect_equal(em1$emmeans@levels$Treatment, em2$emmeans@levels$Treatment)
    expect_equal(em1$emmeans@V[[1]], em2$emmeans@V[[1]])
    expect_equal(em1$contrasts@levels$contrast, em2$contrasts@levels$contrast)
    expect_equal(em1$contrasts@misc$adjust, em2$contrasts@misc$adjust)
    expect_equal(em1$emmeans@grid[,3], em2$emmeans@grid[,3])
    expect_equal(em1$contrasts@model.info$call, em2$contrasts@model.info$call)
    
    #compare posthoc_ tests from grafify with native emmeans call on native model
    em3 <- posthoc_Pairwise(mmod6, c("Treatment", "Strain"))
    em4 <- emmeans(mmod6, pairwise ~Treatment*Strain, adjust = "fdr")
    expect_equal(em3$contrasts@grid, em4$contrasts@grid)
    expect_equal(em3$emmeans@misc$sigma, em4$emmeans@misc$sigma)
    expect_equal(em3$emmeans@levels$Treatment, em4$emmeans@levels$Treatment)
    expect_equal(em3$emmeans@V[[1]], em4$emmeans@V[[1]])
    expect_equal(em3$contrasts@levels$contrast, em4$contrasts@levels$contrast)
    expect_equal(em3$contrasts@misc$adjust, em4$contrasts@misc$adjust)
    expect_equal(em3$contrasts@misc$orig.grid[,2], em4$contrasts@misc$orig.grid[,2])
    expect_equal(em3$contrasts@model.info$call, em4$contrasts@model.info$call)
})
  